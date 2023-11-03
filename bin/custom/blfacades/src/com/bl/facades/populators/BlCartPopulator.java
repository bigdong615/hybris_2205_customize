package com.bl.facades.populators;

import atg.taglib.json.util.JSONArray;
import atg.taglib.json.util.JSONException;
import atg.taglib.json.util.JSONObject;
import com.bl.core.dao.promotion.BlPromotionDao;
import com.bl.core.enums.ProductTypeEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.facades.giftcard.data.BLGiftCardData;
import com.bl.logging.BlLogger;
import com.google.gson.Gson;
import de.hybris.platform.commercefacades.order.converters.populator.CartPopulator;
import de.hybris.platform.commercefacades.order.data.AbstractOrderData;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.user.CustomerModel;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

import de.hybris.platform.promotionengineservices.model.PromotionSourceRuleModel;
import de.hybris.platform.promotions.model.PromotionResultModel;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import javax.annotation.Resource;


/**
 * Extended OOTB CartPopulator to populate custom attributes
 *
 * @author Ravikumar
 *
 */
public class BlCartPopulator extends CartPopulator<CartData>
{
	private static final Logger LOG = Logger.getLogger(BlCartPopulator.class);

	@Resource(name = "promotionDao")
	private BlPromotionDao promotionDao;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void populate(final CartModel source, final CartData target)
	{
		super.populate(source, target);
		target.setTotalDamageWaiverCost(createPrice(source, source.getTotalDamageWaiverCost()));
		target.setTotalOptionsCost(createPrice(source, source.getTotalOptionsCost()));
		target.setPickUpPersonFirstName(source.getPickUpPersonFirstName());
		target.setPickUpPersonLastName(source.getPickUpPersonLastName());
		target.setPickUpPersonEmail(source.getPickUpPersonEmail());
		target.setPickUpPersonPhone(source.getPickUpPersonPhone());
		target.setAvalaraCalculated(source.getAvalaraTaxCalculated());
		target.setTaxAvalaraCalculated(createPrice(source , source.getTotalTax()));
		target.setIsRentalCart(source.getIsRentalOrder());
		target.setPoNumber(source.getPoNumber());
		target.setPoNotes(source.getPoNotes());
		if(source.getUser() instanceof CustomerModel){
			final CustomerModel customerModel = (CustomerModel)source.getUser();
			target.setIsPOEnabled(customerModel.isPoEnabled());
		}
		if (CollectionUtils.isNotEmpty(source.getOrderNotes())){
			target.setOrderNotes(source.getOrderNotes().get(0).getNote());
		}
		final PriceDataType priceType = PriceDataType.BUY;
		target.setHasGiftCart(Boolean.valueOf(false));
		if(CollectionUtils.isNotEmpty(source.getEntries())){
			final Optional<AbstractOrderEntryModel> giftCardEntry = source.getEntries().stream().findFirst();
			if(giftCardEntry.isPresent() && source.getGiftCardCost() != null 
					&& ProductTypeEnum.GIFTCARD.equals(((BlProductModel)giftCardEntry.get().getProduct()).getProductType())){
				target.setGiftCardCost(source.getGiftCardCost());
				target.setHasGiftCart(Boolean.valueOf(true));
				target.setIsRentalCart(Boolean.valueOf(false));
			}
			
		}
		
		if (source.getTotalPrice() != null && source.getGiftCardAmount() != null)
		{
			final PriceData grandTotal = getPriceDataFactory().create(priceType, BigDecimal.valueOf(source.getGrandTotal()),
					source.getCurrency() != null ? source.getCurrency().getIsocode() : "");
			if (grandTotal != null)
			{
				target.setGrandTotal(grandTotal);
			}
		}

		// BL-657 to add total discount with gift cart discount to display on order summary section
		 Double totalPromotionDiscount = 0.0;
		 Double totalGiftCardDiscount = 0.0;
		if(null != source.getTotalDiscounts()){
			totalPromotionDiscount = source.getTotalDiscounts();
		}
		if(null != source.getGiftCardAmount()){
			totalGiftCardDiscount = source.getGiftCardAmount();
		}
			final Double totalDiscount = totalPromotionDiscount + totalGiftCardDiscount;
			target.setTotalDiscounts(createPrice(source , totalDiscount));


		if (CollectionUtils.isNotEmpty(source.getGiftCard()))
		{
			final List<BLGiftCardData> blGiftCardDataList = new ArrayList<>();
			for (final GiftCardModel giftCardModel : source.getGiftCard())
			{
				final BLGiftCardData blGiftCardData = new BLGiftCardData();
				blGiftCardData.setCode(giftCardModel.getCode());
				final List<GiftCardMovementModel> giftCardMovementModelList = giftCardModel.getMovements();
				//rounding off double value to 2 decimal places
				BigDecimal gcRedeemedAmount = BigDecimal.valueOf(giftCardMovementModelList.get(giftCardMovementModelList.size()-1).getAmount()).setScale(2, RoundingMode.HALF_DOWN);
				blGiftCardData.setRedeemamount(createPrice(source , gcRedeemedAmount.doubleValue()));
				blGiftCardData.setBalanceamount(createPrice(source , giftCardModel.getBalance()));
				blGiftCardDataList.add(blGiftCardData);
			}
			target.setGiftCardData(blGiftCardDataList);
		}
		target.setIsRetailGearOrder(BooleanUtils.isTrue(source.getIsRetailGearOrder()));

		Set<PromotionResultModel> results = source.getAllPromotionResults();
		for (PromotionResultModel result : results) {
			String code = result.getPromotion().getCode();
			Optional<PromotionSourceRuleModel> promotion = getPromotionDao().getPromotionByCode(code);
			if (promotion.isPresent()) {
				String jsonString = promotion.get().getActions();
				try {
					JSONArray jsonArray = new JSONArray(jsonString);
					JSONArray freeRenatalDates = findFreeRentalDates(jsonArray);
					if (freeRenatalDates != null) {
						List<String> dates = convertToFormattedDates(freeRenatalDates);
						target.setFreeRentalDates(dates);
					}
				} catch (Exception e) {
					BlLogger.logMessage(LOG, Level.ERROR, "Error occurred while getting the free rental dates", e);
				}
			}
		}

	}

	/**
	 * Overriding to remove discounts from subtotal
	 * @param source abstractOrderModel
	 * @param target target object
	 */
	@Override
	protected void addTotals(final AbstractOrderModel source, final AbstractOrderData target)
	{
		super.addTotals(source,target);
		final double subTotal = source.getSubtotal().doubleValue();
		final PriceData subTotalPriceData = createPrice(source, Double.valueOf(subTotal));
		target.setSubTotal(subTotalPriceData);
	}

	/**
	 * This method overridden to calculate the totalPrice with tax
	 */
	@Override
  protected Double calcTotalWithTax(final AbstractOrderModel source)
  {
    if (source == null)
    {
      BlLogger.logMessage(LOG , Level.ERROR , "source order must not be null" , new IllegalArgumentException());
    }
    // Since we have already calculated the total with Tax , so returning cart total as total price with tax
    return null != source && source.getTotalPrice() != null ? source.getTotalPrice() : 0.0d;
  }

	// Helper method to find the free rental dates array in the JSON
	private JSONArray findFreeRentalDates(JSONArray jsonArray) throws JSONException {
		JSONArray freeRenatalDates = null;
		for (int i = 0; i < jsonArray.length(); i++) {
			JSONObject obj = jsonArray.getJSONObject(i);
			if (obj.getJSONObject("parameters").has("freeRentalDates")) {
				freeRenatalDates = obj.getJSONObject("parameters").getJSONObject("freeRentalDates").getJSONArray("value");
				break;  // Exit the loop as we've found the dates
			}
		}
		return freeRenatalDates;
	}

	// Helper method to convert date strings to the desired format
	private List<String> convertToFormattedDates(JSONArray freeRentalDates) throws JSONException, ParseException {
		List<String> dates = new ArrayList<>();
		SimpleDateFormat sourceFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ");
		SimpleDateFormat targetFormat = new SimpleDateFormat("MM/dd");

		for (int i = 0; i < freeRentalDates.length(); i++) {
			String dateString = freeRentalDates.getString(i);
			Date freeDate = sourceFormat.parse(dateString);
			String formattedDate = targetFormat.format(freeDate);
			dates.add(formattedDate);
		}
		return dates;
	}


	public BlPromotionDao getPromotionDao() {
		return promotionDao;
	}

	public void setPromotionDao(BlPromotionDao promotionDao) {
		this.promotionDao = promotionDao;
	}
}
