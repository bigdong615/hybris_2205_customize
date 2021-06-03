package com.bl.facades.populators;

import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.facades.giftcard.data.BLGiftCardData;
import com.bl.logging.BlLogger;
import de.hybris.platform.commercefacades.order.converters.populator.CartPopulator;
import de.hybris.platform.commercefacades.order.data.AbstractOrderData;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.promotionengineservices.model.RuleBasedOrderAdjustTotalActionModel;
import de.hybris.platform.promotions.model.PromotionResultModel;
import de.hybris.platform.promotions.result.PromotionOrderResults;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


/**
 * Extended OOTB CartPopulator to populate custom attributes
 *
 * @author Ravikumar
 *
 */
public class BlCartPopulator extends CartPopulator<CartData>
{
	private static final Logger LOG = Logger.getLogger(BlCartPopulator.class);

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void populate(final CartModel source, final CartData target)
	{
		super.populate(source, target);
		target.setTotalDamageWaiverCost(createPrice(source, source.getTotalDamageWaiverCost()));
		target.setPickUpPersonFirstName(source.getPickUpPersonFirstName());
		target.setPickUpPersonLastName(source.getPickUpPersonLastName());
		target.setPickUpPersonEmail(source.getPickUpPersonEmail());
		target.setPickUpPersonPhone(source.getPickUpPersonPhone());
		target.setAvalaraCalculated(source.getAvalaraTaxCalculated());
		target.setTaxAvalaraCalculated(createPrice(source , source.getTotalTax()));
		target.setIsRentalCart(source.getIsRentalCart());
		populatePromotionAmount(source,target);


		final PriceDataType priceType = PriceDataType.BUY;
		if (source.getTotalPrice() != null && source.getGiftCardAmount() != null)
		{
			final PriceData grandTotal = getPriceDataFactory().create(priceType, BigDecimal.valueOf(source.getGrandTotal()),
					source.getCurrency() != null ? source.getCurrency().getIsocode() : "");
			if (grandTotal != null)
			{
				target.setGrandTotal(grandTotal);
			}
		}
		if (source.getGiftCardAmount() != null)
		{
			final PriceData giftDiscount = getPriceDataFactory().create(priceType,
					BigDecimal.valueOf(source.getGiftCardAmount().doubleValue()),
					source.getCurrency() != null ? source.getCurrency().getIsocode() : "");
			if (giftDiscount != null)
			{
				target.setGiftCardDiscount(giftDiscount);
			}
		}

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
				blGiftCardData.setRedeemamount(gcRedeemedAmount.doubleValue());
				blGiftCardData.setBalanceamount(giftCardModel.getBalance());
				blGiftCardDataList.add(blGiftCardData);
			}
			target.setGiftCardData(blGiftCardDataList);
		}

		// Added for showing Promotion discount and Gift Card Discounts on Cart page and checkout page
		Double giftCartAmount = 0.0;
		if(null != source.getGiftCardAmount()) {
			giftCartAmount = source.getGiftCardAmount();
		}
		Double totalPromotion = giftCartAmount + source.getTotalDiscounts();

		target.setTotalDiscounts(createPrice(source ,totalPromotion));
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

	/**
   * This method created to add coupon price
	 */
  private void populatePromotionAmount(final CartModel source, final CartData target) {
		final Map<String, BigDecimal> amountMap = new HashMap<>();
		for(final PromotionResultModel promotionResultModel : source.getAllPromotionResults()){
			final RuleBasedOrderAdjustTotalActionModel ruleBasedOrderAdjustTotalActionModel = (RuleBasedOrderAdjustTotalActionModel) promotionResultModel.getActions().iterator().next();
			for(final String couponCode : ruleBasedOrderAdjustTotalActionModel.getUsedCouponCodes())
			{
				amountMap.put(couponCode , ruleBasedOrderAdjustTotalActionModel.getAmount().setScale(2 , RoundingMode.HALF_DOWN));
			}
		}
		target.setPromotionAmountMap(amountMap);
	}

}
