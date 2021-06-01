package com.bl.facades.populators;

import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.facades.giftcard.data.BLGiftCardData;
import com.bl.logging.BlLogger;
import de.hybris.platform.commercefacades.order.converters.populator.CartPopulator;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartModel;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.List;
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

		final double rawSubTotal = source.getSubtotal() != null ? source.getSubtotal().doubleValue() : 0.0d;
		final PriceData subTotalRawPriceData = createPrice(source, Double.valueOf(rawSubTotal));
		//        if (subTotalRawPriceData != null) {
		//            target.setRawSubTotal(subTotalRawPriceData);
		//        }

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

		//        if (source.getGiftCardCode() != null) {
		//            target.setGiftCode(source.getGiftCardCode());
		//        }

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

		//        if (source.getDiscountDelivery() != null) {
		//
		//            final PriceData discountDelivery = getPriceDataFactory().create(priceType,
		//                    BigDecimal.valueOf(source.getDiscountDelivery().doubleValue()), source.getCurrency().getIsocode());
		//            if (discountDelivery != null) {
		//                target.setDiscountDelivery(discountDelivery);
		//            }
		//        }

		//        if (source.getTotalDeliveryWithFreeShipping() != null) {
		//
		//            final PriceData totalDeliveryWithFreeShipping = getPriceDataFactory().create(priceType,
		//                    BigDecimal.valueOf(source.getTotalDeliveryWithFreeShipping().doubleValue()), source.getCurrency().getIsocode());
		//            if (totalDeliveryWithFreeShipping != null) {
		//                target.setTotalDeliveryWithFreeShipping(totalDeliveryWithFreeShipping);
		//            }
		//        }
		//
		//        if (source.getEntries() != null && !source.getEntries().isEmpty()) {
		//
		//            target.setEntries(groupEntries(target.getEntries()));
		//
		//
		//        }
		//        if (source.getDeliveryCost() != null) {
		//            final PriceData totalDeliveryCost = getPriceDataFactory().create(priceType,
		//                    BigDecimal.valueOf(source.getDeliveryCost().doubleValue()), source.getCurrency().getIsocode());
		//            if (totalDeliveryCost != null)
		//            {
		//                target.setDeliveryCost(totalDeliveryCost);
		//            }
		//        }
		if (source.getGiftCard() != null && !source.getGiftCard().isEmpty())
		{
			final List<BLGiftCardData> blGiftCardDataList = new ArrayList<>();

			for (final GiftCardModel giftCardModel : source.getGiftCard())
			{
				final BLGiftCardData blGiftCardData = new BLGiftCardData();
				blGiftCardData.setCode(giftCardModel.getCode());
				final List<GiftCardMovementModel> giftCardMovementModelList = giftCardModel.getMovements();
				//rounding off double value to 2 decimal places
				BigDecimal gcRedeemedAmount = new BigDecimal(giftCardMovementModelList.get(giftCardMovementModelList.size()-1).getAmount()).setScale(2, RoundingMode.HALF_DOWN);
				blGiftCardData.setRedeemamount(gcRedeemedAmount.doubleValue());
				blGiftCardData.setBalanceamount(giftCardModel.getBalance());
				blGiftCardDataList.add(blGiftCardData);
			}

			target.setGiftCardData(blGiftCardDataList);

		}
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
}
