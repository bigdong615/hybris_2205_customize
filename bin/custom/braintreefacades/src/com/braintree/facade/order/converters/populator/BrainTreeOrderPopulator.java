/**
 *
 */
package com.braintree.facade.order.converters.populator;

import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.facades.giftcard.data.BLGiftCardData;
import de.hybris.platform.commercefacades.order.converters.populator.OrderPopulator;
import de.hybris.platform.commercefacades.order.data.AbstractOrderData;
import de.hybris.platform.commercefacades.order.data.CCPaymentInfoData;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.order.payment.CreditCardPaymentInfoModel;
import de.hybris.platform.core.model.order.payment.PaymentInfoModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.product.data.RentalDateDto;
import com.braintree.model.BrainTreePaymentInfoModel;
import org.apache.commons.collections.CollectionUtils;


public class BrainTreeOrderPopulator extends OrderPopulator
{
	private Converter<BrainTreePaymentInfoModel, CCPaymentInfoData> brainTreePaymentInfoConverter;
	
	@Override
  public void populate(final OrderModel source, final OrderData target)
  {
	  super.populate(source, target);
	  target.setRentalDates(getOrderRentalDates(source));
		setGiftCardDetails(source, target);
	}

	/**
	 *  Get gift card from OrderModel and set gift card details to OrderData.
	 * @param source
	 * @param target
	 */
	private void setGiftCardDetails(final OrderModel source, final OrderData target) {
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
	}

	/**
   * Gets the order rental date in formated date (MMM dd format).
   * 
   * example : JUN 25
   *
   * @param source the source
   * @return the order rental dates
   */
  private RentalDateDto getOrderRentalDates(final OrderModel source)
  {
    final RentalDateDto orderRentalDate = new RentalDateDto();
    if (Objects.nonNull(source.getRentalStartDate()) && Objects.nonNull(source.getRentalEndDate()))
    {
      orderRentalDate.setSelectedFromDate(BlDateTimeUtils.convertDateToStringDate(source.getRentalStartDate(), BlCoreConstants.RENTAL_DATE_FORMAT));
      orderRentalDate.setSelectedToDate(BlDateTimeUtils.convertDateToStringDate(source.getRentalEndDate(), BlCoreConstants.RENTAL_DATE_FORMAT));
      orderRentalDate
          .setNumberOfDays(String.valueOf(BlDateTimeUtils.getDaysBetweenDates(source.getRentalStartDate(), source.getRentalEndDate()) + 1));
    }
    return orderRentalDate;
  }
	
	@Override
	protected void addPaymentInformation(final AbstractOrderModel source, final AbstractOrderData prototype)
	{
		final PaymentInfoModel paymentInfo = source.getPaymentInfo();
		if (paymentInfo instanceof CreditCardPaymentInfoModel)
		{
			final CCPaymentInfoData paymentInfoData = getCreditCardPaymentInfoConverter()
					.convert((CreditCardPaymentInfoModel) paymentInfo);
			prototype.setPaymentInfo(paymentInfoData);
		}
		else if (paymentInfo instanceof BrainTreePaymentInfoModel)
		{
			final CCPaymentInfoData paymentInfoData = getBrainTreePaymentInfoConverter()
					.convert((BrainTreePaymentInfoModel) paymentInfo);
			prototype.setPaymentInfo(paymentInfoData);
		}
	}

	/**
	 * @return the brainTreePaymentInfoConverter
	 */
	public Converter<BrainTreePaymentInfoModel, CCPaymentInfoData> getBrainTreePaymentInfoConverter()
	{
		return brainTreePaymentInfoConverter;
	}

	/**
	 * @param brainTreePaymentInfoConverter
	 *           the brainTreePaymentInfoConverter to set
	 */
	public void setBrainTreePaymentInfoConverter(
			final Converter<BrainTreePaymentInfoModel, CCPaymentInfoData> brainTreePaymentInfoConverter)
	{
		this.brainTreePaymentInfoConverter = brainTreePaymentInfoConverter;
	}


}
