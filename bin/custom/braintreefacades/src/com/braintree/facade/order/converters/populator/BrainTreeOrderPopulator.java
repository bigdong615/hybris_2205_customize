/**
 *
 */
package com.braintree.facade.order.converters.populator;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.NotesEnum;
import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.core.model.NotesModel;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.giftcard.data.BLGiftCardData;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.logging.BlLogger;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.google.common.collect.Lists;

import de.hybris.platform.commercefacades.order.converters.populator.OrderPopulator;
import de.hybris.platform.commercefacades.order.data.AbstractOrderData;
import de.hybris.platform.commercefacades.order.data.CCPaymentInfoData;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.order.payment.CreditCardPaymentInfoModel;
import de.hybris.platform.core.model.order.payment.PaymentInfoModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


public class BrainTreeOrderPopulator extends OrderPopulator
{
	private static final Logger LOGGER = Logger.getLogger(BrainTreeOrderPopulator.class);

	private Converter<BrainTreePaymentInfoModel, CCPaymentInfoData> brainTreePaymentInfoConverter;
	private static final int ROUND_OFF_SCALE = 2;
	@Override
	public void populate(final OrderModel source, final OrderData target) {
		super.populate(source, target);
		target.setRentalDates(getOrderRentalDates(source));
		setGiftCardDetails(source, target);
		target.setIsRentalCart(BooleanUtils.toBoolean(source.getIsRentalOrder()));
		target.setTotalDamageWaiverCost(createPrice(source, source.getTotalDamageWaiverCost()));
		target.setPoNotes(source.getPoNotes());
		target.setRentalDatesOnPrint(
				setFormattedRentalDates(source.getRentalStartDate(), source.getRentalEndDate()));
		if (CollectionUtils.isNotEmpty(source.getOrderNotes())) {
			final Optional<NotesModel> notesModel = source.getOrderNotes().stream()
					.filter(orderNote -> orderNote.getType().equals(NotesEnum.CUSTOMER_CHECKOUT_ORDER_NOTES))
					.findFirst();
			notesModel.ifPresent(model -> target.setOrderNotes(model.getNote()));
		}
		final double promoDiscount = Objects.isNull(source.getTotalDiscounts()) ? 0.0 : source.getTotalDiscounts();
		final double gifCardDiscount = Objects.isNull(source.getGiftCardAmount()) ? 0.0 : source.getGiftCardAmount();
		target.setTotalDiscounts(createPrice(source, promoDiscount + gifCardDiscount));
		populateModifiedOrderPaymentInfos(source, target);
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
				if(!source.isGiftCardOrder()){
					final List<GiftCardMovementModel> giftCardMovementModelList = giftCardModel.getMovements();
					//rounding off double value to 2 decimal places
					BigDecimal gcRedeemedAmount = BigDecimal.valueOf(
							giftCardMovementModelList.get(giftCardMovementModelList.size() - 1).getAmount())
							.setScale(ROUND_OFF_SCALE, RoundingMode.HALF_DOWN);
					blGiftCardData.setRedeemamount(createPrice(source , gcRedeemedAmount.doubleValue()));
					blGiftCardData.setBalanceamount(createPrice(source , giftCardModel.getBalance()));
					blGiftCardDataList.add(blGiftCardData);
				}
				
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
		final CCPaymentInfoData paymentInfoData = getPaymentInfoData(paymentInfo);
		if(Objects.nonNull(paymentInfoData))
		{
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

	/**
	 * Sets the formatted rental dates on print order confirmation page.
	 * @param startDate
	 * @param endDate
	 * @return
	 */
	private RentalDateDto setFormattedRentalDates(final Date startDate, final Date endDate)
	{
		final RentalDateDto rentalDateDto = new RentalDateDto();
		if(Objects.nonNull(startDate) && Objects.nonNull(endDate))
		{
			final String formattedRentalStartDate = getFormattedDate(startDate);
			rentalDateDto.setSelectedFromDate(formattedRentalStartDate);
			final String formattedRentalEndDate = getFormattedDate(endDate);
			rentalDateDto.setSelectedToDate(formattedRentalEndDate);
		}
		return  rentalDateDto;
	}

	/**
	 * Gets the formatted date in EEEE, MMM d format.
	 * Example - Wednesday, Jan 31
	 *
	 * @param date the date
	 * @return the formatted date
	 */
	private String getFormattedDate(final Date date)
	{
		return BlDateTimeUtils.convertDateToStringDate(date, BlCoreConstants.REVIEW_PAGE_DATE_FORMAT);
	}

	/**
	 * Overriding to remove discounts from subtotal
	 *
	 * @param source abstractOrderModel
	 * @param target target object
	 */
	@Override
	protected void addTotals(final AbstractOrderModel source, final AbstractOrderData target) {
		super.addTotals(source, target);
		final double subTotal = source.getSubtotal();
		final PriceData subTotalPriceData = createPrice(source, subTotal);
		target.setSubTotal(subTotalPriceData);
	}

	/**
	 * This method overridden to calculate the totalPrice with tax
	 */
	@Override
	protected Double calcTotalWithTax(final AbstractOrderModel source) {
		if (Objects.isNull(source)) {
			BlLogger.logMessage(LOGGER, Level.ERROR, "source order must not be null",
					new IllegalArgumentException());
		}
		// Since we have already calculated the total with Tax , so returning cart total as total price with tax
		return Objects.nonNull(source) && Objects.nonNull(source.getTotalPrice()) ? source
				.getTotalPrice() : 0.0d;
	}
	
	/**
	 * Populate modified order payment infos.
	 *
	 * @param source the source
	 * @param target the target
	 */
	private void populateModifiedOrderPaymentInfos(final AbstractOrderModel source, final AbstractOrderData target)
	{
	  final List<CCPaymentInfoData> modifiedOrderPaymentInfos = Lists.newArrayList();
	  if(CollectionUtils.isNotEmpty(source.getPaymentTransactions()))
	  {
	    source.getPaymentTransactions().forEach(paymentTransaction -> {
	      final PaymentInfoModel paymentInfoModel = paymentTransaction.getInfo();
	      if (paymentInfoModel instanceof BrainTreePaymentInfoModel)
	      {
	        final BrainTreePaymentInfoModel paymentInfo = ((BrainTreePaymentInfoModel) paymentInfoModel);
	        final CCPaymentInfoData paymentInfoData = BooleanUtils.isTrue(paymentInfo.isModifyPayment()) ? getPaymentInfoData(paymentInfoModel) : null;
	        if(Objects.nonNull(paymentInfoData))
	        {
	          modifiedOrderPaymentInfos.add(paymentInfoData);
	        }
	      }	      
	    });
	  }
	  target.setModifiedOrderPaymentInfos(modifiedOrderPaymentInfos);
	  target.setModifiedOrderPoNumber(ObjectUtils.defaultIfNull(source.getModifiedOrderPoNumber(), StringUtils.EMPTY));
	  target.setModifiedOrderPoNotes(ObjectUtils.defaultIfNull(source.getModifiedOrderPoNotes(), StringUtils.EMPTY));
	}
	
	/**
	 * Gets the payment info data.
	 *
	 * @param paymentInfo the payment info
	 * @return the payment info data
	 */
	private CCPaymentInfoData getPaymentInfoData(final PaymentInfoModel paymentInfo)
	{
	  if (paymentInfo instanceof CreditCardPaymentInfoModel)
    {
	    return getCreditCardPaymentInfoConverter()
          .convert((CreditCardPaymentInfoModel) paymentInfo);
    }
    else if (paymentInfo instanceof BrainTreePaymentInfoModel)
    {
      return getBrainTreePaymentInfoConverter().convert((BrainTreePaymentInfoModel) paymentInfo);
    }
	  return null;
	}
}
