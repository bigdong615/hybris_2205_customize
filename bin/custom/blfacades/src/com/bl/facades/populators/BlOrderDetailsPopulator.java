package com.bl.facades.populators;

import com.bl.core.enums.ExtendOrderStatusEnum;
import com.bl.core.enums.NotesEnum;
import com.bl.core.model.BlPickUpZoneDeliveryModeModel;
import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.core.model.NotesModel;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.constants.BlFacadesConstants;
import com.bl.facades.giftcard.data.BLGiftCardData;
import com.bl.facades.product.data.ExtendOrderData;
import com.google.common.collect.Lists;

import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.product.PriceDataFactory;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateUtils;

import de.hybris.platform.core.model.c2l.CurrencyModel;

/**
 * This Populator created to populate order details
 * @author Manikandan
 */
public class BlOrderDetailsPopulator <SOURCE extends OrderModel, TARGET extends OrderData> implements
    Populator<SOURCE, TARGET> {

  private PriceDataFactory priceDataFactory;
  private BlAddressPopulator blAddressPopulator;

  /**
   * This method created to populate order details for custom attributes
   */
  @Override
  public void populate(final OrderModel source, final OrderData target) throws ConversionException {
    target.setIsRentalCart(source.getIsRentalOrder());
    populateDatesForOrderDetails(source , target);
    populatePriceDetails(source , target);
    if(!source.isGiftCardOrder()){	 
        populateOrderDetailsForRentalOrder(source , target);
    }
    //Check gift card purchase order
    if(source.isGiftCardOrder()){
   	 final Optional<AbstractOrderEntryModel> giftCardOrder = source.getEntries().stream().findFirst();
   	 if(giftCardOrder.isPresent()){
   		 
   		 target.setRecipientEmail(giftCardOrder.get().getRecipientEmail());
   		 target.setRecipientName(giftCardOrder.get().getRecipientName());
   	 }
   	 target.setIsRentalCart(Boolean.FALSE);
   	 target.setHasGiftCart(Boolean.TRUE);
    }

    if ((null != source.getReturnRequestForOrder()) && (source.getIsReplacementOrder().booleanValue())) {
      target.setIsReplacementOrder(true);
    }
    if(null != source.getReturnRequestForOrder()) {
      target.setReplacementFor(source.getReturnRequestForOrder().getOrder().getCode());
    }
    populateOrderNotes(source , target);
    if(null == target.getDeliveryAddress() && source.getDeliveryMode() instanceof BlPickUpZoneDeliveryModeModel) {
      final AddressData addressData = new AddressData();
      BlPickUpZoneDeliveryModeModel blPickUpZoneDeliveryModeModel = (BlPickUpZoneDeliveryModeModel) source.getDeliveryMode();
      getBlAddressPopulator().populate(blPickUpZoneDeliveryModeModel.getInternalStoreAddress() , addressData);
      target.setDeliveryAddress(addressData);
    }

    if(source.getDeliveryMode() instanceof BlPickUpZoneDeliveryModeModel) {
      setpickupPersonDetails(source , target);
    }

    if(source.getUser() instanceof CustomerModel){
      target.setIsPOEnabled(((CustomerModel) source.getUser()).isPoEnabled());
    }

    // To Populate Gift Card Details
    populateGiftCardDetails(source , target);
    if(BooleanUtils.isTrue(source.getIsRetailGearOrder())){
      target.setIsNewGearOrder(source.getIsRetailGearOrder());
    }
    
 // BL-1134 to add total discount with gift cart discount to display on summary section
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
    populateTrackingNumberAndUrl(source , target);
  }


  /**
   * This method created to populate dates for order details
   */
  private void populateDatesForOrderDetails(final OrderModel source , final OrderData target) {
    if(null != source.getRentalStartDate() && null != source.getRentalEndDate()) {
      target.setRentalStartDate(
          convertDateToString(source.getRentalStartDate(), BlFacadesConstants.RENTAL_DATE_FORMAT));
      target.setRentalEndDate(
          convertDateToString(updateRentalDatesIfOrderIsExtended(source),  BlFacadesConstants.RENTAL_DATE_FORMAT));
      target.setRentalFormattedStartDate(convertDateToString(source.getRentalStartDate(),
          BlFacadesConstants.FORMATTED_RENTAL_DATE));
      target.setRentalFormattedEndDate(
          convertDateToString(updateRentalDatesIfOrderIsExtended(source), BlFacadesConstants.FORMATTED_RENTAL_DATE));
      target.setTotalRentalDays(String.valueOf(BlDateTimeUtils
          .getDaysBetweenDates(source.getRentalStartDate(), updateRentalDatesIfOrderIsExtended(source))));

      target.setIsRentalStartDateActive(new Date().before(source.getRentalStartDate()));
      target.setIsRentalEndDateActive(new Date().before(updateRentalDatesIfOrderIsExtended(source)));
      target.setIsRentalActive(isRentalCartAcive(source));
    }
    target.setOrderedDate(convertDateToString(source.getDate(), BlFacadesConstants.FORMATTED_RENTAL_DATE));
    target.setOrderedFormatDate(convertDateToString(source.getDate() , BlFacadesConstants.ORDER_FORMAT_PATTERN));
  }

  /**
   * This method created to populate prices for order details
   */
  private void populatePriceDetails(final OrderModel source , final OrderData target) {
    target.setTotalDamageWaiverCost(convertDoubleToPriceData(updateOrderDetailsIfOrderExtended(source , source.getTotalDamageWaiverCost() ,
        BlFacadesConstants.DAMAGE_WAIVER_FIELD) , source));
    target.setTaxAvalaraCalculated(convertDoubleToPriceData(updateOrderDetailsIfOrderExtended(source , source.getTotalTax() ,
        BlFacadesConstants.TOTAL_TAX_FIELD) , source));
    target.setTotalPriceWithTax(convertDoubleToPriceData(updateOrderDetailsIfOrderExtended(source ,source.getTotalPrice() ,
        BlFacadesConstants.TOTAL_PRICE_FIELD), source));
    target.setSubTotal(convertDoubleToPriceData(updateOrderDetailsIfOrderExtended(source ,source.getSubtotal() ,
        BlFacadesConstants.SUB_TOTAL_FIELD), source));
    final Double discountAmount = Objects.isNull(source.getTotalDiscounts()) ?  0.0 : source.getTotalDiscounts();
    final Double giftCartAMount =  Objects.isNull(source.getGiftCardAmount()) ? 0.0: source.getGiftCardAmount();
    final Double totalDisount = discountAmount + giftCartAMount;
    target.setTotalDiscounts(convertDoubleToPriceData(updateOrderDetailsIfOrderExtended(source , totalDisount ,
        BlFacadesConstants.DISCOUNT_FIELD), source));
    if(source.getTotalOptionsCost() != null){
      target.setTotalOptionsCost(convertDoubleToPriceData(updateOrderDetailsIfOrderExtended(source , source.getTotalOptionsCost() ,
          BlFacadesConstants.OPTION_FIELD), source));
    }

  }

  /**
   * This method created to populate dates  for rental order details
   */
  private void populateOrderDetailsForRentalOrder(final OrderModel source , final OrderData target) {
    target.setOrderedFormatDateForExtendRental(convertDateToString(source.getDate() , BlFacadesConstants.EXTEND_ORDER_FORMAT_PATTERN));
    if(BooleanUtils.isTrue(source.getIsRentalOrder())) {
      target.setRentalEndDateForJs(convertDateToString(source.getRentalEndDate(), BlFacadesConstants.START_DATE_PATTERN));
      target.setRentalStartDateForJs(
          convertDateToString(source.getRentalStartDate(), BlFacadesConstants.EXTEND_ORDER_FORMAT_PATTERN_FOR_JS));
      populateExtendOrderDetails(source, target);
    }

    // To set the date for selecting in calendar
    if(CollectionUtils.isNotEmpty(target.getExtendOrderEntrie())) {
      populateRentalEndDateForJs(source, target);
    }
  }

  /**
   * This method populate extend order details to order data
   */
  private void populateExtendOrderDetails(final OrderModel orderModel , final OrderData orderData) {

    final List<ExtendOrderData> extendOrderDataList = new ArrayList<>();
    if(CollectionUtils.isNotEmpty(orderModel.getExtendedOrderCopyList())) {
      for(AbstractOrderModel extendOrderModel : orderModel.getExtendedOrderCopyList()) {
       final ExtendOrderData extendOrderData = new ExtendOrderData();
       extendOrderData.setExtendOrderCost(convertDoubleToPriceData(extendOrderModel.getTotalPrice() , orderModel));
       extendOrderData.setExtendOrderDamageWaiverCost(convertDoubleToPriceData(extendOrderModel.getTotalDamageWaiverCost(), orderModel));
       String suffix = BlFacadesConstants.DAYS;
       if(extendOrderModel.getTotalExtendDays() <= 1) {
         suffix = BlFacadesConstants.DAY;
       }
       extendOrderData.setExtendOrderDaysWithoutPrevOrder(extendOrderModel.getTotalExtendDays() + BlFacadesConstants.BLANK + suffix);
       extendOrderData.setExtendOrderEndDate(convertDateToString(extendOrderModel.getRentalEndDate() , BlFacadesConstants.EXTEND_ORDER_FORMAT_PATTERN));
       extendOrderDataList.add(extendOrderData);
      }
    }
    orderData.setExtendOrderEntrie(extendOrderDataList);
  }

  /**
   * This method created to update the rental end date for calendar if order is extended
   */
  private void populateRentalEndDateForJs(final OrderModel orderModel , final OrderData orderData) {

    final List<AbstractOrderModel> orderModelList = orderModel.getExtendedOrderCopyList();

      final int size = orderModelList.size();
      for (final AbstractOrderModel extendOrder :orderModelList) {
        if (BooleanUtils.isTrue(extendOrder.getIsExtendedOrder()) && extendOrder
            .getExtendOrderStatus().getCode()
            .equalsIgnoreCase(ExtendOrderStatusEnum.COMPLETED.getCode())
            && orderModelList.get(size - 1).getPk()
            .equals(extendOrder.getPk())) {
          orderData.setRentalEndDateForJs(convertDateToString(extendOrder.getRentalEndDate() , BlFacadesConstants.START_DATE_PATTERN));
        }
    }

  }

  /**
   * This method created to populate the order notes
   */
  private void populateOrderNotes(final OrderModel orderModel, final OrderData orderData) {

    final StringBuilder orderNotes = new StringBuilder();
    final List<NotesModel> notesModelList = orderModel.getOrderNotes();

    if (CollectionUtils.isNotEmpty(notesModelList)) {
      for (NotesModel notesModel : notesModelList) {
        if (NotesEnum.CUSTOMER_CHECKOUT_ORDER_NOTES.equals(notesModel.getType())) {
          orderNotes.append(notesModel.getNote());
        }
      }
      populateCustomerOwnedNote(notesModelList, orderData);
    }

    orderData.setOrderNotes(orderNotes.toString());
  }
  
  /**
	 * Populate customer owned note.
	 *
	 * @param notesModelList
	 *           the notes model list
	 * @param orderData
	 *           the order data
	 */
	private void populateCustomerOwnedNote(final List<NotesModel> notesModelList, final OrderData orderData)
	{
		final List<NotesModel> filteredNoteList = notesModelList.stream()
				.filter(note -> note.getType().equals(NotesEnum.CUSTOMER_OWNED_ITEMS_NOTES)).collect(Collectors.toList());
		if (CollectionUtils.isNotEmpty(filteredNoteList))
		{
			if (filteredNoteList.size() >= BlFacadesConstants.TWO)
			{
				Collections.sort(filteredNoteList, (note1,note2) -> note1.getCreationtime().compareTo(note2.getCreationtime()));
			}
			orderData.setCustomerOwnedOrderNote(filteredNoteList.get(filteredNoteList.size() - BlFacadesConstants.ONE).getNote());
		}
	}

  /**
   * This method is created to update the rental end date , if order is extended
   */

  private Date updateRentalDatesIfOrderIsExtended(final OrderModel orderModel) {
    final List<AbstractOrderModel> orderModelList = orderModel.getExtendedOrderCopyList();

    if(CollectionUtils.isNotEmpty(orderModelList)) {
      final int size = orderModelList.size();
      for (final AbstractOrderModel extendOrder : orderModelList) {
        if (BooleanUtils.isTrue(extendOrder.getIsExtendedOrder()) && extendOrder
            .getExtendOrderStatus().getCode()
            .equalsIgnoreCase(ExtendOrderStatusEnum.COMPLETED.getCode())
            && orderModelList.get(size - 1).getPk()
            .equals(extendOrder.getPk())) {
          return extendOrder.getRentalEndDate();
        }
      }
    }

    return orderModel.getRentalEndDate();
  }

  /**
   * This method to update order summary section , if order is extended
   */
  private Double updateOrderDetailsIfOrderExtended(final OrderModel orderModel , final Double actualPrice , final  String field) {

    BigDecimal price = BigDecimal.valueOf(actualPrice);
    if(CollectionUtils.isNotEmpty(orderModel.getExtendedOrderCopyList())) {

      for(final AbstractOrderModel extendOrder : orderModel.getExtendedOrderCopyList()){
        if(BooleanUtils.isTrue(extendOrder.getIsExtendedOrder()) &&
            extendOrder.getExtendOrderStatus().getCode().equalsIgnoreCase(ExtendOrderStatusEnum.COMPLETED.getCode())){
          if(BlFacadesConstants.TOTAL_PRICE_FIELD.equalsIgnoreCase(field)) {
            price = price.add(BigDecimal.valueOf(extendOrder.getTotalPrice()));
          }
          else if(BlFacadesConstants.TOTAL_TAX_FIELD.equalsIgnoreCase(field)) {
            price = price.add(BigDecimal.valueOf(extendOrder.getTotalTax()));
          }
          else if(BlFacadesConstants.SUB_TOTAL_FIELD.equalsIgnoreCase(field)) {
            price = price.add(BigDecimal.valueOf(extendOrder.getSubtotal()));
          }
          else if(BlFacadesConstants.DAMAGE_WAIVER_FIELD.equalsIgnoreCase(field)){
            price = price.add(BigDecimal.valueOf(extendOrder.getTotalDamageWaiverCost()));
          }
          else if(BlFacadesConstants.DISCOUNT_FIELD.equalsIgnoreCase(field)){
            price = price.add(BigDecimal.valueOf(extendOrder.getTotalDiscounts()));
          }
        }
      }
    }
    return price.doubleValue();
  }

  /**
   * This Method converts rental startDate and rental endDate to String
   */
  private String convertDateToString(final Date rentalDate , final String dateFormat) {
    return BlDateTimeUtils.convertDateToStringDate(rentalDate,dateFormat);
  }

  /**
   * This method converts double to price data
   */
  private PriceData convertDoubleToPriceData(final Double price , OrderModel orderModel) {
    return getPriceDataFactory().create(PriceDataType.BUY ,BigDecimal.valueOf(price),orderModel.getCurrency());
  }

  /**
   * This method created to check whether rental order is active or not
   */
  private boolean isRentalCartAcive(final OrderModel orderModel){
    final Date date = new Date();
    return date.before(orderModel.getRentalStartDate()) || date.before(updateRentalDatesIfOrderIsExtended(orderModel))
    || DateUtils.isSameDay(updateRentalDatesIfOrderIsExtended(orderModel), date);
  }


  /**
   * This method created to populate the gift card details for order details
   */

  private void populateGiftCardDetails(final OrderModel source, final OrderData target)
  {
    final List<BLGiftCardData> blGiftCardDataList = new ArrayList<>();
    final List<GiftCardModel> appliedGcList = Lists.newArrayList();
    if(CollectionUtils.isNotEmpty(source.getGiftCard()))
    {
   	 appliedGcList.addAll(source.getGiftCard());
    }    
    filterGcList(appliedGcList, source.getModifiedOrderAppliedGcList());    
    processGcDetails(source, blGiftCardDataList, appliedGcList);
    target.setGiftCardData(blGiftCardDataList);
  }


  /**
   * Process Gift Card details.
   *
   * @param source
   *           the source
   * @param blGiftCardDataList
   *           the bl gift card data list
   * @param gcList
   *           the gc list
   */
  private void processGcDetails(final OrderModel source, final List<BLGiftCardData> blGiftCardDataList,
		  final List<GiftCardModel> gcList)
  {
	  if (CollectionUtils.isNotEmpty(gcList))
	  {
		  for (final GiftCardModel giftCardModel : gcList)
		  {
			  processGcMovements(source, blGiftCardDataList, giftCardModel);
		  }
	  }
  }


  /**
   * Process Gift Card movements details.
   *
   * @param source
   *           the source
   * @param blGiftCardDataList
   *           the bl gift card data list
   * @param giftCardModel
   *           the gift card model
   */
  private void processGcMovements(final OrderModel source, final List<BLGiftCardData> blGiftCardDataList,
		  final GiftCardModel giftCardModel)
  {
	  for (final GiftCardMovementModel giftCardMovementModel : giftCardModel.getMovements())
	  {
		  if (doCheckGC(source, giftCardMovementModel))
		  {
			  final BLGiftCardData blGiftCardData = new BLGiftCardData();
			  blGiftCardData.setCode(giftCardModel.getCode());
			  setGiftCardData(giftCardMovementModel, blGiftCardData, blGiftCardDataList, source);
		  }
	  }
  }
  
  /**
   * Do check GC before adding data.
   *
   * @param source
   *           the source
   * @param blGiftCardDataList
   *           the bl gift card data list
   * @param giftCardModel
   *           the gift card model
   * @param giftCardMovementModel
   *           the gift card movement model
   * @return true, if successful
   */
  private boolean doCheckGC(final OrderModel source, final GiftCardMovementModel giftCardMovementModel)
  {
	  return null != giftCardMovementModel.getOrder()
			  && source.getCode().equalsIgnoreCase(giftCardMovementModel.getOrder().getCode());
  }
  
  /**
   * Filter GiftCard list having same GiftCard code.
   *
   * @param appliedGcList the applied gc list
   * @param gcList the gc list
   */
  private void filterGcList(final List<GiftCardModel> appliedGcList, final List<GiftCardModel> gcList)
  {
	  if(CollectionUtils.isNotEmpty(gcList))
	  {
		  gcList.forEach(giftCard -> {
			  if(appliedGcList.stream().noneMatch(gc -> gc.getCode().equalsIgnoreCase(giftCard.getCode())))
			  {
				  appliedGcList.add(giftCard);
			  }
		  });
	  }
  }

  /**
   * This method added for setting gift card details for order details page
   * @param giftCardMovementModel giftCardMovementModel used for order
   * @param blGiftCardData blGiftCardData
   * @param blGiftCardDataList blGiftCardDataList to show on order page
   * @param source orderModel
   */
  private void setGiftCardData(final GiftCardMovementModel giftCardMovementModel , final BLGiftCardData blGiftCardData ,
      final List<BLGiftCardData> blGiftCardDataList, final OrderModel source ){
    Double balanceAmount = 0.0;
    Double amount = 0.0;
    if(null != giftCardMovementModel.getBalanceAmount()) {
      balanceAmount = giftCardMovementModel.getBalanceAmount();
    }
    if(null != giftCardMovementModel.getAmount()){
      amount = giftCardMovementModel.getAmount();
    }
    blGiftCardData.setBalanceamount(convertDoubleToPriceData(balanceAmount , source));
    blGiftCardData.setRedeemamount(convertDoubleToPriceData(amount , source));
    blGiftCardDataList.add(blGiftCardData);
  }

  
 /**
 * @param source for the order
 * @param val for the price
 * @return PriceData
 */
private PriceData createPrice(final AbstractOrderModel source, final Double val)
  {
    if (source == null)
    {
      throw new IllegalArgumentException("source order must not be null");
    }

    final CurrencyModel currency = source.getCurrency();
    if (currency == null)
    {
      throw new IllegalArgumentException("source order currency must not be null");
    }

    // Get double value, handle null as zero
    final double priceValue = val != null ? val.doubleValue() : 0d;

    return getPriceDataFactory().create(PriceDataType.BUY, BigDecimal.valueOf(priceValue), currency);
  }


  /**
   * To set pickup details for order
   * @param source ordermodel
   * @param target orderdata
   */
  private void setpickupPersonDetails(final OrderModel source, final OrderData target) {
  target.setPickUpPersonFirstName(getValue(source.getPickUpPersonFirstName()));
  target.setPickUpPersonLastName(getValue(source.getPickUpPersonLastName()));
  target.setPickUpPersonEmail(getValue(source.getPickUpPersonEmail()));
  target.setPickUpPersonPhone(getValue(source.getPickUpPersonPhone()));
  }

  /**
   * This common method created to check string value
   * @param value value
   * @return string
   */

  private String getValue(final String value){
  return StringUtils.isBlank(value) ? StringUtils.EMPTY : value;
  }

  /**
   * This method created to get tracking number and tracking URL from package
   * @param orderModel orderModel
   * @param orderData orderDate
   */
  private void populateTrackingNumberAndUrl(final OrderModel orderModel, final OrderData orderData) {
    final Map<String, String> trackingNumberInfo = new HashMap<>();
    if(CollectionUtils.isNotEmpty(orderModel.getConsignments())){
      orderModel.getConsignments().forEach(consignmentModel -> {
        if(CollectionUtils.isNotEmpty(consignmentModel.getPackaginginfos())){
          consignmentModel.getPackaginginfos().forEach(packagingInfoModel -> {
            if(StringUtils.isNotEmpty(packagingInfoModel.getOutBoundTrackingNumber())){
              trackingNumberInfo.put(packagingInfoModel.getOutBoundTrackingNumber() ,
                  StringUtils.isBlank(packagingInfoModel.getLabelURL()) ? BlFacadesConstants.HASH :packagingInfoModel.getLabelURL());
            }
          });
        }
      });
    }

    orderData.setTrackingNumber(trackingNumberInfo);
  }


  public PriceDataFactory getPriceDataFactory() {
    return priceDataFactory;
  }

  public void setPriceDataFactory(
      PriceDataFactory priceDataFactory) {
    this.priceDataFactory = priceDataFactory;
  }

  public BlAddressPopulator getBlAddressPopulator() {
    return blAddressPopulator;
  }

  public void setBlAddressPopulator(BlAddressPopulator blAddressPopulator) {
    this.blAddressPopulator = blAddressPopulator;
  }
}
