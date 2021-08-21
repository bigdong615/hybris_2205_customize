package com.bl.facades.populators;

import com.bl.core.enums.ExtendOrderStatusEnum;
import com.bl.core.enums.ItemBillingChargeTypeEnum;
import com.bl.core.model.BlPickUpZoneDeliveryModeModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.core.model.NotesModel;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.constants.BlFacadesConstants;
import com.bl.facades.giftcard.data.BLGiftCardData;
import com.bl.facades.product.data.AvailabilityMessage;
import com.bl.facades.product.data.ExtendOrderData;
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
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang3.time.DateUtils;


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
    target.setIsRentalCart(source.getIsRentalCart());
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

    if ((null != source.getReturnRequestForOrder()) && (source.getIsCartUsedForReplacementOrder().booleanValue())) {
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
    if(source.getUser() instanceof CustomerModel){
      target.setIsPOEnabled(((CustomerModel) source.getUser()).isPoEnabled());
    }

    // As of now commented below code for bill pay tax , since its breaking normal flow of order details page
   /* boolean allowed = false;
    if(BooleanUtils.isTrue(source.getUnPaidBillPresent()) && allowed) {   // NOSONR
      final AtomicDouble totalAmt = new AtomicDouble(0.0);
      final double priviousTax = source.getTotalTax();
      source.setUnPaidBillPresent(true);
      getDefaultBlExternalTaxesService().calculateExternalTaxes(source);
      final double currentTax = source.getTotalTax();

      source.setTotalTax(priviousTax);
      source.setUnPaidBillPresent(false);
      source.getConsignments()
          .forEach(consignment -> consignment.getConsignmentEntries()
              .forEach(consignmentEntry -> consignmentEntry
                  .getBillingCharges()
                  .forEach((serialCode, listOfCharges) -> listOfCharges.forEach(billing -> {
                    target.getEntries().forEach(entry -> {
                      if (entry.getProduct().getCode()
                          .equals(getSkuCode(consignmentEntry, serialCode))) {
                        final AvailabilityMessage messageForBillingType = getMessageForBillingType(
                            billing.getBillChargeType());
                        final List<AvailabilityMessage> messages = Lists
                            .newArrayList(CollectionUtils.emptyIfNull(entry.getMessages()));
                        messages.add(messageForBillingType);
                        entry.setMessages(messages);
                      }
                    });
                    totalAmt.addAndGet(billing.getChargedAmount().doubleValue());
                  }))));

      target.setExtensionBillingCost(convertDoubleToPriceData(totalAmt.get(), source));
      target.setTotalPayBillTax(convertDoubleToPriceData(currentTax, source));
      target.setOrderTotalWithTaxForPayBill(
          convertDoubleToPriceData(totalAmt.get() + currentTax, source));
    }
*/
    // To Populate Gift Card Details
    populateGiftCardDetails(source , target);
    if(BooleanUtils.isTrue(source.getIsNewGearOrder())){
      target.setIsNewGearOrder(source.getIsNewGearOrder());
    }
  }

  
/**
 * This method created pay bill messages separation.
 * @param billChargeType
 * @return AvailabilityMessage
 */
private AvailabilityMessage getMessageForBillingType(final ItemBillingChargeTypeEnum billChargeType)
  {
    switch (billChargeType.getCode())
    {
      case "MISSING_CHARGE":
        return getMessage("pay.bill.missing.charge");

      case "LATE_CHARGE":
        return getMessage("pay.bill.late.charge");

      case "REPAIR_CHARGE":
        return getMessage("pay.bill.repair.charge");

      default:
        return null;
    }
  }

  
/**
 * This method created pay bill messages.
 * @param messageCode
 * @return AvailabilityMessage
 */
private AvailabilityMessage getMessage(final String messageCode)
  {
    final AvailabilityMessage am = new AvailabilityMessage();
    am.setMessageCode(messageCode);
    return am;

  }

  
/**
  * This method created to get SKU code.
  * @param consignmentEntry
  * @param serialCode
  * @return String
  */
private String getSkuCode(final ConsignmentEntryModel consignmentEntry, final String serialCode)
  {
    final StringBuilder sb = new StringBuilder();
    consignmentEntry.getSerialProducts().forEach(serial -> {
      if(serial instanceof BlSerialProductModel && ((BlSerialProductModel)serial).getCode().equals(serialCode))
      {
        final BlSerialProductModel serialProduct = ((BlSerialProductModel) serial);
        sb.append(serialProduct.getBlProduct().getCode());
      }
    });
    return sb.toString();
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
          .getDaysBetweenDates(source.getRentalStartDate(), updateRentalDatesIfOrderIsExtended(source)) + 1));

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
    final Double discountAmount = source.getTotalDiscounts();
    final Double giftCartAMount = source.getGiftCardAmount();
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
    if(BooleanUtils.isTrue(source.getIsRentalCart())) {
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
  private void populateOrderNotes(final OrderModel orderModel , final OrderData orderData) {

    String orderNotes = BlFacadesConstants.EMPTY;
    final List<NotesModel> notesModelList = orderModel.getOrderNotes();
    if(CollectionUtils.isNotEmpty(notesModelList)) {
      int notesModelSize = notesModelList.size();
      for (NotesModel notesModel :notesModelList) {
        if(notesModelList.get(notesModelSize -1).getPk().equals(notesModel.getPk())) {
          orderNotes = notesModel.getNote();
        }
      }
    }

    orderData.setOrderNotes(orderNotes);
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
    if(CollectionUtils.isNotEmpty(source.getGiftCard())) {
      for(final GiftCardModel giftCardModel : source.getGiftCard()){
        final BLGiftCardData blGiftCardData = new BLGiftCardData();
        blGiftCardData.setCode(giftCardModel.getCode());
        for(final GiftCardMovementModel giftCardMovementModel : giftCardModel.getMovements()){
          if(null != giftCardMovementModel.getOrder() && source.getCode().equalsIgnoreCase(giftCardMovementModel.getOrder().getCode())) {
            setGiftCardData(giftCardMovementModel , blGiftCardData , blGiftCardDataList , source);
          }
        }
      }

    }
    target.setGiftCardData(blGiftCardDataList);
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
