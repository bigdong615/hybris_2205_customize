package com.bl.facades.populators;

import com.bl.core.enums.ExtendOrderStatusEnum;
import com.bl.core.model.BlPickUpZoneDeliveryModeModel;
import com.bl.core.model.NotesModel;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.constants.BlFacadesConstants;
import com.bl.facades.product.data.ExtendOrderData;

import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.product.PriceDataFactory;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
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
   	 target.setIsRentalCart(Boolean.FALSE);
   	 target.setHasGiftCart(Boolean.TRUE);
   	
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
       extendOrderData.setExtendOrderDaysWithoutPrevOrder(extendOrderModel.getTotalExtendDays() + BlFacadesConstants.BLANK + BlFacadesConstants.DAYS);
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
