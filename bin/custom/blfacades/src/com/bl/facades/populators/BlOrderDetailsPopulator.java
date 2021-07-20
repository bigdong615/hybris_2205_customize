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
import de.hybris.platform.commerceservices.constants.GeneratedCommerceServicesConstants.Attributes.Order;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ruleengineservices.order.dao.ExtendedOrderDao;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;

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
    if(null != source.getRentalStartDate() && null != source.getRentalEndDate()) {
      target.setRentalStartDate(
          convertDateToString(source.getRentalStartDate(), BlFacadesConstants.RENTAL_DATE_FORMAT));
      target.setRentalEndDate(
          convertDateToString(source.getRentalEndDate(), BlFacadesConstants.RENTAL_DATE_FORMAT));
      target.setRentalFormattedStartDate(convertDateToString(source.getRentalStartDate(),
          BlFacadesConstants.FORMATTED_RENTAL_DATE));
      target.setRentalFormattedEndDate(
          convertDateToString(source.getRentalEndDate(), BlFacadesConstants.FORMATTED_RENTAL_DATE));
      target.setTotalRentalDays(String.valueOf(BlDateTimeUtils
          .getDaysBetweenDates(source.getRentalStartDate(), source.getRentalEndDate()) + 1));
    }
    target.setOrderedDate(convertDateToString(source.getDate(), BlFacadesConstants.FORMATTED_RENTAL_DATE));
    target.setOrderedFormatDate(convertDateToString(source.getDate() , "MMM d , YYYY hh:mm a"));
    target.setTotalDamageWaiverCost(convertDoubleToPriceData(source.getTotalDamageWaiverCost() , source));
   target.setTaxAvalaraCalculated(convertDoubleToPriceData(source.getTotalTax() , source));
   target.setTotalPriceWithTax(convertDoubleToPriceData(source.getTotalPrice(), source));
   final Double discountAmount = source.getTotalDiscounts();
   final Double giftCartAMount = source.getGiftCardAmount();
   final Double totalDisount = discountAmount + giftCartAMount;
   target.setTotalDiscounts(convertDoubleToPriceData(totalDisount, source));
   target.setPickUpPersonEmail(source.getPickUpPersonFirstName());
   target.setPickUpPersonLastName(source.getPickUpPersonLastName());
   target.setPickUpPersonEmail(source.getPickUpPersonEmail());
   target.setPickUpPersonPhone(source.getPickUpPersonPhone());
    target.setOrderedFormatDateForExtendRental(convertDateToString(source.getDate() , "MMM d , YYYY"));
    if(BooleanUtils.isTrue(source.getIsRentalCart())) {
      target.setRentalEndDateForJs(convertDateToString(source.getRentalEndDate(), "MM/dd/yyyy"));
      target.setRentalStartDateForJs(
          convertDateToString(source.getRentalStartDate(), "yyyy ,MM, dd"));
      populateExtendOrderDetails(source, target);
    }

    // To set the date for selecting in calendar
    if(CollectionUtils.isNotEmpty(target.getExtendOrderEntrie())) {
      populateRentalEndDateForJs(source, target);
    }

    populateOrderNotes(source , target);

    if(null == target.getDeliveryAddress() && source.getDeliveryMode() instanceof BlPickUpZoneDeliveryModeModel) {
      final AddressData addressData = new AddressData();
      BlPickUpZoneDeliveryModeModel blPickUpZoneDeliveryModeModel = (BlPickUpZoneDeliveryModeModel) source.getDeliveryMode();
      getBlAddressPopulator().populate(blPickUpZoneDeliveryModeModel.getInternalStoreAddress() , addressData);
      target.setDeliveryAddress(addressData);
    }

  }

  /**
   * This method poulates extend order details to order data
   */
  private void populateExtendOrderDetails(final OrderModel orderModel , final OrderData orderData) {

    final List<ExtendOrderData> extendOrderDataList = new ArrayList<>();
    if(CollectionUtils.isNotEmpty(orderModel.getExtendedOrderCopyList())) {
      for(AbstractOrderModel extendOrderModel : orderModel.getExtendedOrderCopyList()) {
       final ExtendOrderData extendOrderData = new ExtendOrderData();
       extendOrderData.setExtendOrderCost(convertDoubleToPriceData(extendOrderModel.getTotalPrice() , orderModel));
       extendOrderData.setExtendOrderDamageWaiverCost(convertDoubleToPriceData(extendOrderModel.getTotalDamageWaiverCost(), orderModel));
       extendOrderData.setExtendOrderDaysWithoutPrevOrder(extendOrderModel.getTotaExtendDays() + "days");
       extendOrderData.setExtendOrderEndDate(convertDateToString(extendOrderModel.getRentalEndDate() , "MMM d , YYYY"));
       extendOrderDataList.add(extendOrderData);
      }
    }
    orderData.setExtendOrderEntrie(extendOrderDataList);
  }

  private void populateRentalEndDateForJs(final OrderModel orderModel , final OrderData orderData) {

    final  List<AbstractOrderModel> orderModelList = orderModel.getExtendedOrderCopyList();

      final int size = orderModelList.size();
      for (final AbstractOrderModel extendOrder :orderModelList) {
        if (BooleanUtils.isTrue(extendOrder.getIsExtendedOrder()) && extendOrder
            .getExtendOrderStatus().getCode()
            .equalsIgnoreCase(ExtendOrderStatusEnum.COMPLETED.getCode())
            && orderModelList.get(size - 1).getPk()
            .equals(extendOrder.getPk())) {
          orderData.setRentalEndDateForJs(convertDateToString(extendOrder.getRentalEndDate() , "MM/dd/yyyy"));
        }
    }

  }

  private void populateOrderNotes(final OrderModel orderModel , final OrderData orderData) {

    String orderNotes = "";
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
