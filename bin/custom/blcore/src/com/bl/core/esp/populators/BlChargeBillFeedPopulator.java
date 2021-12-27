package com.bl.core.esp.populators;

import com.bl.esp.constants.BlespintegrationConstants;
import com.bl.esp.dto.OrderFeedData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class BlChargeBillFeedPopulator <SOURCE extends AbstractOrderModel, TARGET extends OrderFeedData>  implements
    Populator<SOURCE, TARGET> {

  @Override
  public void populate(final AbstractOrderModel abstractOrderModel, final OrderFeedData target) throws ConversionException {
    final Document orderItemsInXMLDocument = target.getData();
    final Element rootOrderItems = target.getElement();
    final Element rootOrderItem = createRootElementForRootElement(orderItemsInXMLDocument,
        rootOrderItems, BlespintegrationConstants.ORDER);
    createOrderBillInfo(abstractOrderModel, orderItemsInXMLDocument, rootOrderItem);
        createOrderbillItemInfo(abstractOrderModel, orderItemsInXMLDocument, rootOrderItem);
  }


  protected Element createRootElementForRootElement(final Document document, final Element rootElement, final String rootElementName) {
    final Element childElement = document.createElement(rootElementName);
    rootElement.appendChild(childElement);
    return childElement;
  }

  private void createOrderBillInfo(final AbstractOrderModel abstractOrderModel, final Document orderItemsInXMLDocument, final Element rootOrderItems) {
    final SimpleDateFormat formatter = new SimpleDateFormat("orderBills");


    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.ORDER_ID, getRequestValue(abstractOrderModel.getCode()));
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.OLD_ORDER, StringUtils.EMPTY);
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.SUBSCRIBER_ID, getRequestValue(abstractOrderModel.getUser().getUid()));
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.EMAIL_ADDRESS, getRequestValue(abstractOrderModel.getUser().getUid()));
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.TYPE, getOrderType(abstractOrderModel));
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.REPLACEMENT, BooleanUtils.isTrue(abstractOrderModel.getIsCartUsedForReplacementOrder())
            ? Boolean.TRUE.toString() : Boolean.FALSE.toString());
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.STATUS,getRequestValue(getOrderStatus(abstractOrderModel)));
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.DATE_PLACED, String.valueOf(abstractOrderModel.getCreationtime()));
    if(Objects.nonNull(abstractOrderModel.getDeliveryMode())) {
      final ZoneDeliveryModeModel delivery = ((ZoneDeliveryModeModel) abstractOrderModel
          .getDeliveryMode());
      createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
          BlespintegrationConstants.SHIPPING_METHOD_TYPE, getRequestValue(delivery.getShippingGroup().getName()));
      createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
          BlespintegrationConstants.SHIPPING_METHOD, getRequestValue(delivery.getCode()));
    }


    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.ARRIVAL_DATE, formatter.format(abstractOrderModel.getRentalStartDate()));
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.RETURN_DATE, formatter.format(abstractOrderModel.getRentalEndDate()));
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.ACTUAL_RETURN_DATE, formatter.format(abstractOrderModel.getRentalEndDate()));

    final UserModel userModel = abstractOrderModel.getUser();
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.CUSTOMER_NAME, getRequestValue(userModel.getName()));
    double totalPendingBill = getTotalPendingBill(abstractOrderModel);
   // for total bill amount
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.LAST_UPDATED,formatter.format(abstractOrderModel.getOrderModifiedDate()));
  }

  private void createOrderbillItemInfo(final AbstractOrderModel orderModel, final Document orderItemsInXMLDocument, final Element rootOrderItems) {
    final Element rootOrderItem = createRootElementForRootElement(orderItemsInXMLDocument, rootOrderItems, "orderBillItems");
    if(Objects.nonNull(orderModel.getPaymentInfo()) && Objects.nonNull(orderModel.getPaymentInfo().getBillingAddress())){
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
          BlespintegrationConstants.ORDER_ID, getRequestValue(orderModel.getCode()));
      final AddressModel billingAddress = orderModel.getPaymentInfo().getBillingAddress();
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.BILLING_FIRST_NAME,
          getRequestValue(billingAddress.getFirstname()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.BILLING_LAST_NAME,
          getRequestValue(billingAddress.getLastname()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.BILLING_ORGANIZATION,
          getRequestValue(billingAddress.getCompany()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.BILLING_ADDRESS_1,
          getRequestValue(billingAddress.getLine1()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.BILLING_ADDRESS_2,
          getRequestValue(billingAddress.getLine2()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.BILLING_CITY,
          getRequestValue(billingAddress.getTown()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.BILLING_STATE,
          Objects.nonNull(billingAddress.getRegion()) ? billingAddress.getRegion().getName() : StringUtils.EMPTY);
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.BILLING_ZIP_CODE,
          getRequestValue(billingAddress.getPostalcode()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.BILLING_PHONE,
          getRequestValue(billingAddress.getCellphone()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.BILLING_EMAIL,
          getRequestValue(billingAddress.getEmail()));

      // for billing notes
     // createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.BILLING_NOTES,getOrderNotesFromOrderModel(orderModel));

       }
  }

  protected Element createElementForRootElement(final Document document, final Element rootElement, final String element, final String value) {
    final Element childElement = document.createElement(element);
    childElement.appendChild(document.createTextNode(value));
    rootElement.appendChild(childElement);
    return childElement;
  }

  /**
   * To get the request value based
   * @param value value get from order
   * @return value to set on request
   */
  protected String getRequestValue(final String value){
    return StringUtils.isBlank(value) ? StringUtils.EMPTY :value;
  }
  protected String getOrderType(final AbstractOrderModel orderModel){
    final AtomicReference<String> orderType = new AtomicReference<>(StringUtils.EMPTY);
    if(BooleanUtils.isTrue(orderModel.isGiftCardOrder())) {
      orderType.set(BlespintegrationConstants.GIFT_CARD_ORDER);
    }
    else if(BooleanUtils.isTrue(orderModel.getIsNewGearOrder())){
      orderType.set(BlespintegrationConstants.NEW_GEAR_ORDER);
    }
    else if(BooleanUtils.isTrue(orderModel.getIsRentalCart())){
      orderType.set(BlespintegrationConstants.RENTAL);
    }
    else if(BooleanUtils.isFalse(orderModel.getIsRentalCart())){
      orderType.set(BlespintegrationConstants.USED_GEAR);
    }

    return orderType.get();
  }

  /**
   * This method created to get order status from order model
   * @param orderModel orderModel
   * @return String
   */
  private String getOrderStatus(final AbstractOrderModel orderModel) {
    return Objects.isNull(orderModel.getStatus()) ? StringUtils.EMPTY : orderModel.getStatus().getCode();
  }

 private double getTotalPendingBill(AbstractOrderModel abstractOrderModel){
    final AtomicReference<Double> pendingBill= new AtomicReference<>(0.0);
    abstractOrderModel.getConsignments().forEach(consignmentModel -> {
      consignmentModel.getConsignmentEntries().forEach(consignmentEntryModel -> {
        consignmentEntryModel.getBillingCharges().forEach((serialId,pendingBillList) ->{
          BigDecimal bill = new BigDecimal(0);
          pendingBillList.forEach(blItemsBillingChargeModel -> {
            pendingBill.set(pendingBill.get()+ (blItemsBillingChargeModel.getChargedAmount().doubleValue()));
          });
        });
      });
    });
    return pendingBill.get();
 }
}
