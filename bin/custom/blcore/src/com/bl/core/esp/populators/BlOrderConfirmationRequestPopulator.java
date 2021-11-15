/**
 *
 */
package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlSerialProductModel;
import com.bl.esp.dto.orderconfirmation.OrderConfirmationEventRequest;
import com.bl.esp.dto.orderconfirmation.data.OrderConfirmationData;
import com.bl.esp.exception.BlESPIntegrationException;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import com.braintree.model.BrainTreePaymentInfoModel;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.Map;
import java.util.Objects;
import javax.xml.transform.Transformer;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.util.Assert;
import org.w3c.dom.Document;
import org.w3c.dom.Element;


/**
 * This populator created for order confirmation ESP Event
 * @author Manikandan
 */
public class BlOrderConfirmationRequestPopulator  extends ESPEventCommonPopulator<OrderModel, OrderConfirmationEventRequest> {

  private static final org.apache.log4j.Logger LOG = Logger.getLogger(BlOrderConfirmationRequestPopulator.class);

  private static final String POPULATOR_ERROR = "Error while populating data for ESP Event";

  /**
     * Populate the OrderConfirmationRequest instance with values from the OrderModel.
     *
     * @param order the source object
     * @param orderConfirmationEventRequest the target to fill
     * @throws ConversionException if an error occurs
     */
    @Override
    public void populate(final OrderModel order, final OrderConfirmationEventRequest orderConfirmationEventRequest) throws ConversionException {
        Assert.notNull(order, "Parameter order cannot be null.");
        Assert.notNull(orderConfirmationEventRequest, "Parameter orderConfirmationEventRequest cannot be null.");

        final UserModel userModel = order.getUser();
        if(Objects.nonNull(userModel)) {
            orderConfirmationEventRequest.setContactKey(getRequestValue(userModel.getUid()));
        }
            orderConfirmationEventRequest
                .setEventDefinitionKey(getRequestValue(getConfigurationService().getConfiguration().
                getString(BlCoreConstants.ORDER_CONFIRMATION_EVENT_DEFINITION_KEY)));
        populateOrderData(order, orderConfirmationEventRequest);

    }

  /**
   * This method populate order data from order model
   * @param orderModel orderodel
   * @param orderConfirmationEventRequest request to be get updated
   */
    private void populateOrderData(final OrderModel orderModel, final OrderConfirmationEventRequest orderConfirmationEventRequest) {

      final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
      final OrderConfirmationData data = new OrderConfirmationData();
       populateCommonData(orderModel , data);
       data.setOldorderid(StringUtils.EMPTY);
       data.setTemplate(getRequestValue(getConfigurationService().getConfiguration().getString(BlCoreConstants.ORDER_CONFIRMATION_EVENT_TEMPLATE)));
       final UserModel userModel = orderModel.getUser();
        if (Objects.nonNull(userModel)) {
            data.setCustomername(getRequestValue(userModel.getName()));
        }
        data.setType(getOrderType(orderModel));
        data.setReplacement(BooleanUtils.isTrue(orderModel.getIsCartUsedForReplacementOrder())
            ? Boolean.TRUE.toString() : Boolean.FALSE.toString());
        data.setStatus(getRequestValue(getOrderStatus(orderModel)));
        data.setDateplaced(formatter.format(orderModel.getDate()));
        if(Objects.nonNull(orderModel.getDeliveryMode())) {
          final ZoneDeliveryModeModel delivery = ((ZoneDeliveryModeModel) orderModel
              .getDeliveryMode());
          data.setShippingmethodtype(getRequestValue(delivery.getShippingGroup().getName()));
          data.setShippingmethod(getRequestValue(delivery.getCode()));
          data.setShippingmethodtext(getRequestValue(delivery.getName()));
        }
        data.setTrackinginfo(StringUtils.EMPTY);
        data.setItemcost(getDoubleValueForRequest(orderModel.getTotalPrice()));
        data.setDamagewaivercost(getDoubleValueForRequest(orderModel.getTotalDamageWaiverCost()));
        data.setSubtotal(getDoubleValueForRequest(orderModel.getSubtotal()));
        data.setShippingamount(getDoubleValueForRequest(orderModel.getDeliveryCost()));
        data.setTaxamount(getDoubleValueForRequest(orderModel.getTotalTax()));
        data.setDiscountamount(getDoubleValueForRequest(orderModel.getTotalDiscounts()));
        data.setTotalcost(getDoubleValueForRequest(orderModel.getTotalPrice()));
        data.setDiscounttext(StringUtils.EMPTY);
        if(BooleanUtils.isTrue(orderModel.getIsRentalCart()) && BooleanUtils.isFalse(
            orderModel.isGiftCardOrder())) {
          data.setExpectedshippingdate(formatter.format(orderModel.getRentalStartDate()));
          data.setArrivaldate(formatter.format(orderModel.getRentalStartDate()));
          data.setReturndate(formatter.format(orderModel.getRentalEndDate()));
          data.setActualreturndate(formatter.format(orderModel.getRentalEndDate()));
          data.setRentalduration((int) getRentalDuration(orderModel));
        }
        if (Objects.nonNull(orderModel.getPaymentInfo())) {
            final BrainTreePaymentInfoModel brainTreePaymentInfoModel = (BrainTreePaymentInfoModel) orderModel.getPaymentInfo();
            data.setPaymenttype(StringUtils.equalsIgnoreCase(BlCoreConstants.PAY_PAL_PROVIDER,brainTreePaymentInfoModel.getPaymentProvider())
                ? BlCoreConstants.PAY_PAL : brainTreePaymentInfoModel.getPaymentProvider());
        }
        if(StringUtils.isNotBlank(orderModel.getPoNumber())){
          data.setPaymenttype(BlCoreConstants.PO);
        }
        data.setPaymenttext(StringUtils.EMPTY);
        data.setExtensiontotal(0.0);
        data.setVerificationlevel(orderModel.getVerificationLevel());
        populateXMLData(orderModel, data);
        orderConfirmationEventRequest.setData(data);
    }

  /**
   * This method created to get order status from order model
   * @param orderModel orderModel
   * @return String
   */
  private String getOrderStatus(final OrderModel orderModel) {
    return Objects.isNull(orderModel.getStatus()) ? StringUtils.EMPTY : orderModel.getStatus().getCode();
  }


  /**
   * This method populate the shiiping , billing and order info details
   * @param orderModel ordermodel
   * @param data data to be set
   */
    private void populateXMLData(final OrderModel orderModel, final OrderConfirmationData data) {
        // Populate Shipping Info In XML
        populateShippingInfoInXML(orderModel, data);
        // Populate Billing Info In XML
        populateBillingInfoInXML(orderModel, data);
        // Populate Order Items In XML
        populateOrderItemsInXML(orderModel, data);

    }

  /**
   * This method populate shipping info into xml format
   * @param orderModel ordermodel
   * @param data data to be set
   */
    private void populateShippingInfoInXML(final OrderModel orderModel, final OrderConfirmationData data) {
        if (Objects.nonNull(orderModel.getDeliveryAddress())) {
            final AddressModel shippingAddress = orderModel.getDeliveryAddress();
            try {
                final Document shippingInfoInXMLDocument = createNewXMLDocument();
                final Element root = createRootElementForDocument(shippingInfoInXMLDocument, BlCoreConstants.SHIPPING_ROOT_ELEMENT);
                createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_FIRST_NAME, getRequestValue(shippingAddress.getFirstname()));
                createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_LAST_NAME, getRequestValue(shippingAddress.getLastname()));
                createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_ORGANIZATION, getRequestValue(shippingAddress.getCompany()));
                createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_ADDRESS_1, getRequestValue(shippingAddress.getLine1()));
                createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_ADDRESS_2, getRequestValue(shippingAddress.getLine2()));
                createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_CITY, getRequestValue(shippingAddress.getTown()));
                createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_STATE,
                    Objects.nonNull(shippingAddress.getRegion()) ? shippingAddress.getRegion().getName() : StringUtils.EMPTY);
                createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_ZIP_CODE, getRequestValue(shippingAddress.getPostalcode()));
                createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_PHONE, getRequestValue(shippingAddress.getCellphone()));
                createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_EMAIL, getRequestValue(shippingAddress.getEmail()));
              if(StringUtils.isNotEmpty(orderModel.getPickUpPersonEmail())){
                createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_HOURS, getStoreOpeningHours(shippingAddress));
              }
                createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_NOTES, StringUtils.isNotBlank(orderModel.getDeliveryNotes())  ? orderModel.getDeliveryNotes() : StringUtils.EMPTY);

              final Transformer transformer = getTransformerFactoryObject();
              final StringWriter writer = new StringWriter();

                //transform document to string
                transformer.transform(new DOMSource(shippingInfoInXMLDocument), new StreamResult(writer));
                data.setShippinginfo(writer.getBuffer().toString());

            } catch (final Exception exception) {
              BlLogger.logMessage(LOG , Level.ERROR , POPULATOR_ERROR , exception);
              throw new BlESPIntegrationException(exception.getMessage() , LogErrorCodeEnum.ESP_EVENT_POPULATOR_EXCEPTION.getCode() , exception);
            }
        }
    }

  /**
   * It returns opening hours of a store.
   *
   * @param shippingAddress the AddressModel
   * @return opening hours
   */
  private String getStoreOpeningHours(final AddressModel shippingAddress) {
    final Map<String, String> openingDaysDetails = shippingAddress.getOpeningDaysDetails();
    final StringBuilder stringBuilder = new StringBuilder();
    if (MapUtils.isNotEmpty(openingDaysDetails)) {
      openingDaysDetails.forEach(
          (key, value) -> stringBuilder.append(key).append(BlCoreConstants.COLON).append(value)
              .append(StringUtils.SPACE));
    }
    return stringBuilder.toString();
  }


  /**
   * This method created to populate billing info into xml format
   * @param orderModel ordermodel
   * @param data data to be set
   */
    private void populateBillingInfoInXML(final OrderModel orderModel, final OrderConfirmationData data) {
        if (Objects.nonNull(orderModel.getPaymentInfo()) && Objects.nonNull(orderModel.getPaymentInfo().getBillingAddress())) {
            final AddressModel billingAddress = orderModel.getPaymentInfo().getBillingAddress();
            try {
                final Document billingInfoInXMLDocument = createNewXMLDocument();
                final Element root = createRootElementForDocument(billingInfoInXMLDocument, BlCoreConstants.BILLING_ROOT_ELEMENT);
                createElementForRootElement(billingInfoInXMLDocument, root, BlCoreConstants.BILLING_FIRST_NAME,
                    getRequestValue(billingAddress.getFirstname()));
                createElementForRootElement(billingInfoInXMLDocument, root, BlCoreConstants.BILLING_LAST_NAME,
                    getRequestValue(billingAddress.getLastname()));
                createElementForRootElement(billingInfoInXMLDocument, root, BlCoreConstants.BILLING_ORGANIZATION,
                    getRequestValue(billingAddress.getCompany()));
                createElementForRootElement(billingInfoInXMLDocument, root, BlCoreConstants.BILLING_ADDRESS_1,
                    getRequestValue(billingAddress.getLine1()));
                createElementForRootElement(billingInfoInXMLDocument, root, BlCoreConstants.BILLING_ADDRESS_2,
                    getRequestValue(billingAddress.getLine2()));
                createElementForRootElement(billingInfoInXMLDocument, root, BlCoreConstants.BILLING_CITY,
                    getRequestValue(billingAddress.getTown()));
                createElementForRootElement(billingInfoInXMLDocument, root, BlCoreConstants.BILLING_STATE,
                    Objects.nonNull(billingAddress.getRegion()) ? billingAddress.getRegion().getName() : StringUtils.EMPTY);
                createElementForRootElement(billingInfoInXMLDocument, root, BlCoreConstants.BILLING_ZIP_CODE,
                    getRequestValue(billingAddress.getPostalcode()));
                createElementForRootElement(billingInfoInXMLDocument, root, BlCoreConstants.BILLING_PHONE,
                    getRequestValue(billingAddress.getCellphone()));
                createElementForRootElement(billingInfoInXMLDocument, root, BlCoreConstants.BILLING_EMAIL,
                    getRequestValue(billingAddress.getEmail()));
                createElementForRootElement(billingInfoInXMLDocument, root, BlCoreConstants.BILLING_NOTES,getOrderNotesFromOrderModel(orderModel));
                createElementForRootElement(billingInfoInXMLDocument, root, BlCoreConstants.BILLING_GIFT_CARD_USED,
                    String.valueOf(Objects.isNull(orderModel.getGiftCardAmount()) ? 0.0: orderModel.getGiftCardAmount()));
                createElementForRootElement(billingInfoInXMLDocument, root, BlCoreConstants.BILLING_GIFT_CARD_BALANCE, getGiftCardBalance(orderModel));

              final Transformer transformer = getTransformerFactoryObject();
              final StringWriter writer = new StringWriter();

                //transform document to string
                transformer.transform(new DOMSource(billingInfoInXMLDocument), new StreamResult(writer));
                data.setBillinginfo(writer.getBuffer().toString());

            } catch (final Exception exception) {
              BlLogger.logMessage(LOG , Level.ERROR , POPULATOR_ERROR , exception);
              throw new BlESPIntegrationException(exception.getMessage() , LogErrorCodeEnum.ESP_EVENT_POPULATOR_EXCEPTION.getCode() , exception);
            }
        }
    }


  /**
   * This method created to populate the order line items into xml format
   * @param orderModel ordermodel
   * @param data data to be set
   */
    private void populateOrderItemsInXML(final OrderModel orderModel, final OrderConfirmationData data) {
        try {
            final Document orderItemsInXMLDocument = createNewXMLDocument();
            final Element rootOrderItems = createRootElementForDocument(orderItemsInXMLDocument, BlCoreConstants.ORDER_ITEMS_ROOT_ELEMENT);

            if (CollectionUtils.isNotEmpty(orderModel.getEntries())) {
                for (final AbstractOrderEntryModel entryModel : orderModel.getEntries()) {
                    populateOrderDetailsInXMl(entryModel , orderItemsInXMLDocument , rootOrderItems);
                }
            }

          final Transformer transformer = getTransformerFactoryObject();
          final StringWriter writer = new StringWriter();

            //transform document to string
            transformer.transform(new DOMSource(orderItemsInXMLDocument), new StreamResult(writer));
            data.setOrderitemsinfo(writer.getBuffer().toString());

        } catch (final Exception exception) {
          BlLogger.logMessage(LOG , Level.ERROR , POPULATOR_ERROR , exception);
          throw new BlESPIntegrationException(exception.getMessage() , LogErrorCodeEnum.ESP_EVENT_POPULATOR_EXCEPTION.getCode() , exception);
        }
    }

  /**
   * This method created to populate data in XML format
   * @param entryModel entryModel
   * @param orderItemsInXMLDocument orderItemsInXMLDocument
   * @param rootOrderItems rootOrderItems
   */
    private void populateOrderDetailsInXMl(final AbstractOrderEntryModel entryModel, final Document orderItemsInXMLDocument,
        final Element rootOrderItems) {
      final Element rootOrderItem = createRootElementForRootElement(orderItemsInXMLDocument, rootOrderItems, BlCoreConstants.ORDER_ITEM_ROOT_ELEMENT);
      if (Objects.nonNull(entryModel.getProduct())) {
        createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlCoreConstants.ORDER_ITEM_PRODUCT_CODE,
            getRequestValue(entryModel.getProduct().getCode()));
        createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlCoreConstants.ORDER_ITEM_PRODUCT_TITLE,
            entryModel.getProduct() instanceof BlSerialProductModel ? getProductTitle(entryModel.getProduct().getCode()) :entryModel.getProduct().getName());
      }
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlCoreConstants.ORDER_ITEM_PRODUCT_PHOTO,
          entryModel.getProduct() instanceof BlSerialProductModel ? getSerialProductUrl(entryModel.getProduct().getCode()) : getProductURL(entryModel));
      if (Objects.nonNull(entryModel.getBasePrice())) {
        createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlCoreConstants.ORDER_ITEM_RENTAL_PRICE, String.valueOf(entryModel.getBasePrice().doubleValue()));
      }
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlCoreConstants.ORDER_ITEM_DAMAGE_WAIVER_PRICE,
          String.valueOf(getDamageWaiverPriceFromEntry(entryModel)));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlCoreConstants.ORDER_ITEM_DAMAGE_WAIVER_TEXT, getDamageWaiverName(entryModel));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlCoreConstants.ORDER_ITEM_TOTAL_PRICE,
          String.valueOf(getDoubleValueForRequest(entryModel.getTotalPrice())));
    }


}
