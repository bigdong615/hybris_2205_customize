/**
 *
 */
package com.bl.core.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.GearGaurdEnum;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.esp.dto.OrderData;
import com.bl.esp.dto.orderconfirmation.OrderConfirmationRequest;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.google.common.util.concurrent.AtomicDouble;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.Assert;
import org.w3c.dom.Document;
import org.w3c.dom.Element;


/**
 * BlOrderConfirmationRequestPopulator
 *
 *
 */
public class BlOrderConfirmationRequestPopulator implements Populator<OrderModel, OrderConfirmationRequest> {

    private ConfigurationService configurationService;

    /**
     * Populate the OrderConfirmationRequest instance with values from the OrderModel.
     *
     * @param order        the source object
     * @param orderConfirmationRequest the target to fill
     * @throws ConversionException if an error occurs
     */
    @Override
    public void populate(final OrderModel order, final OrderConfirmationRequest orderConfirmationRequest) throws ConversionException {
        Assert.notNull(order, "Parameter emailId cannot be null.");
        Assert.notNull(orderConfirmationRequest, "Parameter contactRequest cannot be null.");

        final UserModel userModel = order.getUser();
        if(Objects.nonNull(userModel)) {
            orderConfirmationRequest.setContactKey(getRequestValue(userModel.getUid()));
        }
            orderConfirmationRequest.setEventDefinitionKey(getRequestValue(getConfigurationService().getConfiguration().
                getString(BlCoreConstants.ORDER_CONFIRMATION_EVENT_DEFINITION_KEY)));
        populateData(order, orderConfirmationRequest);

    }

    private void populateData(final OrderModel orderModel, final OrderConfirmationRequest orderConfirmationRequest) {
        final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
       final OrderData data = new OrderData();
        data.setOrderid(getRequestValue(orderModel.getCode()));
        data.setOldorderid(getRequestValue(orderModel.getCode()));
        data.setTemplate(getRequestValue(getConfigurationService().getConfiguration().getString(BlCoreConstants.ORDER_CONFIRMATION_EVENT_TEMPLATE)));
       final UserModel userModel = orderModel.getUser();
        if (Objects.nonNull(userModel)) {
            data.setSubscriberid(getRequestValue(userModel.getUid()));
            data.setEmailaddress(getRequestValue(userModel.getUid()));
            data.setCustomername(getRequestValue(userModel.getName()));
        }
        data.setType(BooleanUtils.isTrue(orderModel.getIsRentalCart()) ? BlCoreConstants.RENTAL : BlCoreConstants.USED_GEAR);
        data.setReplacement(BooleanUtils.isTrue(orderModel.getIsCartUsedForReplacementOrder()) ? Boolean.TRUE.toString() : Boolean.FALSE.toString()); // is order is replacement
        data.setStatus(getRequestValue(Objects.nonNull(orderModel.getStatus()) ? orderModel.getStatus().getCode() : StringUtils.EMPTY));
        data.setDateplaced(formatter.format(orderModel.getDate()));
        if(Objects.nonNull(orderModel.getDeliveryMode())) {
          final ZoneDeliveryModeModel delivery = ((ZoneDeliveryModeModel) orderModel
              .getDeliveryMode());
          data.setShippingmethodtype(getRequestValue(delivery.getShippingGroup().getName()));
          data.setShippingmethod(getRequestValue(delivery.getCarrier().getCode())); // Needs to update this value , throwing max limit exception
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
        data.setExpectedshippingdate(formatter.format(orderModel.getActualRentalStartDate()));
        data.setArrivaldate(formatter.format(orderModel.getRentalStartDate()));
        data.setReturndate(formatter.format(orderModel.getRentalEndDate()));
        data.setActualreturndate(formatter.format(orderModel.getActualRentalEndDate()));

        final long defaultAddedTimeForExtendRental = BlDateTimeUtils
            .getDaysBetweenDates(orderModel.getRentalStartDate(), orderModel.getRentalEndDate());


        data.setRentalduration(Objects.nonNull(defaultAddedTimeForExtendRental) ? (int) defaultAddedTimeForExtendRental : 0);
        data.setVerificationlevel(1);
        if (Objects.nonNull(orderModel.getPaymentInfo())) {
            BrainTreePaymentInfoModel brainTreePaymentInfoModel = (BrainTreePaymentInfoModel) orderModel.getPaymentInfo();
            data.setPaymenttype(getRequestValue(brainTreePaymentInfoModel.getPaymentProvider()));
        }
        data.setPaymenttext(StringUtils.EMPTY);
        data.setExtensiontotal(0.0);
        populateXMLData(orderModel, data);
        orderConfirmationRequest.setData(data);
    }

    private void populateXMLData(final OrderModel orderModel, final OrderData data) {
        // Populate Shipping Info In XML
        populateShippingInfoInXML(orderModel, data);
        // Populate Billing Info In XML
        populateBillingInfoInXML(orderModel, data);
        // Populate Order Items In XML
        populateOrderItemsInXML(orderModel, data);

    }

    private void populateShippingInfoInXML(final OrderModel orderModel, final OrderData data) {
        if (Objects.nonNull(orderModel.getDeliveryAddress())) {
            AddressModel shippingAddress = orderModel.getDeliveryAddress();
            try {
                Document shippingInfoInXMLDocument = createNewXMLDocument();
                Element root = createRootElementForDocument(shippingInfoInXMLDocument, BlCoreConstants.SHIPPING_ROOT_ELEMENT);
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
                createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_HOURS, "Mon-Fri: 8:00 AM - 6:00 PM Sat: 10:00 AM - 5:00 PM Sun: Closed");
                createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_NOTES, "In the Safeway Shopping Center");

                TransformerFactory tf = TransformerFactory.newInstance();
                Transformer transformer;
                transformer = tf.newTransformer();
                StringWriter writer = new StringWriter();

                //transform document to string
                transformer.transform(new DOMSource(shippingInfoInXMLDocument), new StreamResult(writer));
                data.setShippinginfo(writer.getBuffer().toString());

            } catch (final ParserConfigurationException| TransformerException exception) {
                exception.printStackTrace();
            } catch (final Exception exception) {
                exception.printStackTrace();
            }
        }
    }

    private void populateBillingInfoInXML(final OrderModel orderModel, final OrderData data) {
        if (Objects.nonNull(orderModel.getPaymentInfo()) && Objects.nonNull(orderModel.getPaymentInfo().getBillingAddress())) {
            AddressModel billingAddress = orderModel.getPaymentInfo().getBillingAddress();
            try {
                Document billingInfoInXMLDocument = createNewXMLDocument();
                Element root = createRootElementForDocument(billingInfoInXMLDocument, BlCoreConstants.BILLING_ROOT_ELEMENT);
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
                    String.valueOf(getDoubleValueForRequest(orderModel.getGiftCardAmount())));
                createElementForRootElement(billingInfoInXMLDocument, root, BlCoreConstants.BILLING_GIFT_CARD_BALANCE, getGiftCardBalance(orderModel));

                TransformerFactory tf = TransformerFactory.newInstance();
                Transformer transformer;
                transformer = tf.newTransformer();
                StringWriter writer = new StringWriter();

                //transform document to string
                transformer.transform(new DOMSource(billingInfoInXMLDocument), new StreamResult(writer));
                data.setBillinginfo(writer.getBuffer().toString());

            } catch (final ParserConfigurationException|TransformerException exception) {
                exception.printStackTrace();
            } catch (final Exception exception) {
                exception.printStackTrace();
            }
        }
    }


    private void populateOrderItemsInXML(final OrderModel orderModel, final OrderData data) {
        try {
            Document orderItemsInXMLDocument = createNewXMLDocument();
            Element rootOrderItems = createRootElementForDocument(orderItemsInXMLDocument, BlCoreConstants.ORDER_ITEMS_ROOT_ELEMENT);

            if (CollectionUtils.isNotEmpty(orderModel.getEntries())) {
                for (AbstractOrderEntryModel entryModel : orderModel.getEntries()) {
                    Element rootOrderItem = createRootElementForRootElement(orderItemsInXMLDocument, rootOrderItems, BlCoreConstants.ORDER_ITEM_ROOT_ELEMENT);
                    if (Objects.nonNull(entryModel.getProduct())) {
                        createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlCoreConstants.ORDER_ITEM_PRODUCT_CODE,
                            getRequestValue(entryModel.getProduct().getCode()));
                        createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlCoreConstants.ORDER_ITEM_PRODUCT_TITLE,
                            getRequestValue(entryModel.getProduct().getName()));
                    }
                    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlCoreConstants.ORDER_ITEM_PRODUCT_PHOTO, StringUtils.EMPTY);
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

            TransformerFactory tf = TransformerFactory.newInstance();
            Transformer transformer;
            transformer = tf.newTransformer();
            transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
            StringWriter writer = new StringWriter();

            //transform document to string
            transformer.transform(new DOMSource(orderItemsInXMLDocument), new StreamResult(writer));
            data.setOrderitemsinfo(writer.getBuffer().toString());

        } catch (final ParserConfigurationException|TransformerException exception) {
            exception.printStackTrace();
        } catch (final Exception exception) {
            exception.printStackTrace();
        }
    }

    /**
     * To get the request value based
     * @param value value get from order
     * @return value to set on request
     */
    private String getRequestValue(final String value){
        return StringUtils.isNotBlank(value) ? value : StringUtils.EMPTY;
    }


    /**
     * To get the double value for request
     * @param value value get from order
     * @return value to be set on request
     */
    private Double getDoubleValueForRequest(final Double value) {
        return value.compareTo(0.0) > 0 ? value : 0.0;
    }

    private Document createNewXMLDocument() throws ParserConfigurationException {
        DocumentBuilderFactory documentFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder documentBuilder = documentFactory.newDocumentBuilder();
        return documentBuilder.newDocument();
    }

    private Element createRootElementForDocument(Document document, String rootElement) {
        Element root = document.createElement(rootElement);
        document.appendChild(root);
        return root;
    }

    private Element createElementForRootElement(Document document, Element rootElement, String element, String value) {
        Element childElement = document.createElement(element);
        childElement.appendChild(document.createTextNode(value));
        rootElement.appendChild(childElement);
        return childElement;
    }

    private Element createRootElementForRootElement(Document document, Element rootElement, String rootElementName) {
        Element childElement = document.createElement(rootElementName);
        rootElement.appendChild(childElement);
        return childElement;
    }

    /**
     * This method is to get the order notes from order model
     * @param orderModel ordermodel
     * @return values to set on request
     */
    private String getOrderNotesFromOrderModel(final OrderModel orderModel) {
     final StringBuilder orderNotes = new StringBuilder();

        if(CollectionUtils.isNotEmpty(orderModel.getOrderNotes())){
         orderModel.getOrderNotes().forEach(notesModel -> {
             if(BlCoreConstants.CUSTOMER_CHECKOUT_ORDER_NOTES.equalsIgnoreCase(notesModel.getType().getCode())) {
                 orderNotes.append(StringUtils.EMPTY + notesModel.getNote());
             }
         });

        }
        return orderNotes.toString();
    }


    /**
     * This method is to get the gift card details from order model
     * @param orderModel ordermodel
     * @return values to set on request
     */
    private String getGiftCardBalance(final OrderModel orderModel) {
       final AtomicReference<String> giftCardBalance = new AtomicReference<>(String.valueOf(0.0));
        if (CollectionUtils.isNotEmpty(orderModel.getGiftCard())) {
            orderModel.getGiftCard().forEach(giftCardModel -> giftCardModel.getMovements().forEach(giftCardMovementModel -> {
                if(StringUtils.equals(orderModel.getCode() , giftCardMovementModel.getOrder().getCode())) {
                    giftCardBalance.set(String.valueOf(giftCardMovementModel.getBalanceAmount()));
                }
            }));
        }
        return giftCardBalance.get();
    }

    /**
     * This method is to get the damage waiver price  from order  entry model
     * @param abstractOrderEntryModel AbstractOrderEntryModel
     * @return values to set on request
     */
    private Double getDamageWaiverPriceFromEntry(final AbstractOrderEntryModel abstractOrderEntryModel) {
        final AtomicDouble damageWaiverPrice = new AtomicDouble(0.0);

        if(BooleanUtils.isTrue(abstractOrderEntryModel.getGearGuardWaiverSelected())) {
            damageWaiverPrice.set(abstractOrderEntryModel.getGearGuardWaiverPrice());
        }
        else if(BooleanUtils.isTrue(abstractOrderEntryModel.getGearGuardProFullWaiverSelected())){
            damageWaiverPrice.set(abstractOrderEntryModel.getGearGuardWaiverPrice());
        }
        else if(BooleanUtils.isTrue(abstractOrderEntryModel.getNoDamageWaiverSelected())){
            damageWaiverPrice.set(0.0);
        }

        return damageWaiverPrice.get();
    }


    /**
     * This method is to get the damage waiver text  from order  entry model
     * @param abstractOrderEntryModel AbstractOrderEntryModel
     * @return values to set on request
     */
    private String getDamageWaiverName(final AbstractOrderEntryModel abstractOrderEntryModel) {
        final AtomicReference<String> damageWaiverText = new AtomicReference<>(StringUtils.EMPTY);

        if(BooleanUtils.isTrue(abstractOrderEntryModel.getGearGuardWaiverSelected())) {
            damageWaiverText.set(GearGaurdEnum.GEAR_GAURD.getCode());
        }
        else if(BooleanUtils.isTrue(abstractOrderEntryModel.getGearGuardProFullWaiverSelected())){
            damageWaiverText.set(GearGaurdEnum.GEAR_GAURD_PRO.getCode());
        }
        else if(BooleanUtils.isTrue(abstractOrderEntryModel.getNoDamageWaiverSelected())){
            damageWaiverText.set(GearGaurdEnum.NONE.getCode());
        }

        return damageWaiverText.get();
    }


    public ConfigurationService getConfigurationService() {
        return configurationService;
    }

    public void setConfigurationService(ConfigurationService configurationService) {
        this.configurationService = configurationService;
    }
}
