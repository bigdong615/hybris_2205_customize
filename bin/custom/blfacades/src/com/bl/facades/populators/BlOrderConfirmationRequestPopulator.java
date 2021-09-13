/**
 *
 */
package com.bl.facades.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.services.esp.models.OrderConfirmationRequest;
import com.bl.core.services.esp.models.OrderData;
import com.bl.facades.constants.BlFacadesConstants;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.util.Assert;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Objects;


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

        orderConfirmationRequest.setContactKey("test@gmail.com");
        orderConfirmationRequest.setEventDefinitionKey(getConfigurationService().getConfiguration().getString(BlCoreConstants.ORDER_CONFIRMATION_EVENT_DEFINITION_KEY));
        populateData(order, orderConfirmationRequest);

    }

    private void populateData(final OrderModel orderModel, final OrderConfirmationRequest orderConfirmationRequest) {
        String pattern = "yyyy-MM-dd";
        SimpleDateFormat formatter = new SimpleDateFormat(pattern);
        OrderData data = new OrderData();
        data.setOrderid(orderModel.getCode());
        data.setOldorderid("1153066");
        data.setTemplate(getConfigurationService().getConfiguration().getString(BlCoreConstants.ORDER_CONFIRMATION_EVENT_TEMPLATE));
        data.setSubscriberid("718628824577");
        if (Objects.nonNull(orderModel.getUser())) {
            data.setEmailaddress(orderModel.getUser().getUid());
            data.setCustomername(orderModel.getUser().getName());
        }
        data.setType("Rental");
        data.setReplacement("true");
        if (Objects.nonNull(orderModel.getStatus())) {
            data.setStatus(orderModel.getStatus().getCode());
        }
        Date date = orderModel.getDate();
        data.setDateplaced(formatter.format(date));
        data.setShippingmethodtype("Recieved");
        if (Objects.nonNull(orderModel.getDeliveryMode())) {
            data.setShippingmethod(orderModel.getDeliveryMode().getCode());
        }
        data.setShippingmethodtext("test");
        data.setTrackinginfo("test");
        data.setItemcost(orderModel.getTotalPrice());
        data.setDamagewaivercost(orderModel.getTotalDamageWaiverCost());
        data.setSubtotal(orderModel.getSubtotal());
        data.getShippingamount(orderModel.getDeliveryCost());
        data.setTaxamount(orderModel.getTotalTax());
        data.setDiscountamount(orderModel.getTotalDiscounts());
        data.setTotalcost(orderModel.getTotalPrice());
        data.setDiscounttext("test");
        data.setExpectedshippingdate("test");
        data.setArrivaldate("test");
        data.setReturndate("test");
        data.setActualreturndate("test");
        data.setRentalduration(2);
        data.setVerificationlevel(1);
        if (Objects.nonNull(orderModel.getPaymentMode())) {
            data.setPaymenttype(orderModel.getPaymentMode().getCode());
        }
        data.setPaymenttext("test");
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
                Element root = createRootElementForDocument(shippingInfoInXMLDocument, BlFacadesConstants.SHIPPING_ROOT_ELEMENT);
                createElementForRootElement(shippingInfoInXMLDocument, root, BlFacadesConstants.SHIPPING_FIRST_NAME, shippingAddress.getFirstname());
                createElementForRootElement(shippingInfoInXMLDocument, root, BlFacadesConstants.SHIPPING_LAST_NAME, shippingAddress.getLastname());
                createElementForRootElement(shippingInfoInXMLDocument, root, BlFacadesConstants.SHIPPING_ORGANIZATION, shippingAddress.getCompany());
                createElementForRootElement(shippingInfoInXMLDocument, root, BlFacadesConstants.SHIPPING_ADDRESS_1, shippingAddress.getLine1());
                createElementForRootElement(shippingInfoInXMLDocument, root, BlFacadesConstants.SHIPPING_ADDRESS_2, shippingAddress.getLine2());
                createElementForRootElement(shippingInfoInXMLDocument, root, BlFacadesConstants.SHIPPING_CITY, shippingAddress.getTown());
                createElementForRootElement(shippingInfoInXMLDocument, root, BlFacadesConstants.SHIPPING_STATE, "AK");
                createElementForRootElement(shippingInfoInXMLDocument, root, BlFacadesConstants.SHIPPING_ZIP_CODE, shippingAddress.getPostalcode());
                createElementForRootElement(shippingInfoInXMLDocument, root, BlFacadesConstants.SHIPPING_PHONE, shippingAddress.getCellphone());
                createElementForRootElement(shippingInfoInXMLDocument, root, BlFacadesConstants.SHIPPING_EMAIL, shippingAddress.getEmail());
                createElementForRootElement(shippingInfoInXMLDocument, root, BlFacadesConstants.SHIPPING_HOURS, "Mon-Fri: 8:00 AM - 6:00 PM Sat: 10:00 AM - 5:00 PM Sun: Closed");
                createElementForRootElement(shippingInfoInXMLDocument, root, BlFacadesConstants.SHIPPING_NOTES, "In the Safeway Shopping Center");

                TransformerFactory tf = TransformerFactory.newInstance();
                Transformer transformer;
                transformer = tf.newTransformer();
                transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
                StringWriter writer = new StringWriter();

                //transform document to string
                transformer.transform(new DOMSource(shippingInfoInXMLDocument), new StreamResult(writer));
                data.setShippinginfo(writer.getBuffer().toString());

            } catch (ParserConfigurationException e) {
                e.printStackTrace();
            } catch (TransformerException e) {
                e.printStackTrace();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    private void populateBillingInfoInXML(final OrderModel orderModel, final OrderData data) {
        if (Objects.nonNull(orderModel.getPaymentInfo()) && Objects.nonNull(orderModel.getPaymentInfo().getBillingAddress())) {
            AddressModel billingAddress = orderModel.getPaymentInfo().getBillingAddress();
            try {
                Document billingInfoInXMLDocument = createNewXMLDocument();
                Element root = createRootElementForDocument(billingInfoInXMLDocument, BlFacadesConstants.BILLING_ROOT_ELEMENT);
                createElementForRootElement(billingInfoInXMLDocument, root, BlFacadesConstants.BILLING_FIRST_NAME, billingAddress.getFirstname());
                createElementForRootElement(billingInfoInXMLDocument, root, BlFacadesConstants.BILLING_LAST_NAME, billingAddress.getLastname());
                createElementForRootElement(billingInfoInXMLDocument, root, BlFacadesConstants.BILLING_ORGANIZATION, billingAddress.getCompany());
                createElementForRootElement(billingInfoInXMLDocument, root, BlFacadesConstants.BILLING_ADDRESS_1, billingAddress.getLine1());
                createElementForRootElement(billingInfoInXMLDocument, root, BlFacadesConstants.BILLING_ADDRESS_2, billingAddress.getLine2());
                createElementForRootElement(billingInfoInXMLDocument, root, BlFacadesConstants.BILLING_CITY, billingAddress.getTown());
                createElementForRootElement(billingInfoInXMLDocument, root, BlFacadesConstants.BILLING_STATE, "AK");
                createElementForRootElement(billingInfoInXMLDocument, root, BlFacadesConstants.BILLING_ZIP_CODE, billingAddress.getPostalcode());
                createElementForRootElement(billingInfoInXMLDocument, root, BlFacadesConstants.BILLING_PHONE, billingAddress.getCellphone());
                createElementForRootElement(billingInfoInXMLDocument, root, BlFacadesConstants.BILLING_EMAIL, billingAddress.getEmail());
                createElementForRootElement(billingInfoInXMLDocument, root, BlFacadesConstants.BILLING_NOTES, "Test");
                createElementForRootElement(billingInfoInXMLDocument, root, BlFacadesConstants.BILLING_GIFT_CARD_USED, "test");
                createElementForRootElement(billingInfoInXMLDocument, root, BlFacadesConstants.BILLING_GIFT_CARD_BALANCE, "test");

                TransformerFactory tf = TransformerFactory.newInstance();
                Transformer transformer;
                transformer = tf.newTransformer();
                transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
                StringWriter writer = new StringWriter();

                //transform document to string
                transformer.transform(new DOMSource(billingInfoInXMLDocument), new StreamResult(writer));
                data.setBillinginfo(writer.getBuffer().toString());

            } catch (ParserConfigurationException e) {
                e.printStackTrace();
            } catch (TransformerException e) {
                e.printStackTrace();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }


    private void populateOrderItemsInXML(final OrderModel orderModel, final OrderData data) {
        try {
            Document orderItemsInXMLDocument = createNewXMLDocument();
            Element rootOrderItems = createRootElementForDocument(orderItemsInXMLDocument, BlFacadesConstants.ORDER_ITEMS_ROOT_ELEMENT);

            if (CollectionUtils.isNotEmpty(orderModel.getEntries())) {
                for (AbstractOrderEntryModel entryModel : orderModel.getEntries()) {
                    Element rootOrderItem = createRootElementForRootElement(orderItemsInXMLDocument, rootOrderItems, BlFacadesConstants.ORDER_ITEM_ROOT_ELEMENT);
                    if (Objects.nonNull(entryModel.getProduct())) {
                        createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlFacadesConstants.ORDER_ITEM_PRODUCT_CODE, entryModel.getProduct().getCode());
                        createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlFacadesConstants.ORDER_ITEM_PRODUCT_TITLE, entryModel.getProduct().getName());
                    }
                    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlFacadesConstants.ORDER_ITEM_PRODUCT_PHOTO, "Test");
                    if (Objects.nonNull(entryModel.getBasePrice())) {
                        createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlFacadesConstants.ORDER_ITEM_RENTAL_PRICE, String.valueOf(entryModel.getBasePrice().doubleValue()));
                    }
                    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlFacadesConstants.ORDER_ITEM_DAMAGE_WAIVER_PRICE, "Test");
                    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlFacadesConstants.ORDER_ITEM_DAMAGE_WAIVER_TEXT, "Test");
                    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlFacadesConstants.ORDER_ITEM_TOTAL_PRICE, String.valueOf(entryModel.getTotalPrice()));
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

        } catch (ParserConfigurationException e) {
            e.printStackTrace();
        } catch (TransformerException e) {
            e.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private Document createNewXMLDocument() throws ParserConfigurationException {
        DocumentBuilderFactory documentFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder documentBuilder = documentFactory.newDocumentBuilder();
        Document document = documentBuilder.newDocument();
        return document;
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

    public ConfigurationService getConfigurationService() {
        return configurationService;
    }

    public void setConfigurationService(ConfigurationService configurationService) {
        this.configurationService = configurationService;
    }
}
