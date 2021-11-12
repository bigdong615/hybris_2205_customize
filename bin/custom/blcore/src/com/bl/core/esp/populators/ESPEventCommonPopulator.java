/**
 *
 */
package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.GearGaurdEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.esp.dto.orderpullback.data.OrderPullBackItems;
import com.bl.esp.exception.BlESPIntegrationException;
import com.bl.esp.order.ESPEventCommonOrderDataRequest;
import com.bl.esp.order.ESPEventCommonRequest;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import com.google.common.util.concurrent.AtomicDouble;
import de.hybris.platform.catalog.CatalogVersionService;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.w3c.dom.Document;
import org.w3c.dom.Element;


/**
 * Abstract class for ESPEvent requests. Conversion methods should be implemented in inheriting
 * class.
 * @author Manikandan
 */
public abstract class ESPEventCommonPopulator<SOURCE extends AbstractOrderModel, TARGET extends ESPEventCommonRequest> implements
    Populator<SOURCE, TARGET> {

    private ConfigurationService configurationService;
    private ProductService productService;
    private CatalogVersionService catalogVersionService;


    private static final String POPULATOR_ERROR = "Error while populating data for ESP Event";

    private static final List<String> LIST_OF_OC_LOCATIONS = Arrays.asList(BlCoreConstants.FEDEX, BlCoreConstants.UPS,BlCoreConstants.USPS);


    private static final Logger LOG = Logger.getLogger(ESPEventCommonPopulator.class);



    /**
     * Populate common attributes with values from the OrderModel.
     *
     * @param orderModel            the source object
     * @param espEventCommonRequest the target to fill
     */
    protected void populateCommonData(final AbstractOrderModel orderModel,
        final ESPEventCommonOrderDataRequest espEventCommonRequest) {

        espEventCommonRequest.setOrderid(orderModel.getCode());
        if (Objects.nonNull(orderModel.getUser())) {
            espEventCommonRequest.setEmailaddress(orderModel.getUser().getUid());
            espEventCommonRequest.setSubscriberid(orderModel.getUser().getUid());
        }
    }

    /**
     * Populate rental duration with values from the OrderModel.
     *
     * @param orderModel the source object
     */
    protected long getRentalDuration(final AbstractOrderModel orderModel) {
        return BlDateTimeUtils
            .getDaysBetweenDates(orderModel.getRentalStartDate(), orderModel.getRentalEndDate());
    }


    /**
     * To get the request value based
     * @param value value get from order
     * @return value to set on request
     */
    protected String getRequestValue(final String value){
        return StringUtils.isBlank(value) ? StringUtils.EMPTY :value;
    }

    /**
     * To get the double value for request
     * @param value value get from order
     * @return value to be set on request
     */
    protected Double getDoubleValueForRequest(final Double value) {
        return value.compareTo(0.0) > 0 ? value : 0.0;
    }

    /**
     * This method created to get transformer factory object
     * @return transformer
     * @throws TransformerConfigurationException TransformerConfigurationException
     */
    protected Transformer getTransformerFactoryObject() throws TransformerConfigurationException {
        final TransformerFactory transformerFactory = TransformerFactory.newInstance();
        return transformerFactory.newTransformer();
    }

    /**
     * This method created to populate data
     * @return data which converted
     * @throws ParserConfigurationException parserConfigurationException
     */
    protected Document createNewXMLDocument() throws ParserConfigurationException {
        final DocumentBuilderFactory documentFactory = DocumentBuilderFactory.newInstance();
        final DocumentBuilder documentBuilder = documentFactory.newDocumentBuilder();
        return documentBuilder.newDocument();
    }

    /**
     * This method created to add the root element
     * @param document document to be add
     * @param rootElement root element to be set
     * @return element which append
     */
    protected Element createRootElementForDocument(final Document document, final String rootElement) {
        final Element root = document.createElement(rootElement);
        document.appendChild(root);
        return root;
    }

    /**
     * This method created to add the root element
     * @param document document to be add
     * @param rootElement root element to be set
     * @param value value to add
     * @return element which append
     */

    protected Element createElementForRootElement(final Document document, final Element rootElement, final String element, final String value) {
        final Element childElement = document.createElement(element);
        childElement.appendChild(document.createTextNode(value));
        rootElement.appendChild(childElement);
        return childElement;
    }


    /**
     * This method created to add the root element
     * @param document document to be add
     * @param rootElement root element to be set
     * @return element which append
     */
    protected Element createRootElementForRootElement(final Document document, final Element rootElement, final String rootElementName) {
        final Element childElement = document.createElement(rootElementName);
        rootElement.appendChild(childElement);
        return childElement;
    }


    /**
     * This method is to get the order notes from order model
     * @param orderModel ordermodel
     * @return values to set on request
     */
    protected String getOrderNotesFromOrderModel(final OrderModel orderModel) {
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
    protected String getGiftCardBalance(final OrderModel orderModel) {
        final AtomicReference<String> giftCardBalance = new AtomicReference<>(String.valueOf(0.0));
        if (CollectionUtils.isNotEmpty(orderModel.getGiftCard())) {
            orderModel.getGiftCard().forEach(giftCardModel -> giftCardModel.getMovements().forEach(giftCardMovementModel -> {
                if(StringUtils.equals(orderModel.getCode() , (giftCardMovementModel.getOrder() != null ? giftCardMovementModel.getOrder().getCode() :StringUtils.EMPTY))) {
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
    protected Double getDamageWaiverPriceFromEntry(final AbstractOrderEntryModel abstractOrderEntryModel) {
        final AtomicDouble damageWaiverPrice = new AtomicDouble(0.0);
        if(BooleanUtils.isTrue(abstractOrderEntryModel.getGearGuardWaiverSelected())) {
            if(abstractOrderEntryModel.getGearGuardWaiverPrice()!=null) {
                damageWaiverPrice.set(abstractOrderEntryModel.getGearGuardWaiverPrice());
            }
        }
        else if(BooleanUtils.isTrue(abstractOrderEntryModel.getGearGuardProFullWaiverSelected())){
            if(abstractOrderEntryModel.getGearGuardWaiverPrice()!=null) {
                damageWaiverPrice.set(abstractOrderEntryModel.getGearGuardWaiverPrice());
            }
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
    protected String getDamageWaiverName(final AbstractOrderEntryModel abstractOrderEntryModel) {
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

    /**
     * This method created to get the order type
     * @param orderModel ordermodel
     * @return string
     */
    protected String getOrderType(final OrderModel orderModel){
        final AtomicReference<String> orderType = new AtomicReference<>(StringUtils.EMPTY);
        if(BooleanUtils.isTrue(orderModel.isGiftCardOrder())) {
            orderType.set(BlCoreConstants.GIFT_CARD_ORDER);
        }
        else if(BooleanUtils.isTrue(orderModel.getIsNewGearOrder())){
            orderType.set(BlCoreConstants.NEW_GEAR_ORDER);
        }
        else if(BooleanUtils.isTrue(orderModel.getIsRentalCart())){
            orderType.set(BlCoreConstants.RENTAL);
        }
        else if(BooleanUtils.isFalse(orderModel.getIsRentalCart())){
            orderType.set(BlCoreConstants.USED_GEAR);
        }

        return orderType.get();
    }
    /**
     * To check whether media is empty of not
     * @param abstractOrderEntryModel abstractOrderEntryModel
     * @return string
     */
    protected String getProductURL(final AbstractOrderEntryModel abstractOrderEntryModel){
        return Objects.nonNull(abstractOrderEntryModel.getProduct().getPicture()) &&
            StringUtils.isNotBlank(abstractOrderEntryModel.getProduct().getPicture().getURL()) ?
            abstractOrderEntryModel.getProduct().getPicture().getURL() : StringUtils.EMPTY;
    }

    /**
     * This method craeted to get the product title
     * @param serialProductCode serial code
     * @return string
     */

    protected String getProductTitle(final String serialProductCode) {
        final AtomicReference<String> productTitle = new AtomicReference<>(StringUtils.EMPTY);
        final CatalogVersionModel catalogVersion = getCatalogVersionService().getCatalogVersion(BlCoreConstants.CATALOG_VALUE,BlCoreConstants.ONLINE);
        final BlSerialProductModel blSerialProduct = (BlSerialProductModel) getProductService().getProductForCode(catalogVersion, serialProductCode);
        if(Objects.nonNull(blSerialProduct)) {
            final BlProductModel blProductModel = blSerialProduct.getBlProduct();
            if(Objects.nonNull(blProductModel)){
                productTitle.set(blProductModel.getName());
            }
        }
        return productTitle.get();
    }


    /**
     * This method created to get the serial product url for request
     * @param serialProductCode serial product code
     * @return string
     */
    protected String getProductUrl(final String serialProductCode) {
        final AtomicReference<String> productUrl = new AtomicReference<>(StringUtils.EMPTY);
        final CatalogVersionModel catalogVersion = getCatalogVersionService().getCatalogVersion(BlCoreConstants.CATALOG_VALUE,BlCoreConstants.ONLINE);
        final BlSerialProductModel blSerialProduct = (BlSerialProductModel) getProductService().getProductForCode(catalogVersion, serialProductCode);
        if(Objects.nonNull(blSerialProduct)) {
            final BlProductModel blProductModel = blSerialProduct.getBlProduct();
            if(Objects.nonNull(blProductModel)  && Objects.nonNull(blProductModel.getPicture()) &&
                StringUtils.isNotBlank(blProductModel.getPicture().getURL())){
                productUrl.set(blProductModel.getPicture().getURL());
            }
        }
        return productUrl.get();
    }
    /**
     * It returns billing charges type
     *
     * @param billingTypeList
     * @return billType
     */
    protected String getBillingTypes(final List<String> billingTypeList) {
        final StringBuilder stringBuilder = new StringBuilder();
        int count = 0;
        for (String billType : billingTypeList) {
            stringBuilder.append(billType);
            if (count != billingTypeList.size() - 1) {
                stringBuilder.append(BlCoreConstants.SHARE_A_SALE_COMMA);
            }
            count++;
        }
        return stringBuilder.toString();
    }

    /**
     * This method check Gift card payment type
     * @param orderModel
     * @return string
     */
    protected String checkIsGiftCardUsed(final OrderModel orderModel , final String creditCart){
       final StringBuilder paymentType= new StringBuilder();
        /*if(CollectionUtils.isNotEmpty (orderModel.getGiftCard())){
            return orderModel.getTotalPrice() == 0 ? paymentType.append(BlCoreConstants.GIFT_CARD_TYPE).toString() :
                paymentType.append(creditCart).append(StringUtils.SPACE).append(BlCoreConstants.PLUS).append(StringUtils.SPACE ).append(BlCoreConstants.GC_TYPE).toString();
        }*/
        return paymentType.append(creditCart).toString();
    }


    /**
     * This method created to populate order data from order model
     * @param order order model to get the data
     * @param data data to get updated
     * @param blSerialProductModels
     */
    protected void populateOrderDataForOrderPullBackItems(final OrderModel order,
        final OrderPullBackItems data, final String templateName, final List<BlSerialProductModel> blSerialProductModels) {
        final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
        data.setOldOrderId(StringUtils.EMPTY);
        data.setStatus(getRequestValue(
            Objects.nonNull(order.getStatus()) ? order.getStatus().getCode() : StringUtils.EMPTY));
        data.setOrdertype(getOrderType(order));
        data.setDateplaced(formatter.format(order.getDate()));
        if(Objects.nonNull(order.getDeliveryMode())) {
            final ZoneDeliveryModeModel delivery = ((ZoneDeliveryModeModel) order
                .getDeliveryMode());
            data.setShippingmethodtype(getRequestValue(delivery.getShippingGroup().getName()));
            data.setShippingmethod(getRequestValue(delivery.getCode()));
        }
        data.setExpectedshippingdate(formatter.format(order.getRentalStartDate()));
        data.setArrivaldate(formatter.format(order.getRentalStartDate()));
        data.setReturndate(formatter.format(order.getRentalEndDate()));
        populateOrderItemsInXML(order , data , templateName , blSerialProductModels);
    }

    /**
     * This method created to populate order
     * @param orderModel order model to get the data
     * @param data date to get updated
     * @param templateName template for request
     * @param blSerialProductModels
     */
    private void populateOrderItemsInXML(final OrderModel orderModel, final OrderPullBackItems data,
        final String templateName, final List<BlSerialProductModel> blSerialProductModels) {
        try {
            final Document orderItemsInXMLDocument = createNewXMLDocument();
            final Element rootOrderItems = createRootElementForDocument(orderItemsInXMLDocument, BlCoreConstants.ITEMS_ROOT_ELEMENT);
                if(StringUtils.equalsIgnoreCase(templateName , BlCoreConstants.ORDER_PULL_BACK_REMOVED_ITEMS_EVENT_TEMPLATE) && CollectionUtils.isNotEmpty(blSerialProductModels)){
                    populateOrderDetailsForRemovedEntriesInXMl(orderItemsInXMLDocument , rootOrderItems , blSerialProductModels);
                }
                else if(CollectionUtils.isNotEmpty(orderModel.getEntries())) {
                    for (final AbstractOrderEntryModel entryModel : orderModel.getEntries()) {
                        populateOrderDetailsInXMl(entryModel, orderItemsInXMLDocument, rootOrderItems);
                    }
                }
            final Transformer transformer = getTransformerFactoryObject();
            final StringWriter writer = new StringWriter();

            //transform document to string
            transformer.transform(new DOMSource(orderItemsInXMLDocument), new StreamResult(writer));
            data.setItemsxml(writer.getBuffer().toString());

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
    private void populateOrderDetailsInXMl(final AbstractOrderEntryModel entryModel,
        final Document orderItemsInXMLDocument,
        final Element rootOrderItems) {
        final Element rootOrderItem = createRootElementForRootElement(orderItemsInXMLDocument, rootOrderItems, BlCoreConstants.ITEM_ROOT_ELEMENT);
            createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlCoreConstants.ORDER_ITEM_PRODUCT_CODE,
                getRequestValue(entryModel.getProduct().getCode()));
            createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlCoreConstants.ORDER_ITEM_PRODUCT_TITLE,
                entryModel.getProduct() instanceof BlSerialProductModel ? getProductTitle(entryModel.getProduct().getCode()) :entryModel.getProduct().getName());

    }


    /**
     * This method created to populate removed entry data in XML format
     * @param orderItemsInXMLDocument  orderItemsInXMLDocument
     * @param rootOrderItems rootOrderItems
     * @param blSerialProductModels list of removed entries
     */
    private void populateOrderDetailsForRemovedEntriesInXMl(final Document orderItemsInXMLDocument,
        final Element rootOrderItems, final List<BlSerialProductModel> blSerialProductModels) {
        final Element rootOrderItem = createRootElementForRootElement(orderItemsInXMLDocument, rootOrderItems, BlCoreConstants.ITEM_ROOT_ELEMENT);

        blSerialProductModels.forEach(blSerialProductModel -> {
                if(Objects.nonNull(blSerialProductModel)) {
                    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlCoreConstants.ORDER_ITEM_PRODUCT_CODE,
                        getRequestValue(blSerialProductModel.getCode()));
                    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlCoreConstants.ORDER_ITEM_PRODUCT_TITLE,
                        getProductTitle(blSerialProductModel.getCode()));
                    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlCoreConstants.ORDER_ITEM_QUANTITY,
                        BlCoreConstants.ONE);
                }
        });

    }

    public ConfigurationService getConfigurationService() {
        return configurationService;
    }

    public void setConfigurationService(ConfigurationService configurationService) {
        this.configurationService = configurationService;
    }

    public ProductService getProductService() {
        return productService;
    }

    public void setProductService(ProductService productService) {
        this.productService = productService;
    }

    public CatalogVersionService getCatalogVersionService() {
        return catalogVersionService;
    }

    public void setCatalogVersionService(
        CatalogVersionService catalogVersionService) {
        this.catalogVersionService = catalogVersionService;
    }
}
