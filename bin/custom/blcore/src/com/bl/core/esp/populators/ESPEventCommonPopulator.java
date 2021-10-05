/**
 *
 */
package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.GearGaurdEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.esp.order.ESPEventCommonOrderDataRequest;
import com.bl.esp.order.ESPEventCommonRequest;
import com.google.common.util.concurrent.AtomicDouble;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
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
    protected Double getDamageWaiverPriceFromEntry(final AbstractOrderEntryModel abstractOrderEntryModel) {
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
            orderType.set(BlCoreConstants.NEW_GEAR);
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
     * This method craeted to get the product title
     * @param serialProductCode serial code
     * @return string
     */

    protected String getProductTitle(final String serialProductCode) {
        final AtomicReference<String> productTitle = new AtomicReference<>(StringUtils.EMPTY);
        final BlSerialProductModel blSerialProduct = (BlSerialProductModel) getProductService().getProductForCode(serialProductCode);
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
        final BlSerialProductModel blSerialProduct = (BlSerialProductModel) getProductService().getProductForCode(serialProductCode);
        if(Objects.nonNull(blSerialProduct)) {
            final BlProductModel blProductModel = blSerialProduct.getBlProduct();
            if(Objects.nonNull(blProductModel)){
                productUrl.set(blProductModel.getPicture().getURL());
            }
        }
        return productUrl.get();
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

}
