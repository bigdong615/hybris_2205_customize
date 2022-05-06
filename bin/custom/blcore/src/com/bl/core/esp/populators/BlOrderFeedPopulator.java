package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.DocumentType;
import com.bl.core.enums.ExtendOrderStatusEnum;
import com.bl.core.enums.GearGaurdEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.esp.constants.BlespintegrationConstants;
import com.bl.esp.dto.OrderFeedData;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.google.common.util.concurrent.AtomicDouble;
import de.hybris.platform.catalog.CatalogVersionService;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * This populator created to convert Order to XML
 * @author Manikandan
 */
public class BlOrderFeedPopulator <SOURCE extends AbstractOrderModel, TARGET extends OrderFeedData>  implements
    Populator<SOURCE, TARGET> {

  private ProductService productService;
  private CatalogVersionService catalogVersionService;

  /**
   * This method created to convert order into XML
   * @param abstractOrderModel ordermodel
   * @param target the target to fill
   * @throws ConversionException ConversionException
   */
  @Override
  public void populate(final AbstractOrderModel abstractOrderModel, final OrderFeedData target) throws ConversionException {
    final Document orderItemsInXMLDocument = target.getData();
    final Element rootOrderItems = target.getElement();
    final Element rootOrderItem = createRootElementForRootElement(orderItemsInXMLDocument,
        rootOrderItems, BlespintegrationConstants.ORDER);
    createOrderMasterInfo(abstractOrderModel, orderItemsInXMLDocument, rootOrderItem);
    createOrderShippingInfo(abstractOrderModel, orderItemsInXMLDocument, rootOrderItem);
    createOrderBillingInfo(abstractOrderModel, orderItemsInXMLDocument, rootOrderItem);
    createOrderItems(abstractOrderModel, orderItemsInXMLDocument, rootOrderItem);
  }

  /**
   * This method created to populate order related items
   * @param abstractOrderModel order model
   * @param orderItemsInXMLDocument orderItemsInXMLDocument
   * @param rootOrderItems roota tags for values
   */
  private void createOrderMasterInfo(final AbstractOrderModel abstractOrderModel, final Document orderItemsInXMLDocument, final Element rootOrderItems) {
    final SimpleDateFormat formatter = new SimpleDateFormat(BlespintegrationConstants.DATE_PATTERN);
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
        BlespintegrationConstants.REPLACEMENT, BooleanUtils.isTrue(abstractOrderModel.getIsReplacementOrder())
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
      createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
          BlespintegrationConstants.SHIPPING_METHOD_TEXT, getRequestValue(delivery.getName()));
    }
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.TRACKING_INFO, getTrackingInfoFromOrder(abstractOrderModel));
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.ITEM_COST, String.valueOf(getDoubleValueForRequest(abstractOrderModel.getTotalPrice())));
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.DAMAGE_WAIVER_COST, String.valueOf(getDoubleValueForRequest(abstractOrderModel.getTotalDamageWaiverCost())));
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.SUB_TOTAL, String.valueOf(getDoubleValueForRequest(abstractOrderModel.getSubtotal())));
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.SHIPPING_AMOUNT, String.valueOf(getDoubleValueForRequest(abstractOrderModel.getDeliveryCost())));
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.TAX_AMOUNT, String.valueOf(getDoubleValueForRequest(abstractOrderModel.getTotalTax())));
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.DISCOUNT_AMOUNT, String.valueOf(getDoubleValueForRequest(abstractOrderModel.getTotalDiscounts())));
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.TOTAL_COST, String.valueOf(getDoubleValueForRequest(abstractOrderModel.getTotalPrice())));
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.DISCOUNT_TEXT, StringUtils.EMPTY);
    if(BooleanUtils.isTrue(abstractOrderModel.getIsRentalOrder()) && BooleanUtils.isFalse(abstractOrderModel.isGiftCardOrder())) {
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItems,
          BlespintegrationConstants.EXPECTED_SHIPPING_DATE,
          formatter.format(abstractOrderModel.getRentalStartDate()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItems,
          BlespintegrationConstants.ARRIVAL_DATE,
          formatter.format(abstractOrderModel.getRentalStartDate()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItems,
          BlespintegrationConstants.RETURN_DATE,
          formatter.format(abstractOrderModel.getRentalEndDate()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItems,
          BlespintegrationConstants.ACTUAL_RETURN_DATE,
          formatter.format(abstractOrderModel.getRentalEndDate()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItems,
          BlespintegrationConstants.RENTAL_DURATION,
          String.valueOf(getRentalDuration(abstractOrderModel)));
    }
    final UserModel userModel = abstractOrderModel.getUser();
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.CUSTOMER_NAME, getRequestValue(userModel.getName()));
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.VERIFICATION_LEVEL, getRequestValue(abstractOrderModel.getVerificationLevel()));
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.COI_AMOUNT, null != abstractOrderModel.getCoiAmount() && abstractOrderModel.getCoiAmount().compareTo(
            BigDecimal.valueOf(0.0)) > 0 ? String.valueOf(abstractOrderModel.getCoiAmount().setScale(
            BlCoreConstants.DECIMAL_PRECISION , RoundingMode.DOWN)) :
            String.valueOf(0.0));
    final CustomerModel user = (CustomerModel) abstractOrderModel.getUser();
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.COI_EXPIRATION_DATE, getCOIExpirationDateFromCustomer(user));
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.TOTAL_VALUE,String.valueOf(abstractOrderModel.getSubtotal()));

    if (Objects.nonNull(abstractOrderModel.getPaymentInfo())) {
      final BrainTreePaymentInfoModel brainTreePaymentInfoModel = (BrainTreePaymentInfoModel) abstractOrderModel.getPaymentInfo();
      createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
          BlespintegrationConstants.PAYMENT_TYPE,StringUtils.equalsIgnoreCase(BlespintegrationConstants.PAY_PAL_PROVIDER,brainTreePaymentInfoModel.getPaymentProvider())
              ? BlespintegrationConstants.PAY_PAL :getRequestValue(brainTreePaymentInfoModel.getPaymentProvider()));
    }
    else if(StringUtils.isNotBlank(abstractOrderModel.getPoNumber())){
      createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
          BlespintegrationConstants.PAYMENT_TYPE,BlespintegrationConstants.PO);
    }
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.PAYMENT_TEXT,StringUtils.EMPTY);
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.EXTENSION_AMOUNT,getOrderExtensionAmount(abstractOrderModel));
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.RETURNING_CUSTOMER,String.valueOf(isReturningCustomer(abstractOrderModel)));
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.LAST_UPDATED,formatter.format(abstractOrderModel.getOrderModifiedDate()));
  }


  /**
   * This method created to convert shipping info into XML
   * @param abstractOrderModel order model
   * @param orderItemsInXMLDocument orderItemsInXMLDocument
   * @param rootOrderItems rootOrderItems
   */
  private void createOrderShippingInfo(final AbstractOrderModel abstractOrderModel, final Document orderItemsInXMLDocument, final Element rootOrderItems) {
    final Element rootOrderItem = createRootElementForRootElement(orderItemsInXMLDocument, rootOrderItems, BlespintegrationConstants.SHIPPING_ROOT_ELEMENT);
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
        BlespintegrationConstants.ORDER_ID, getRequestValue(abstractOrderModel.getCode()));
    if (Objects.nonNull(abstractOrderModel.getDeliveryAddress())) {
      final AddressModel shippingAddress = abstractOrderModel.getDeliveryAddress();
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
          BlespintegrationConstants.SHIPPING_FIRST_NAME, getRequestValue(shippingAddress.getFirstname()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
          BlespintegrationConstants.SHIPPING_LAST_NAME, getRequestValue(shippingAddress.getLastname()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
          BlespintegrationConstants.SHIPPING_ORGANIZATION, getRequestValue(shippingAddress.getCompany()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
          BlespintegrationConstants.SHIPPING_ADDRESS_1, getRequestValue(shippingAddress.getLine1()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
          BlespintegrationConstants.SHIPPING_ADDRESS_2, getRequestValue(shippingAddress.getLine2()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
          BlespintegrationConstants.SHIPPING_CITY, getRequestValue(shippingAddress.getTown()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.SHIPPING_STATE,
          Objects.isNull(shippingAddress.getRegion()) ?  StringUtils.EMPTY : shippingAddress.getRegion().getName() );
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.SHIPPING_ZIP_CODE, getRequestValue(shippingAddress.getPostalcode()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.SHIPPING_PHONE, getRequestValue(shippingAddress.getCellphone()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.SHIPPING_EMAIL, getRequestValue(shippingAddress.getEmail()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.SHIPPING_HOURS,
          StringUtils.isEmpty(abstractOrderModel.getPickUpPersonEmail()) ?  StringUtils.EMPTY : getStoreOpeningHours(shippingAddress));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.SHIPPING_NOTES,
          StringUtils.isEmpty(abstractOrderModel.getDeliveryNotes())  ?  StringUtils.EMPTY: abstractOrderModel.getDeliveryNotes());

    }
  }

  /**
   * This method created to convert billing info into XML
   * @param orderModel ordermodel
   * @param orderItemsInXMLDocument orderItemsInXMLDocument
   * @param rootOrderItems rootOrderItems
   */
  private void createOrderBillingInfo(final AbstractOrderModel orderModel, final Document orderItemsInXMLDocument, final Element rootOrderItems) {
    final Element rootOrderItem = createRootElementForRootElement(orderItemsInXMLDocument, rootOrderItems, BlespintegrationConstants.BILLING_ROOT_ELEMENT);
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
          Objects.isNull(billingAddress.getRegion()) ? StringUtils.EMPTY : billingAddress.getRegion().getName());
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.BILLING_ZIP_CODE,
          getRequestValue(billingAddress.getPostalcode()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.BILLING_PHONE,
          getRequestValue(billingAddress.getCellphone()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.BILLING_EMAIL,
          getRequestValue(billingAddress.getEmail()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.BILLING_NOTES,getOrderNotesFromOrderModel(orderModel));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.BILLING_GIFT_CARD_USED,
          String.valueOf(Objects.isNull(orderModel.getGiftCardAmount()) ? 0.0: orderModel.getGiftCardAmount()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.BILLING_GIFT_CARD_BALANCE, getGiftCardBalance(orderModel));
    }
  }


  /**
   * This method created to populate order items into XML
   * @param abstractOrderModel abstractOrderModel
   * @param orderItemsInXMLDocument orderItemsInXMLDocument
   * @param rootOrderItems rootOrderItems
   */
  private void createOrderItems(final AbstractOrderModel abstractOrderModel, final Document orderItemsInXMLDocument,
      final Element rootOrderItems) {
    final SimpleDateFormat formatter = new SimpleDateFormat(BlespintegrationConstants.DATE_PATTERN);
    final Element rootOrderItem = createRootElementForRootElement(orderItemsInXMLDocument, rootOrderItems, BlespintegrationConstants.ORDER_ITEM_ROOT_ELEMENT);
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.ORDER_ID, getRequestValue(abstractOrderModel.getCode()));
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.ORDER_ITEM_ID, getRequestValue(abstractOrderModel.getCode()));
    if (CollectionUtils.isNotEmpty(abstractOrderModel.getEntries())) {
      for (final AbstractOrderEntryModel entryModel : abstractOrderModel.getEntries()) {
        convertOrderItems(orderItemsInXMLDocument , rootOrderItem , entryModel , formatter);
      }
    }
  }

  /**
   * This method created to convert order entry to XML
   * @param orderItemsInXMLDocument  orderItemsInXMLDocument
   * @param rootOrderItem rootOrderItem
   * @param entryModel entryModel
   * @param formatter formatter
   */
  private void convertOrderItems(final Document orderItemsInXMLDocument,
     final  Element rootOrderItem, final AbstractOrderEntryModel entryModel,
      final SimpleDateFormat formatter){
    final Element rootOrderEntry = createRootElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.ORDER_ITEM_ROOT);

    if (Objects.nonNull(entryModel.getProduct())) {
      createElementForRootElement(orderItemsInXMLDocument, rootOrderEntry, BlespintegrationConstants.ORDER_ITEM_PRODUCT_CODE,
          getRequestValue(entryModel.getProduct().getCode()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderEntry, BlespintegrationConstants.ORDER_ITEM_PRODUCT_TITLE,
          entryModel.getProduct() instanceof BlSerialProductModel ? getProductTitle(entryModel.getProduct().getCode()) :entryModel.getProduct().getName());
    }
    createElementForRootElement(orderItemsInXMLDocument, rootOrderEntry, BlespintegrationConstants.ORDER_ITEM_PRODUCT_PHOTO,
        entryModel.getProduct() instanceof BlSerialProductModel ? getSerialProductUrl(entryModel.getProduct().getCode()) : getProductURL(entryModel));
    if (Objects.nonNull(entryModel.getBasePrice())) {
      createElementForRootElement(orderItemsInXMLDocument, rootOrderEntry, BlespintegrationConstants.ORDER_ITEM_RENTAL_PRICE, String.valueOf(entryModel.getBasePrice().doubleValue()));
    }
    createElementForRootElement(orderItemsInXMLDocument, rootOrderEntry, BlespintegrationConstants.ORDER_ITEM_DAMAGE_WAIVER_PRICE,
        String.valueOf(getDamageWaiverPriceFromEntry(entryModel)));
    createElementForRootElement(orderItemsInXMLDocument, rootOrderEntry, BlespintegrationConstants.ORDER_ITEM_DAMAGE_WAIVER_TEXT, getDamageWaiverName(entryModel));
    createElementForRootElement(orderItemsInXMLDocument, rootOrderEntry, BlespintegrationConstants.ORDER_ITEM_TOTAL_PRICE,
        String.valueOf(getDoubleValueForRequest(entryModel.getTotalPrice())));
    createElementForRootElement(orderItemsInXMLDocument, rootOrderEntry, BlespintegrationConstants.LAST_UPDATED,
        Objects.isNull(entryModel.getUpdatedTime()) ? StringUtils.EMPTY : formatter.format(entryModel.getUpdatedTime()));
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
   * This method check Gift card payment type
   * @return string
   */
  protected String checkIsGiftCardUsed(final String creditCart){
    final StringBuilder paymentType= new StringBuilder();
    return paymentType.append(creditCart).toString();
  }

  /* This method is to get the damage waiver text  from order  entry model
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
   * This method created to get order status from order model
   * @param orderModel orderModel
   * @return String
   */
  private String getOrderStatus(final AbstractOrderModel orderModel) {
    return Objects.isNull(orderModel.getStatus()) ? StringUtils.EMPTY : orderModel.getStatus().getCode();
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
   * To get the request value based
   * @param value value get from order
   * @return value to set on request
   */
  protected String getRequestValue(final String value){
    return StringUtils.isBlank(value) ? StringUtils.EMPTY :value;
  }


  /**
   * This method created to get the shipping hours
   * @param shippingAddress addressmodel
   * @return string
   */
  private String getStoreOpeningHours(final AddressModel shippingAddress) {
    final Map<String, String> openingDaysDetails = shippingAddress.getOpeningDaysDetails();
    final StringBuilder stringBuilder = new StringBuilder();
    if (MapUtils.isNotEmpty(openingDaysDetails)) {
      openingDaysDetails.forEach(
          (key, value) -> stringBuilder.append(key).append(BlespintegrationConstants.COLON).append(value)
              .append(StringUtils.SPACE));
    }
    return stringBuilder.toString();
  }

  /**
   * This method is to get the order notes from order model
   * @param orderModel ordermodel
   * @return values to set on request
   */
  protected String getOrderNotesFromOrderModel(final AbstractOrderModel orderModel) {
    final StringBuilder orderNotes = new StringBuilder();

    if(CollectionUtils.isNotEmpty(orderModel.getOrderNotes())){
      orderModel.getOrderNotes().forEach(notesModel -> {
        if(BlespintegrationConstants.CUSTOMER_CHECKOUT_ORDER_NOTES.equalsIgnoreCase(notesModel.getType().getCode())) {
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
  protected String getGiftCardBalance(final AbstractOrderModel orderModel) {
    final AtomicReference<String> giftCardBalance = new AtomicReference<>(String.valueOf(0.0));
    if (CollectionUtils.isNotEmpty(orderModel.getGiftCard())) {
      orderModel.getGiftCard().forEach(giftCardModel -> giftCardModel.getMovements().forEach(giftCardMovementModel -> {
        if(StringUtils.equals(orderModel.getCode() , (giftCardMovementModel.getOrder() == null ? StringUtils.EMPTY : giftCardMovementModel.getOrder().getCode()))) {
          giftCardBalance.set(String.valueOf(giftCardMovementModel.getBalanceAmount()));
        }
      }));
    }
    return giftCardBalance.get();
  }

  /**
   * This method created to get the product title
   * @param serialProductCode serial code
   * @return string
   */

  protected String getProductTitle(final String serialProductCode) {
    final AtomicReference<String> productTitle = new AtomicReference<>(StringUtils.EMPTY);
    final CatalogVersionModel catalogVersion = getCatalogVersionService().getCatalogVersion(BlespintegrationConstants.CATALOG_VALUE,BlespintegrationConstants.ONLINE);
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
  protected String getSerialProductUrl(final String serialProductCode) {
    final AtomicReference<String> productUrl = new AtomicReference<>(StringUtils.EMPTY);
    final CatalogVersionModel catalogVersion = getCatalogVersionService().getCatalogVersion(BlespintegrationConstants.CATALOG_VALUE,BlespintegrationConstants.ONLINE);
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
   * This method created to get the order type
   * @param orderModel ordermodel
   * @return string
   */
  protected String getOrderType(final AbstractOrderModel orderModel){
    final AtomicReference<String> orderType = new AtomicReference<>(StringUtils.EMPTY);
    if(BooleanUtils.isTrue(orderModel.isGiftCardOrder())) {
      orderType.set(BlespintegrationConstants.GIFT_CARD_ORDER);
    }
    else if(BooleanUtils.isTrue(orderModel.getIsRetailGearOrder())){
      orderType.set(BlespintegrationConstants.NEW_GEAR_ORDER);
    }
    else if(BooleanUtils.isTrue(orderModel.getIsRentalOrder())){
      orderType.set(BlespintegrationConstants.RENTAL);
    }
    else if(BooleanUtils.isFalse(orderModel.getIsRentalOrder())){
      orderType.set(BlespintegrationConstants.USED_GEAR);
    }

    return orderType.get();
  }

  /**
   * Populate rental duration with values from the OrderModel.
   *
   * @param orderModel the source object
   */
  protected long getRentalDuration(final AbstractOrderModel orderModel) {
    return getDaysBetweenDates(orderModel.getRentalStartDate(), orderModel.getRentalEndDate());
  }

  /**
   * This method created to get difference between the dates
   * @param startDate startdate
   * @param endDate end date
   * @return numbtrer of days
   */
  public static long getDaysBetweenDates(final Date startDate, final Date endDate)
  {
    final LocalDate localStartDate = startDate.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
    final LocalDate localEndDate = endDate.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
    return ChronoUnit.DAYS.between(localStartDate, localEndDate);
  }

  /**
   * This method created to get tracking numbers from order model
   * @param abstractOrderModel order model
   * @return tracking numbers
   */
  private String getTrackingInfoFromOrder(final AbstractOrderModel abstractOrderModel) {
    final StringBuilder stringBuilder = new StringBuilder();
    if(CollectionUtils.isNotEmpty(abstractOrderModel.getConsignments())) {
      abstractOrderModel.getConsignments().forEach(consignmentModel -> {
        if(CollectionUtils.isNotEmpty(consignmentModel.getPackaginginfos())){
          consignmentModel.getPackaginginfos().forEach(packagingInfoModel -> {
            if(StringUtils.isNotEmpty(packagingInfoModel.getOutBoundTrackingNumber())) {
              stringBuilder.append(packagingInfoModel.getOutBoundTrackingNumber()).append(BlespintegrationConstants.COMMA);
            }
          });
        }
      });
    }
    return stringBuilder.toString();
  }

  /**
   * This method created to get the Order extension amount
   * @param abstractOrderModel order model
   * @return extension amount
   */
  private String getOrderExtensionAmount(final AbstractOrderModel abstractOrderModel) {
    final AtomicDouble atomicDouble = new AtomicDouble(0.0);
    if(CollectionUtils.isNotEmpty(abstractOrderModel.getExtendedOrderCopyList())) {
      abstractOrderModel.getExtendedOrderCopyList().forEach(extendOrderList -> {
        if(StringUtils.equalsIgnoreCase(extendOrderList.getExtendOrderStatus().getCode(), ExtendOrderStatusEnum.COMPLETED.getCode())){
          atomicDouble.addAndGet(extendOrderList.getTotalPrice());
        }
      });
    }

    return String.valueOf(atomicDouble.get());
  }


  /**
   * This method created to check whether is returning customer or not
   * @param orderModel order model
   * @return boolean value
   */
  protected boolean isReturningCustomer(final AbstractOrderModel orderModel) {
    final Collection<OrderModel> abstractOrderEntryModel =  orderModel.getUser().getOrders();
    return CollectionUtils.isNotEmpty(abstractOrderEntryModel) && abstractOrderEntryModel.size() > 1;
  }

  /**
   * This method created to get COI expiration time from verification document
   * @param user user
   * @return string
   */
  private String getCOIExpirationDateFromCustomer(final CustomerModel user) {
   final List<Date> dateList = new ArrayList<>();
    if(CollectionUtils.isNotEmpty(user.getVerificationDocuments())) {
      user.getVerificationDocuments().forEach(verificationDocumentMediaModel -> {
        if (StringUtils.equalsIgnoreCase(verificationDocumentMediaModel.getDocumentType().getCode() ,
            DocumentType.INSURANCE_CERTIFICATE.getCode())&& Objects.nonNull(verificationDocumentMediaModel.getExpiryDate())) {
          dateList.add(verificationDocumentMediaModel.getExpiryDate());
        }
      });
    }
    dateList.sort(Date::compareTo);
    return String.valueOf(dateList.isEmpty() ? StringUtils.EMPTY : dateList.get(dateList.size()-1));

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
