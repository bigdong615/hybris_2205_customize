package com.bl.esp.service.impl;

import com.bl.core.enums.GearGaurdEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.esp.constants.BlespintegrationConstants;
import com.bl.esp.service.BlFTPService;
import com.bl.logging.BlLogger;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.google.common.util.concurrent.AtomicDouble;
import com.jcraft.jsch.Channel;
import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import com.jcraft.jsch.SftpException;
import de.hybris.platform.catalog.CatalogVersionService;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.util.Config;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.StringWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicReference;
import javax.xml.bind.JAXBException;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * This class created to send file to FTP
 * @author Manikandan
 */
public class DefaultBlESPFTPService implements BlFTPService {

  private static final Logger LOG = Logger.getLogger(DefaultBlESPFTPService.class);

  private ProductService productService;
  private CatalogVersionService catalogVersionService;

  /**
   * This method created to convert order into XML
   * @param abstractOrderModels list of orders
   * @throws ParserConfigurationException ParserConfigurationException
   */
  public void convertOrderIntoXML(final List<AbstractOrderModel> abstractOrderModels)
      throws ParserConfigurationException, JAXBException {
    final Document orderItemsInXMLDocument = createNewXMLDocument();
    final Element rootOrderItems = createRootElementForDocument(orderItemsInXMLDocument, BlespintegrationConstants.ORDERS);

    try {
      abstractOrderModels.forEach(abstractOrderModel -> {
        final Element rootOrderItem = createRootElementForRootElement(orderItemsInXMLDocument,
            rootOrderItems, BlespintegrationConstants.ORDER);
        createOrderMasterInfo(abstractOrderModel, orderItemsInXMLDocument, rootOrderItem);
        createOrderShippingInfo(abstractOrderModel, orderItemsInXMLDocument, rootOrderItem);
        createOrderBillingInfo(abstractOrderModel, orderItemsInXMLDocument, rootOrderItem);
        createOrderItems(abstractOrderModel, orderItemsInXMLDocument, rootOrderItem);

      });

      Transformer transformer = TransformerFactory.newInstance().newTransformer();
      transformer.setOutputProperty(OutputKeys.INDENT, BlespintegrationConstants.YES);
      transformer.setOutputProperty(BlespintegrationConstants.XML_INDENT, Config.getParameter(BlespintegrationConstants.XML_INDENT_SIZE));
      StreamResult result = new StreamResult(new StringWriter());
      DOMSource source = new DOMSource(orderItemsInXMLDocument);
      transformer.transform(source, result);
      String xmlString = result.getWriter().toString();
      final String logFileName = new SimpleDateFormat(BlespintegrationConstants.FILE_FORMAT).format(new Date());
      final String fileName = logFileName + BlespintegrationConstants.FILE_SUFFIX;
      Path completefileName = Path.of(Config.getParameter(BlespintegrationConstants.LOCAL_FTP_PATH) + fileName);
      Files.writeString(completefileName, xmlString);

        Session session = null;
        Channel channel = null;
        ChannelSftp channelSftp = null;
      try {
        JSch jsch = new JSch();
        session = jsch.getSession(Config.getParameter(BlespintegrationConstants.SFTPUSER),
            BlespintegrationConstants.SFTPHOST,
            BlespintegrationConstants.SFTPPORT);
        session.setPassword(Config.getParameter(BlespintegrationConstants.SFTPPASS));
        Properties config = new Properties();
        config.put(BlespintegrationConstants.STICT_HOST_KEY, BlespintegrationConstants.NO);
        session.setConfig(config);
        session.connect();
        channel = session.openChannel(BlespintegrationConstants.SFTP);
        channel.connect();
        channelSftp = (ChannelSftp) channel;
        channelSftp.cd(Config.getParameter(BlespintegrationConstants.CLIENT_FTP_PATH));
        File f = new File(completefileName.toAbsolutePath().toString());
        channelSftp.put(new FileInputStream(f), f.getName());
      } catch (JSchException | SftpException |FileNotFoundException ex) {
        BlLogger.logMessage(LOG, Level.ERROR, "Error while performing sendFileTOFTP:-", ex);
      }
      finally {
        if (null != channelSftp) {
          channelSftp.disconnect();
          channelSftp.exit();
        }
        if (null != channel) {
          channel.disconnect();
        }
        if (null != session) {
          session.disconnect();
        }
      }

    }
    catch (final TransformerException | IOException e) {
      BlLogger.logMessage(LOG, Level.ERROR, "Error while performing convertOrderIntoXML" , e );

    }

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
        BlespintegrationConstants.OLD_ORDER,StringUtils.EMPTY);
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
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.EXPECTED_SHIPPING_DATE, formatter.format(abstractOrderModel.getRentalStartDate()));
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.ARRIVAL_DATE, formatter.format(abstractOrderModel.getRentalStartDate()));
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.RETURN_DATE, formatter.format(abstractOrderModel.getRentalEndDate()));
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.ACTUAL_RETURN_DATE, formatter.format(abstractOrderModel.getRentalEndDate()));
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.RENTAL_DURATION, String.valueOf(getRentalDuration(abstractOrderModel)));
    final UserModel userModel = abstractOrderModel.getUser();
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.CUSTOMER_NAME, getRequestValue(userModel.getName()));
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.VERIFICATION_LEVEL, abstractOrderModel.getVerificationLevel());
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.COI_AMOUNT, String.valueOf(0.0));
    final CustomerModel user = (CustomerModel) abstractOrderModel.getUser();
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.COI_EXPIRATION_DATE, Objects.nonNull(user.getCoiExpirationDate()) ?
            String.valueOf(user.getCoiExpirationDate()) : StringUtils.EMPTY);
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.TOTAL_VALUE,String.valueOf(abstractOrderModel.getSubtotal()));

    if (Objects.nonNull(abstractOrderModel.getPaymentInfo())) {
      final BrainTreePaymentInfoModel brainTreePaymentInfoModel = (BrainTreePaymentInfoModel) abstractOrderModel.getPaymentInfo();
      createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
          BlespintegrationConstants.TOTAL_VALUE,StringUtils.equalsIgnoreCase(BlespintegrationConstants.PAY_PAL_PROVIDER,brainTreePaymentInfoModel.getPaymentProvider())
              ? BlespintegrationConstants.PAY_PAL :getRequestValue(brainTreePaymentInfoModel.getPaymentProvider()));
    }
    else if(StringUtils.isNotBlank(abstractOrderModel.getPoNumber())){
      createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
          BlespintegrationConstants.PAYMENT_TYPE,BlespintegrationConstants.PO);
    }
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.PAYMENT_TEXT,StringUtils.EMPTY);
    createElementForRootElement(orderItemsInXMLDocument , rootOrderItems,
        BlespintegrationConstants.EXTENSION_AMOUNT,StringUtils.EMPTY);

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
          Objects.nonNull(shippingAddress.getRegion()) ? shippingAddress.getRegion().getName() : StringUtils.EMPTY);
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.SHIPPING_ZIP_CODE, getRequestValue(shippingAddress.getPostalcode()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.SHIPPING_PHONE, getRequestValue(shippingAddress.getCellphone()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.SHIPPING_EMAIL, getRequestValue(shippingAddress.getEmail()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.SHIPPING_HOURS,
          StringUtils.isNotEmpty(abstractOrderModel.getPickUpPersonEmail()) ? getStoreOpeningHours(shippingAddress) : StringUtils.EMPTY);
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.SHIPPING_NOTES,
          StringUtils.isNotBlank(abstractOrderModel.getDeliveryNotes())  ? abstractOrderModel.getDeliveryNotes() : StringUtils.EMPTY);

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
    final Element rootOrderItem = createRootElementForRootElement(orderItemsInXMLDocument, rootOrderItems, BlespintegrationConstants.ORDER_ITEM_ROOT_ELEMENT);
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.ORDER_ID, getRequestValue(abstractOrderModel.getCode()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.ORDER_ITEM_ID, getRequestValue(abstractOrderModel.getCode()));
    if (CollectionUtils.isNotEmpty(abstractOrderModel.getEntries())) {
      for (final AbstractOrderEntryModel entryModel : abstractOrderModel.getEntries()) {
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
      }
      }
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
        if(StringUtils.equals(orderModel.getCode() , (giftCardMovementModel.getOrder() != null ? giftCardMovementModel.getOrder().getCode() :StringUtils.EMPTY))) {
          giftCardBalance.set(String.valueOf(giftCardMovementModel.getBalanceAmount()));
        }
      }));
    }
    return giftCardBalance.get();
  }

  /**
   * This method craeted to get the product title
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
   * Populate rental duration with values from the OrderModel.
   *
   * @param orderModel the source object
   */
  protected long getRentalDuration(final AbstractOrderModel orderModel) {
    return getDaysBetweenDates(orderModel.getRentalStartDate(), orderModel.getRentalEndDate());
  }

  /**
   * This method created to get differnce between twodats
   * @param startDate startdate
   * @param endDate end date
   * @return number of days
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
