package com.bl.core.esp.populators;

import com.bl.core.enums.GearGaurdEnum;
import com.bl.core.model.BlItemsBillingChargeModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.esp.constants.BlespintegrationConstants;
import com.bl.esp.dto.OrderFeedData;
import com.google.common.util.concurrent.AtomicDouble;
import de.hybris.platform.catalog.CatalogVersionService;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.product.ProductService;
import java.text.SimpleDateFormat;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

public class BlChargeBillFeedPopulator <SOURCE extends AbstractOrderModel, TARGET extends OrderFeedData>  implements
    Populator<SOURCE, TARGET> {

  private ProductService productService;
  private CatalogVersionService catalogVersionService;

  @Override
  public void populate(final AbstractOrderModel abstractOrderModel, final OrderFeedData target) {
    final Document orderItemsInXMLDocument = target.getData();
    final Element rootOrderItems = target.getElement();
    final Element rootOrderItem = createRootElementForRootElement(orderItemsInXMLDocument,
        rootOrderItems, BlespintegrationConstants.ORDER);
    final SimpleDateFormat formatter = new SimpleDateFormat(BlespintegrationConstants.DATE_PATTERN);
    createOrderBillInfo(abstractOrderModel, orderItemsInXMLDocument, rootOrderItem, formatter);
    createOrderBillItemInfo(abstractOrderModel, orderItemsInXMLDocument, rootOrderItem, formatter);
  }


  protected Element createRootElementForRootElement(final Document document, final Element rootElement, final String rootElementName) {
    final Element childElement = document.createElement(rootElementName);
    rootElement.appendChild(childElement);
    return childElement;
  }

  /**
   *  changes related to convert order bill related information into xml.
   * @param abstractOrderModel
   * @param orderItemsInXMLDocument
   * @param rootOrderItems
   * @param formatter
   */
  private void createOrderBillInfo(final AbstractOrderModel abstractOrderModel,
      final Document orderItemsInXMLDocument, final Element rootOrderItems,
      final SimpleDateFormat formatter) {
    final Element rootOrderItem = createRootElementForRootElement(orderItemsInXMLDocument,
        rootOrderItems, BlespintegrationConstants.Order_Bill);
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
        BlespintegrationConstants.ORDER_ID, getRequestValue(abstractOrderModel.getCode()));
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
        BlespintegrationConstants.OLD_ORDER, StringUtils.EMPTY);
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
        BlespintegrationConstants.SUBSCRIBER_ID,
        getRequestValue(abstractOrderModel.getUser().getUid()));
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
        BlespintegrationConstants.EMAIL_ADDRESS,
        getRequestValue(abstractOrderModel.getUser().getUid()));
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
        BlespintegrationConstants.TYPE, getOrderType(abstractOrderModel));
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
        BlespintegrationConstants.REPLACEMENT,
        BooleanUtils.isTrue(abstractOrderModel.getIsCartUsedForReplacementOrder())
            ? Boolean.TRUE.toString() : Boolean.FALSE.toString());
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
        BlespintegrationConstants.STATUS, getRequestValue(getOrderStatus(abstractOrderModel)));
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
        BlespintegrationConstants.DATE_PLACED,
        String.valueOf(abstractOrderModel.getCreationtime()));
    if (Objects.nonNull(abstractOrderModel.getDeliveryMode())) {
      final ZoneDeliveryModeModel delivery = ((ZoneDeliveryModeModel) abstractOrderModel
          .getDeliveryMode());
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
          BlespintegrationConstants.SHIPPING_METHOD_TYPE,
          getRequestValue(delivery.getShippingGroup().getName()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
          BlespintegrationConstants.SHIPPING_METHOD, getRequestValue(delivery.getCode()));
    }
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
        BlespintegrationConstants.ARRIVAL_DATE,
        formatter.format(abstractOrderModel.getRentalStartDate()));
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
        BlespintegrationConstants.RETURN_DATE,
        formatter.format(abstractOrderModel.getRentalEndDate()));
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
        BlespintegrationConstants.ACTUAL_RETURN_DATE,
        formatter.format(abstractOrderModel.getRentalEndDate()));
    final UserModel userModel = abstractOrderModel.getUser();
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
        BlespintegrationConstants.CUSTOMER_NAME, getRequestValue(userModel.getName()));
    final double totalPendingBill = getTotalPendingBill(abstractOrderModel);
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
        BlespintegrationConstants.TOTAL_BILL_AMOUNT, String.valueOf(totalPendingBill));
  }

  /**
   *  changes related to convert order bill items related data into xml
   * @param orderModel
   * @param orderItemsInXMLDocument
   * @param rootOrderItems
   * @param formatter
   */
  private void createOrderBillItemInfo(final AbstractOrderModel orderModel,
      final Document orderItemsInXMLDocument, final Element rootOrderItems,
      final SimpleDateFormat formatter) {
    final Element rootOrderItem = createRootElementForRootElement(orderItemsInXMLDocument,
        rootOrderItems,
        BlespintegrationConstants.ORDER_BILL_ITEMS);
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
        BlespintegrationConstants.ORDER_ID, getRequestValue(orderModel.getCode()));
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
        BlespintegrationConstants.ORDER_ITEM_ID,
        getRequestValue(orderModel.getCode()));
    orderModel.getConsignments().forEach(consignmentModel -> {
      consignmentModel.getConsignmentEntries().forEach(consignmentEntryModel -> {
        final Element rootOrderEntryItem = createRootElementForRootElement(orderItemsInXMLDocument,
            rootOrderItem, BlespintegrationConstants.ORDER_BILL_ITEM);
        convertProductData(orderItemsInXMLDocument, rootOrderEntryItem, consignmentEntryModel);
        convertBillsData(orderItemsInXMLDocument, rootOrderEntryItem, consignmentEntryModel,
            formatter);
      });
    });
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.TOTAL,
        getRequestValue(String.valueOf(orderModel.getTotalPrice())));
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

  /**
   * This method used to add all pending charges for particular order.
   * @param abstractOrderModel
   * @return
   */
 private double getTotalPendingBill(AbstractOrderModel abstractOrderModel){
    final AtomicDouble pendingBill= new AtomicDouble(0.0);
    abstractOrderModel.getConsignments().forEach(consignmentModel -> {
      consignmentModel.getConsignmentEntries().forEach(consignmentEntryModel -> {
        consignmentEntryModel.getBillingCharges().forEach((serialId,pendingBillList) ->{
          pendingBillList.forEach(blItemsBillingChargeModel ->
            pendingBill.addAndGet(blItemsBillingChargeModel.getChargedAmount().doubleValue())
          );
        });
      });
    });
    return pendingBill.get();
 }

  /**
   * Convert product related data into xml
   * @param orderItemsInXMLDocument
   * @param rootOrderItem
   * @param consignmentEntryModel
   */
  private void convertProductData(final Document orderItemsInXMLDocument,
      final Element rootOrderItem, ConsignmentEntryModel consignmentEntryModel) {
    final AbstractOrderEntryModel orderEntry = consignmentEntryModel.getOrderEntry();
    if (Objects.nonNull(orderEntry.getProduct())) {
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
          BlespintegrationConstants.ORDER_ITEM_PRODUCT_TITLE,
          orderEntry.getProduct() instanceof BlSerialProductModel ? getProductTitle(
              orderEntry.getProduct().getCode()) : orderEntry.getProduct().getName());
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
          BlespintegrationConstants.PRODUCT_IMAGE_THUMBNAIL,
          orderEntry.getProduct() instanceof BlSerialProductModel ? getSerialProductThumbnailUrl(
              orderEntry.getProduct().getCode()) : getProductThumbnailURL(orderEntry));
    }
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
        BlespintegrationConstants.ORDER_ITEM_QUANTITY,
        getRequestValue(String.valueOf(orderEntry.getQuantity())));
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
        BlespintegrationConstants.GEAR_GUARD, getDamageWaiverName(orderEntry));
  }

  /**
   * This method used to convert bill entry related data into xml.
   * @param orderItemsInXMLDocument
   * @param rootOrderItem
   * @param consignmentEntryModel
   * @param formatter
   */
  private void convertBillsData(final Document orderItemsInXMLDocument,
      final Element rootOrderItem, ConsignmentEntryModel consignmentEntryModel,
      final SimpleDateFormat formatter) {
    final Map<String, List<BlItemsBillingChargeModel>> billingCharges = consignmentEntryModel
        .getBillingCharges();
    if (Objects.nonNull(billingCharges) && !billingCharges.isEmpty()) {
      billingCharges.forEach((serialCode, serialBills) -> {
        if (CollectionUtils.isNotEmpty(serialBills)) {
          createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
              BlespintegrationConstants.ORDER_ITEM_PRODUCT_CODE,
              getRequestValue(serialCode));
          serialBills.forEach(billCharge ->
              convertBillDataToXml(orderItemsInXMLDocument, rootOrderItem, billCharge, formatter)
          );
        }
      });
    }
  }

  /**
   * Convert bill data into xml.
   * @param orderItemsInXMLDocument
   * @param rootOrderItem
   * @param billCharge
   * @param formatter
   */
  private void convertBillDataToXml(final Document orderItemsInXMLDocument,
  final  Element rootOrderItem,final BlItemsBillingChargeModel billCharge,final SimpleDateFormat formatter){
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.BILL_TYPE,
      getRequestValue(billCharge.getBillChargeType().toString()));
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.NOTES,
      getRequestValue(billCharge.getUnPaidBillNotes()));
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.BILL_CREATED_DATE,
      getRequestValue(formatter.format(billCharge.getCreationtime())));
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
        BlespintegrationConstants.STATUS, getRequestValue(billCharge.isBillPaid() ? BlespintegrationConstants.PAID : BlespintegrationConstants.NOT_PAID));
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

  /**
   * This method created to get the serial product url for request
   * @param serialProductCode serial product code
   * @return string
   */
  protected String getSerialProductThumbnailUrl(final String serialProductCode) {
    final AtomicReference<String> productUrl = new AtomicReference<>(StringUtils.EMPTY);
    final CatalogVersionModel catalogVersion = getCatalogVersionService().getCatalogVersion(BlespintegrationConstants.CATALOG_VALUE,BlespintegrationConstants.ONLINE);
    final BlSerialProductModel blSerialProduct = (BlSerialProductModel) getProductService().getProductForCode(catalogVersion, serialProductCode);
    if(Objects.nonNull(blSerialProduct)) {
      final BlProductModel blProductModel = blSerialProduct.getBlProduct();
      if(Objects.nonNull(blProductModel)  && Objects.nonNull(blProductModel.getThumbnail()) &&
          StringUtils.isNotBlank(blProductModel.getThumbnail().getURL())){
        productUrl.set(blProductModel.getThumbnail().getURL());
      }
    }
    return productUrl.get();
  }

  /**
   * To check whether media is empty of not
   * @param abstractOrderEntryModel abstractOrderEntryModel
   * @return string
   */
  protected String getProductThumbnailURL(final AbstractOrderEntryModel abstractOrderEntryModel){
    return Objects.nonNull(abstractOrderEntryModel.getProduct().getThumbnail()) &&
        StringUtils.isNotBlank(abstractOrderEntryModel.getProduct().getThumbnail().getURL()) ?
        abstractOrderEntryModel.getProduct().getThumbnail().getURL() : StringUtils.EMPTY;
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
}
