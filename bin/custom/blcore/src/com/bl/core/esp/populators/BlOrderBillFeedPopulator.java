package com.bl.core.esp.populators;

import com.bl.core.model.BlItemsBillingChargeModel;
import com.bl.esp.constants.BlespintegrationConstants;
import com.bl.esp.dto.OrderFeedData;
import com.google.common.util.concurrent.AtomicDouble;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * This populator created to convert Order bills to XML
 * @author Vijay Vishwakarma
 */
public class BlOrderBillFeedPopulator<SOURCE extends AbstractOrderModel, TARGET extends OrderFeedData>  extends
    AbstractBlOrderFeedPopulater implements
    Populator<SOURCE, TARGET> {

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
    final String totalPendingBill = getTotalPendingBill(abstractOrderModel);
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
        BlespintegrationConstants.TOTAL_BILL_AMOUNT, totalPendingBill);
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
    orderModel.getConsignments().forEach(consignmentModel -> {
      consignmentModel.getConsignmentEntries().forEach(consignmentEntryModel -> {
        final AbstractOrderEntryModel orderEntry = consignmentEntryModel.getOrderEntry();
        final ProductModel product = orderEntry.getProduct();
        final Map<String, List<BlItemsBillingChargeModel>> billingCharges = consignmentEntryModel
            .getBillingCharges();
        if (Objects.nonNull(billingCharges) && !billingCharges.isEmpty()) {
          billingCharges.forEach((serialCode, serialBills) -> {
            if (CollectionUtils.isNotEmpty(serialBills)) {
              serialBills.forEach(billCharge -> {
                final Element rootOrderItem = createRootElementForRootElement(orderItemsInXMLDocument,
                    rootOrderItems,BlespintegrationConstants.ORDER_BILL_ITEM);
                createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
                    BlespintegrationConstants.ORDER_ID, getRequestValue(orderModel.getCode()));
                createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,BlespintegrationConstants.ORDER_ITEM_ID,
                    billCharge.getPk().toString());
                // converting product related data into xml tag
                convertProductData(orderItemsInXMLDocument, rootOrderItem, product);
                createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.TOTAL,
                    getRequestValue(String.valueOf(billCharge.getChargedAmount().doubleValue())));
                createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
                    BlespintegrationConstants.GEAR_GUARD, getDamageWaiverName(
                        orderEntry));
                // Converting bill related data into xml tag
                convertBillDataToXml(orderItemsInXMLDocument, rootOrderItem, billCharge, formatter);
                  }
              );
            }
          });
        }
      });
    });
  }

  /**
   * This method used to add all pending charges for particular order.
   * @param abstractOrderModel
   * @return
   */
 private String getTotalPendingBill(AbstractOrderModel abstractOrderModel){
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
   DecimalFormat f = new DecimalFormat("##0.00");
    return f.format(pendingBill.get());
 }

  /**
   * Convert product related data into xml
   * @param orderItemsInXMLDocument
   * @param rootOrderItem
   * @param productModel
   */
  private void convertProductData(final Document orderItemsInXMLDocument,
      final Element rootOrderItem, ProductModel productModel) {
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
        BlespintegrationConstants.ORDER_ITEM_PRODUCT_CODE,getRequestValue(productModel.getCode()));
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
        BlespintegrationConstants.ORDER_ITEM_PRODUCT_TITLE,getRequestValue(productModel.getName()));
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
        BlespintegrationConstants.PRODUCT_IMAGE_THUMBNAIL, getProductThumbnailURL(productModel));
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
        BlespintegrationConstants.ORDER_ITEM_QUANTITY,BlespintegrationConstants.BILL_QUANTITY);
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
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlespintegrationConstants.UPDATED_TIME,
        getRequestValue(formatter.format(billCharge.getUpdatedBillTime())));
    createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
        BlespintegrationConstants.STATUS, getRequestValue(billCharge.isBillPaid() ? BlespintegrationConstants.PAID : BlespintegrationConstants.NOT_PAID));
}

  /**
   * To check whether media is empty of not
   * @param productModel
   * @return string
   */
  protected String getProductThumbnailURL(final ProductModel productModel){
    return Objects.nonNull(productModel.getThumbnail()) &&
        StringUtils.isNotBlank(productModel.getThumbnail().getURL()) ?
        productModel.getThumbnail().getURL() : StringUtils.EMPTY;
  }

}
