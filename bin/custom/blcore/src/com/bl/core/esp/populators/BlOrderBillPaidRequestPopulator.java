package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlSerialProductModel;
import com.bl.esp.dto.billpaid.OrderBillPaidEventRequest;
import com.bl.esp.dto.billpaid.data.OrderBillPaidData;
import com.bl.esp.dto.billpaid.data.OrderBillPaidExtraData;
import com.bl.esp.exception.BlESPIntegrationException;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.List;
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
 * This populator populates Bill Paid ESP attributes.
 *
 * @author Neeraj Singh
 */
public class BlOrderBillPaidRequestPopulator extends
    ESPEventCommonPopulator<OrderModel, OrderBillPaidEventRequest> {

  private static final Logger LOG = Logger.getLogger(BlOrderBillPaidRequestPopulator.class);
  private static final String POPULATOR_ERROR = "Error while populating data for ESP Event";

  /**
   * Populate the target instance with values from the source instance.
   *
   * @param orderModel                the source object
   * @param orderBillPaidEventRequest the target to fill
   * @throws ConversionException if an error occurs
   */
  @Override
  public void populate(final OrderModel orderModel,
      final OrderBillPaidEventRequest orderBillPaidEventRequest)
      throws ConversionException {
    Assert.notNull(orderModel, "Parameter emailId cannot be null.");
    Assert.notNull(orderBillPaidEventRequest, "Parameter contactRequest cannot be null.");

    final UserModel userModel = orderModel.getUser();
    if (Objects.nonNull(userModel)) {
      orderBillPaidEventRequest.setContactKey(getRequestValue(userModel.getUid()));
    }
    orderBillPaidEventRequest
        .setEventDefinitionKey(getRequestValue(getConfigurationService().getConfiguration().
            getString(BlCoreConstants.ORDER_BILL_PAID_EVENT_DEFINITION_KEY)));
    populateBillPaidData(orderModel, orderBillPaidEventRequest);

  }

  /**
   * This method populates bill paid data from order model
   *
   * @param orderModel                orderodel
   * @param orderBillPaidEventRequest request to be get updated
   */
  private void populateBillPaidData(final OrderModel orderModel,
      final OrderBillPaidEventRequest orderBillPaidEventRequest) {

    final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
    final OrderBillPaidData orderBillPaidData = new OrderBillPaidData();
    populateCommonData(orderModel, orderBillPaidData);
    orderBillPaidData.setOldOrderId(StringUtils.EMPTY);
    orderBillPaidData.setTemplate(getRequestValue(getConfigurationService().getConfiguration().
        getString(BlCoreConstants.ORDER_BILL_PAID_EVENT_TEMPLATE)));
    orderBillPaidData.setStatus(getRequestValue(
        Objects.nonNull(orderModel.getStatus()) ? orderModel.getStatus().getCode()
            : StringUtils.EMPTY));
    orderBillPaidData.setDatePlaced(formatter.format(orderModel.getDate()));
    if (Objects.nonNull(orderModel.getDeliveryMode())) {
      final ZoneDeliveryModeModel zoneDeliveryModeModel = ((ZoneDeliveryModeModel) orderModel
          .getDeliveryMode());
      orderBillPaidData.setShippingMethodType(
          getRequestValue(zoneDeliveryModeModel.getShippingGroup().getName()));
      orderBillPaidData.setShippingMethod(getRequestValue(zoneDeliveryModeModel.getCode()));
    } else {
      orderBillPaidData.setShippingMethodType(StringUtils.EMPTY);
      orderBillPaidData.setShippingMethod(StringUtils.EMPTY);
    }
    if (BooleanUtils.isTrue(orderModel.getIsRentalCart()) && BooleanUtils
        .isFalse(orderModel.isGiftCardOrder())) {
      orderBillPaidData
          .setExpectedShipping(formatter.format(orderModel.getActualRentalStartDate()));
      orderBillPaidData.setArrivalDate(formatter.format(orderModel.getRentalStartDate()));
      orderBillPaidData.setReturnDate(formatter.format(orderModel.getRentalEndDate()));
      orderBillPaidData.setRentalDuration((int) getRentalDuration(orderModel));
    }
    final UserModel userModel = orderModel.getUser();
    if (Objects.nonNull(userModel)) {
      orderBillPaidData.setCustomerName(getRequestValue(userModel.getName()));
    }
    final OrderBillPaidExtraData orderBillPaidExtraData = orderBillPaidEventRequest.getExtraData();
    orderBillPaidData.setTotalBillAmount(orderBillPaidExtraData.getTotalBillPaidAmount());
    populateOrderItemsInXML(orderModel, orderBillPaidData,orderBillPaidExtraData);
    orderBillPaidEventRequest.setExtraData(null);
    orderBillPaidEventRequest.setData(orderBillPaidData);
  }

  /**
   * This method populates item info details
   *
   * @param orderModel
   * @param orderBillPaidData
   */
  private void populateOrderItemsInXML(final OrderModel orderModel,
      final OrderBillPaidData orderBillPaidData, final OrderBillPaidExtraData orderBillPaidExtraData) {
    try {
      final Map<String, List<String>> billPaidTypesMap = orderBillPaidExtraData.getBillPaidTypesMap();
      final Document orderItemsInXMLDocument = createNewXMLDocument();
      final Element rootOrderItems = createRootElementForDocument(orderItemsInXMLDocument,
          BlCoreConstants.ORDER_ITEMS_ROOT_ELEMENT);

      if (CollectionUtils.isNotEmpty(orderModel.getEntries())) {
        for (final AbstractOrderEntryModel entryModel : orderModel.getEntries()) {
          final Element rootOrderItem = createRootElementForRootElement(orderItemsInXMLDocument,
              rootOrderItems, BlCoreConstants.ORDER_ITEM_ROOT_ELEMENT);
          if (Objects.nonNull(entryModel.getProduct())) {
            createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
                BlCoreConstants.ORDER_ITEM_PRODUCT_CODE,
                getRequestValue(entryModel.getProduct().getCode()));
            createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
                BlCoreConstants.ORDER_ITEM_PRODUCT_TITLE,
                entryModel.getProduct() instanceof BlSerialProductModel ? getProductTitle(
                    entryModel.getProduct().getCode()) : entryModel.getProduct().getName());
          }
          createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
              BlCoreConstants.ORDER_ITEM_PRODUCT_PHOTO,
              entryModel.getProduct() instanceof BlSerialProductModel ? getProductUrl(
                  entryModel.getProduct().getCode()) : getProductURL(entryModel));
          createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
              BlCoreConstants.ORDER_ITEM_QUANTITY,
              String.valueOf(entryModel.getQuantity()));
          setBillType(billPaidTypesMap, orderItemsInXMLDocument, entryModel, rootOrderItem);
          createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
              BlCoreConstants.BILLING_NOTES, getOrderNotesFromOrderModel(orderModel));
          createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
              BlCoreConstants.ORDER_ITEM_TOTAL_PRICE,
              String.valueOf(getDoubleValueForRequest(entryModel.getTotalPrice())));
        }
      }

      final Transformer transformer = getTransformerFactoryObject();
      final StringWriter writer = new StringWriter();

      //transform document to string
      transformer.transform(new DOMSource(orderItemsInXMLDocument), new StreamResult(writer));
      orderBillPaidData.setOrderItems(writer.getBuffer().toString());

    } catch (final Exception exception) {
      BlLogger.logMessage(LOG, Level.ERROR, POPULATOR_ERROR, exception);
      throw new BlESPIntegrationException(exception.getMessage(),
          LogErrorCodeEnum.ESP_EVENT_POPULATOR_EXCEPTION.getCode(), exception);
    }
  }

  /**
   * This method sets billing charges type.
   *
   * @param billPaidTypesMap
   * @param orderItemsInXMLDocument
   * @param entryModel
   * @param rootOrderItem
   */
  private void setBillType(final Map<String, List<String>> billPaidTypesMap,
      final Document orderItemsInXMLDocument, final AbstractOrderEntryModel entryModel,
      final Element rootOrderItem) {
    if (MapUtils.isNotEmpty(billPaidTypesMap)) {
      if (billPaidTypesMap.containsKey(entryModel.getProduct().getCode())) {
        List<String> billingTypes = billPaidTypesMap.get(entryModel.getProduct().getCode());
        createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
            BlCoreConstants.BILLING_TYPE, getBillingTypes(billingTypes));
      }
    } else {
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
          BlCoreConstants.BILLING_TYPE, StringUtils.EMPTY);
    }
  }
}
