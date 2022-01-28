package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.esp.dto.orderextension.OrderExtensionRequest;
import com.bl.esp.dto.orderextension.data.OrderExtensionData;
import com.bl.esp.exception.BlESPIntegrationException;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import com.google.common.util.concurrent.AtomicDouble;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.Objects;
import javax.xml.transform.Transformer;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.util.Assert;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * This populator created to prepare the extend order ESP Event Request
 * @author Manikandan
 */
public class BlExtendOrderRequestPopulator extends ESPEventCommonPopulator<OrderModel, OrderExtensionRequest> {

  private static final org.apache.log4j.Logger LOG = Logger.getLogger(BlExtendOrderRequestPopulator.class);

  private static final String POPULATOR_ERROR = "Error while populating data for ESP Event";


  /**
   * This method created to populate the extend order ESP Event Request
   * @param orderModel order model
   * @param orderExtensionRequest order extension request
   * @throws ConversionException ConversionException
   */
  @Override
  public void populate(final OrderModel orderModel, final OrderExtensionRequest orderExtensionRequest)
      throws ConversionException {

    Assert.notNull(orderModel, "Parameter order cannot be null.");
    Assert.notNull(orderExtensionRequest, "Parameter orderExtensionRequest cannot be null.");

    final UserModel userModel = orderModel.getUser();
    if(Objects.nonNull(userModel)) {
      orderExtensionRequest.setContactKey(getRequestValue(userModel.getUid()));
    }
    orderExtensionRequest
        .setEventDefinitionKey(getRequestValue(getConfigurationService().getConfiguration().
            getString(BlCoreConstants.ORDER_EXTENSION_EVENT_DEFINITION_KEY)));
    populateExtendOrderData(orderModel, orderExtensionRequest);
  }

  /**
   * This method created to populate order extension data
   * @param orderModel order model
   * @param orderExtensionRequest orderExtensionRequest
   */
  private void populateExtendOrderData(final OrderModel orderModel, final OrderExtensionRequest orderExtensionRequest) {

    final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
    final OrderExtensionData data = new OrderExtensionData();
    populateCommonData(orderModel , data);
    data.setOldorderid(StringUtils.EMPTY);
    data.setType(BooleanUtils.isTrue(orderModel.getIsRentalCart()) ? BlCoreConstants.RENTAL : BlCoreConstants.USED_GEAR);
    data.setTemplate(getRequestValue(getConfigurationService().getConfiguration().getString(BlCoreConstants.ORDER_EXTENSION_EVENT_TEMPLATE)));
    data.setStatus(getRequestValue(Objects.nonNull(orderModel.getStatus()) ? orderModel.getStatus().getCode() : StringUtils.EMPTY));
    data.setDateplaced(formatter.format(orderModel.getDate()));
    data.setItemcost(formatAmount(getDoubleValueForRequest(getItemCostFromOrderEntry(orderModel))));
    data.setDamagewaivercost(formatAmount(getDoubleValueForRequest(orderModel.getTotalDamageWaiverCost())));
    data.setSubtotal(formatAmount(getDoubleValueForRequest(orderModel.getSubtotal())));
    data.setShippingamount(formatAmount(getDoubleValueForRequest(orderModel.getDeliveryCost())));
    data.setTaxamount(formatAmount(getDoubleValueForRequest(orderModel.getTotalTax())));
    data.setDiscountamount(formatAmount(getDoubleValueForRequest(orderModel.getTotalDiscounts())));
    data.setTotalcost(formatAmount(getDoubleValueForRequest(orderModel.getTotalPrice())));
    data.setArrivaldate(formatter.format(orderModel.getExtendRentalStartDate()));
    data.setReturndate(formatter.format(orderModel.getExtendRentalEndDate()));
    data.setRentalduration((int) BlDateTimeUtils
        .getDaysBetweenDates(orderModel.getExtendRentalStartDate(), orderModel.getExtendRentalEndDate()));
    final UserModel userModel = orderModel.getUser();
    if (Objects.nonNull(userModel)) {
      data.setCustomername(getRequestValue(userModel.getName()));
    }
    data.setExtensionamount(formatAmount(getDoubleValueForRequest(orderModel.getTotalPrice())));
    populateOrderItemXMLData(orderModel, data);
    orderExtensionRequest.setData(data);
  }


  /**
   * This method created to get item cost from order entry
   * @param orderModel order model
   * @return item cost
   */
  private Double getItemCostFromOrderEntry(final OrderModel orderModel) {
    final AtomicDouble itemCost = new AtomicDouble(0.0);
    orderModel.getEntries().forEach(abstractOrderEntryModel -> itemCost.addAndGet(abstractOrderEntryModel.getTotalPrice()));
    return itemCost.get();
  }

  /**
   * THis method populates order items in xml
   * @param orderModel ordermodel
   * @param data data
   */
  private void populateOrderItemXMLData(final OrderModel orderModel, final OrderExtensionData data) {
    try {
      final Document orderItemsInXMLDocument = createNewXMLDocument();
      final Element rootOrderItems = createRootElementForDocument(orderItemsInXMLDocument, BlCoreConstants.ORDER_ITEMS_ROOT_ELEMENT);

      if (CollectionUtils.isNotEmpty(orderModel.getEntries())) {
        for (final AbstractOrderEntryModel entryModel : orderModel.getEntries()) {
          final Element rootOrderItem = createRootElementForRootElement(orderItemsInXMLDocument, rootOrderItems, BlCoreConstants.ORDER_ITEM_ROOT_ELEMENT);
          if (Objects.nonNull(entryModel.getProduct())) {
            createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlCoreConstants.ORDER_ITEM_PRODUCT_CODE,
                getRequestValue(entryModel.getProduct().getCode()));
            createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlCoreConstants.ORDER_ITEM_PRODUCT_TITLE,
                getRequestValue(entryModel.getProduct().getName()));
          }
          createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlCoreConstants.ORDER_ITEM_PRODUCT_PHOTO,
              getProductURL(entryModel));
          if (Objects.nonNull(entryModel.getBasePrice())) {
            createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlCoreConstants.ORDER_ITEM_RENTAL_PRICE, String.valueOf(entryModel.getBasePrice().doubleValue()));
          }
          createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlCoreConstants.ORDER_ITEM_DAMAGE_WAIVER_PRICE,
              String.valueOf(getDamageWaiverPriceFromEntry(entryModel)));
          createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlCoreConstants.ORDER_ITEM_DAMAGE_WAIVER_TEXT, getDamageWaiverName(entryModel));
          createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlCoreConstants.ORDER_ITEM_TOTAL_PRICE,
              String.valueOf(getDoubleValueForRequest(entryModel.getTotalPrice())));
          createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlCoreConstants.QUANTITY, String.valueOf(entryModel.getQuantity()));
          createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlCoreConstants.TAX, String.valueOf(getDoubleValueForRequest(entryModel.getAvalaraLineTax())));
        }
      }

      final Transformer transformer = getTransformerFactoryObject();
      final StringWriter writer = new StringWriter();

      //transform document to string
      transformer.transform(new DOMSource(orderItemsInXMLDocument), new StreamResult(writer));
      data.setOrderitems(writer.getBuffer().toString());

    } catch (final Exception exception) {
      BlLogger.logMessage(LOG , Level.ERROR , POPULATOR_ERROR , exception);
      throw new BlESPIntegrationException(exception.getMessage() , LogErrorCodeEnum.ESP_EVENT_POPULATOR_EXCEPTION.getCode() , exception);
    }
  }

}
