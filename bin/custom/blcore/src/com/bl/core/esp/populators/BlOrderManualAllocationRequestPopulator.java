package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlSerialProductModel;
import com.bl.esp.dto.manualallocation.OrderManualAllocationEventRequest;
import com.bl.esp.dto.manualallocation.data.OrderManualAllocationData;
import com.bl.esp.exception.BlESPIntegrationException;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.servicelayer.config.ConfigurationService;
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
 * This populator populates manual allocation ESP attributes.
 *
 * @author Neeraj Singh
 */
public class BlOrderManualAllocationRequestPopulator extends
    ESPEventCommonPopulator<OrderModel, OrderManualAllocationEventRequest> {

  private static final Logger LOG = Logger.getLogger(BlOrderManualAllocationRequestPopulator.class);
  private static final String POPULATOR_ERROR = "Error while populating data for ESP Event";

  private ConfigurationService configurationService;

  /**
   * Populate the target instance with values from the source instance.
   *
   * @param orderModel                        the source object
   * @param orderManualAllocationEventRequest the target to fill
   * @throws ConversionException if an error occurs
   */
  @Override
  public void populate(final OrderModel orderModel,
      final OrderManualAllocationEventRequest orderManualAllocationEventRequest)
      throws ConversionException {
    Assert.notNull(orderModel, "Parameter emailId cannot be null.");
    Assert.notNull(orderManualAllocationEventRequest, "Parameter contactRequest cannot be null.");

    final UserModel userModel = orderModel.getUser();
    if (Objects.nonNull(userModel)) {
      orderManualAllocationEventRequest.setContactKey(getRequestValue(userModel.getUid()));
    }
    orderManualAllocationEventRequest
        .setEventDefinitionKey(getRequestValue(getConfigurationService().getConfiguration().
            getString(BlCoreConstants.ORDER_MANUAL_ALLOCATION_EVENT_DEFINITION_KEY)));
    populateManualAllocationData(orderModel, orderManualAllocationEventRequest);

  }

  /**
   * This method populates manual allocation data from order model
   *
   * @param orderModel                        orderodel
   * @param orderManualAllocationEventRequest request to be get updated
   */
  private void populateManualAllocationData(final OrderModel orderModel,
      final OrderManualAllocationEventRequest orderManualAllocationEventRequest) {

    final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
    final OrderManualAllocationData orderManualAllocationData = new OrderManualAllocationData();
    orderManualAllocationData.setOrderid(orderModel.getCode());
    orderManualAllocationData
        .setSubscriberid(getConfigurationService().getConfiguration().getString(
            BlCoreConstants.ORDER_MANUAL_ALLOCATION_SUBSCRIBER_ID));
    orderManualAllocationData.setOldOrderId(StringUtils.EMPTY);
    orderManualAllocationData
        .setTemplate(getRequestValue(getConfigurationService().getConfiguration().
            getString(BlCoreConstants.ORDER_MANUAL_ALLOCATION_EVENT_TEMPLATE)));
    orderManualAllocationData.setStatus(getRequestValue(
        Objects.nonNull(orderModel.getStatus()) ? orderModel.getStatus().getCode()
            : StringUtils.EMPTY));
    orderManualAllocationData.setOrderType(getOrderType(orderModel));
    orderManualAllocationData.setDatePlaced(formatter.format(orderModel.getDate()));
    if (Objects.nonNull(orderModel.getDeliveryMode())) {
      final ZoneDeliveryModeModel zoneDeliveryModeModel = ((ZoneDeliveryModeModel) orderModel
          .getDeliveryMode());
      orderManualAllocationData.setShippingMethodType(
          getRequestValue(zoneDeliveryModeModel.getShippingGroup().getName()));
      orderManualAllocationData.setShippingMethod(getRequestValue(zoneDeliveryModeModel.getCode()));
    } else {
      orderManualAllocationData.setShippingMethodType(StringUtils.EMPTY);
      orderManualAllocationData.setShippingMethod(StringUtils.EMPTY);
    }
    if (BooleanUtils.isTrue(orderModel.getIsRentalCart()) && BooleanUtils
        .isFalse(orderModel.isGiftCardOrder())) {
      orderManualAllocationData
          .setExpectedShipping(formatter.format(orderModel.getActualRentalStartDate()));
      orderManualAllocationData.setArrivalDate(formatter.format(orderModel.getRentalStartDate()));
      orderManualAllocationData.setReturnDate(formatter.format(orderModel.getRentalEndDate()));
    }

    populateOrderItemsInXML(orderModel, orderManualAllocationData);
    orderManualAllocationEventRequest.setData(orderManualAllocationData);
  }

  /**
   * This method populates item info details
   *
   * @param orderModel
   * @param orderManualAllocationData
   */
  private void populateOrderItemsInXML(final OrderModel orderModel,
      final OrderManualAllocationData orderManualAllocationData) {
    try {
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
              BlCoreConstants.ORDER_ITEM_QUANTITY,
              String.valueOf(entryModel.getQuantity()));
          createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
              BlCoreConstants.SERIAL_ALLOCATED,
              String.valueOf(isSerialAllocated(entryModel)));
          createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
              BlCoreConstants.UNALLOCATED_QUANTITY,
              String.valueOf(entryModel.getUnAllocatedQuantity()));
        }
      }

      final Transformer transformer = getTransformerFactoryObject();
      final StringWriter writer = new StringWriter();

      //transform document to string
      transformer.transform(new DOMSource(orderItemsInXMLDocument), new StreamResult(writer));
      orderManualAllocationData.setOrderItems(writer.getBuffer().toString());

    } catch (final Exception exception) {
      BlLogger.logMessage(LOG, Level.ERROR, POPULATOR_ERROR, exception);
      throw new BlESPIntegrationException(exception.getMessage(),
          LogErrorCodeEnum.ESP_EVENT_POPULATOR_EXCEPTION.getCode(), exception);
    }
  }

  /**
   * It returns string value Yes or No, based on comparison between unAllocatedQuantity and quantity
   * of orderEntry.
   *
   * @param abstractOrderEntryModel
   * @return Yes/No
   */
  private String isSerialAllocated(final AbstractOrderEntryModel abstractOrderEntryModel) {
    String isSerialAllocated = BlCoreConstants.SERIAL_ALLOCATED_YES;
    if (abstractOrderEntryModel.getUnAllocatedQuantity() == abstractOrderEntryModel.getQuantity()) {
      isSerialAllocated = BlCoreConstants.SERIAL_ALLOCATED_NO;
    }
    return isSerialAllocated;
  }

  @Override
  public ConfigurationService getConfigurationService() {
    return configurationService;
  }

  @Override
  public void setConfigurationService(
      ConfigurationService configurationService) {
    this.configurationService = configurationService;
  }
}
