package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlSerialProductModel;
import com.bl.esp.dto.orderpullback.OrderPullBackRequest;
import com.bl.esp.dto.orderpullback.data.OrderPullBackItems;
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
import java.util.Objects;
import javax.xml.transform.Transformer;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.util.Assert;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * This populator created for order pull back added items ESP Event service
 * @author Manikandan
 */
public class BlOrderPullBackRequestPopulator extends ESPEventCommonPopulator<OrderModel, OrderPullBackRequest> {

  private static final Logger LOG = Logger.getLogger(BlOrderPullBackRequestPopulator.class);
  private static final String POPULATOR_ERROR = "Error while populating data for ESP Event";

  /**
   * Populate the  instance with values from the OrderModel.
   *
   * @param order the source object
   * @param orderPullBackRequest the target to fill
   * @throws ConversionException if an error occurs
   */
  @Override
  public void populate(final OrderModel order, final OrderPullBackRequest orderPullBackRequest) throws ConversionException {
    Assert.notNull(order, "Parameter order cannot be null.");
    Assert.notNull(orderPullBackRequest, "Parameter orderPullBackRequest cannot be null.");
    final UserModel userModel = order.getUser();
    if(Objects.nonNull(userModel)) {
      orderPullBackRequest.setContactKey(getRequestValue(userModel.getUid()));
    }
    orderPullBackRequest
        .setEventDefinitionKey(getRequestValue(getConfigurationService().getConfiguration().
            getString(BlCoreConstants.ORDER_PULL_BACK_ADDED_ITEMS_EVENT_DEFINITION_KEY)));
    final OrderPullBackItems data = new OrderPullBackItems();
    populateCommonData(order , data);
    populateOrderData(order , data);
    orderPullBackRequest.setData(data);
  }

  /**
   * This method created to populate order data from order model
   * @param order order model to get the data
   * @param data data to get updated
   */
  private void populateOrderData(final OrderModel order, final OrderPullBackItems data) {
    final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
    data.setOldOrderId(StringUtils.EMPTY);
    data.setStatus(getRequestValue(
        Objects.nonNull(order.getStatus()) ? order.getStatus().getCode() : StringUtils.EMPTY));
    data.setOrdertype(getOrderType(order));
    data.setDateplaced(formatter.format(order.getDate()));
    data.setTemplate(getRequestValue(getConfigurationService().getConfiguration().
        getString(BlCoreConstants.ORDER_PULL_BACK_ADDED_ITEMS_EVENT_TEMPLATE))); // TODO Setting dummy value, once we got the actual value then set actual value one
    if(Objects.nonNull(order.getDeliveryMode())) {
      final ZoneDeliveryModeModel delivery = ((ZoneDeliveryModeModel) order
          .getDeliveryMode());
      data.setShippingmethodtype(getRequestValue(delivery.getShippingGroup().getName()));
      data.setShippingmethod(getRequestValue(delivery.getCode()));
    }
    data.setExpectedshippingdate(formatter.format(order.getRentalStartDate()));
    data.setArrivaldate(formatter.format(order.getRentalStartDate()));
    data.setReturndate(formatter.format(order.getRentalEndDate()));
    populateOrderItemsInXML(order , data);
  }

  /**
   * This method created to populate order
   * @param orderModel order model to get the data
   * @param data date to get updated
   */
  private void populateOrderItemsInXML(final OrderModel orderModel, final OrderPullBackItems data) {
    try {
      final Document orderItemsInXMLDocument = createNewXMLDocument();
      final Element rootOrderItems = createRootElementForDocument(orderItemsInXMLDocument, BlCoreConstants.ITEMS_ROOT_ELEMENT);

      if (CollectionUtils.isNotEmpty(orderModel.getEntries())) {
        for (final AbstractOrderEntryModel entryModel : orderModel.getEntries()) {
          populateOrderDetailsInXMl(entryModel , orderItemsInXMLDocument , rootOrderItems);
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
  private void populateOrderDetailsInXMl(final AbstractOrderEntryModel entryModel, final Document orderItemsInXMLDocument,
      final Element rootOrderItems) {
    final Element rootOrderItem = createRootElementForRootElement(orderItemsInXMLDocument, rootOrderItems, BlCoreConstants.ITEM_ROOT_ELEMENT);
    if (Objects.nonNull(entryModel.getProduct())) {
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlCoreConstants.ORDER_ITEM_PRODUCT_CODE,
          getRequestValue(entryModel.getProduct().getCode()));
      createElementForRootElement(orderItemsInXMLDocument, rootOrderItem, BlCoreConstants.ORDER_ITEM_PRODUCT_TITLE,
          entryModel.getProduct() instanceof BlSerialProductModel ? getProductTitle(entryModel.getProduct().getCode()) :entryModel.getProduct().getName());
    }
  }

}
