package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.esp.dto.shipped.OrderShippedEventRequest;
import com.bl.esp.dto.shipped.data.OrderShippedEventData;
import com.bl.esp.exception.BlESPIntegrationException;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.Objects;
import javax.xml.transform.Transformer;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.util.Assert;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * This populator is used to populate  Shipped Event Request.
 *
 * @author Neeraj Singh
 */
public class BlOrderShippedRequestPopulator extends
    ESPEventCommonPopulator<OrderModel, OrderShippedEventRequest> {

  private static final Logger LOG = Logger.getLogger(BlOrderShippedRequestPopulator.class);
  private static final String POPULATOR_ERROR = "Error while populating data for ESP Event";

  /**
   * Populate the OrderShippedEventRequest instance with values from the OrderModel instance.
   *
   * @param orderModel               the source object
   * @param orderShippedEventRequest the target to fill
   * @throws ConversionException if an error occurs
   */
  @Override
  public void populate(final OrderModel orderModel,
      final OrderShippedEventRequest orderShippedEventRequest)
      throws ConversionException {
    Assert.notNull(orderModel, "Parameter emailId cannot be null.");
    Assert.notNull(orderShippedEventRequest, "Parameter contactRequest cannot be null.");

    final UserModel userModel = orderModel.getUser();
    if (Objects.nonNull(userModel)) {
      orderShippedEventRequest.setContactKey(getRequestValue(userModel.getUid()));
    }
    orderShippedEventRequest
        .setEventDefinitionKey(getRequestValue(getConfigurationService().getConfiguration().
            getString(BlCoreConstants.ORDER_SHIPPED_EVENT_DEFINITION_KEY)));
    populateShippedData(orderModel, orderShippedEventRequest);
  }

  /**
   * This method populate order Shipped data from order model
   *
   * @param orderModel               orderModel
   * @param orderShippedEventRequest request to be get updated
   */
  private void populateShippedData(final OrderModel orderModel,
      final OrderShippedEventRequest orderShippedEventRequest) {
    final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
    final OrderShippedEventData data = new OrderShippedEventData();
    populateCommonData(orderModel, data);
    data.setOldOrderId(StringUtils.EMPTY);
    data.setTemplate(getRequestValue(getConfigurationService().getConfiguration()
        .getString(BlCoreConstants.ORDER_SHIPPED_EVENT_TEMPLATE)));
    final UserModel userModel = orderModel.getUser();
    if (Objects.nonNull(userModel)) {
      data.setCustomerName(getRequestValue(userModel.getName()));
    }
    data.setType(getOrderType(orderModel));
    data.setReplacement(BooleanUtils.isTrue(orderModel.getIsCartUsedForReplacementOrder())
        ? Boolean.TRUE.toString() : Boolean.FALSE.toString());
    data.setStatus(getRequestValue(
        Objects.nonNull(orderModel.getStatus()) ? orderModel.getStatus().getCode()
            : StringUtils.EMPTY));
    data.setDatePlaced(formatter.format(orderModel.getDate()));
    if (Objects.nonNull(orderModel.getDeliveryMode())) {
      final ZoneDeliveryModeModel delivery = ((ZoneDeliveryModeModel) orderModel
          .getDeliveryMode());
      data.setShippingMethodType(getRequestValue(delivery.getShippingGroup().getName()));
      data.setShippingMethod(getRequestValue(delivery.getCode()));
      data.setShippingMethodText(getRequestValue(delivery.getName()));
    }else{
      data.setShippingMethodType(StringUtils.EMPTY);
      data.setShippingMethod(StringUtils.EMPTY);
      data.setShippingMethodText(StringUtils.EMPTY);
    }
   if(BooleanUtils.isTrue(orderModel.getIsRentalCart()) && BooleanUtils.isFalse(orderModel.isGiftCardOrder())) {
      data.setArrivalDate(formatter.format(orderModel.getRentalStartDate()));
      data.setReturnDate(formatter.format(orderModel.getRentalEndDate()));
      data.setRentalDuration((int) getRentalDuration(orderModel));
    }
    data.setTrackingString(
        "test");// TODO Setting dummy value, once we got the actual value then set actual value one
    populateShippingInfoInXML(orderModel, data);
    orderShippedEventRequest.setData(data);
  }

  /**
   * This method populate shipping info into xml format
   *
   * @param orderModel            ordermodel
   * @param orderShippedEventData data to be set
   */
  private void populateShippingInfoInXML(final OrderModel orderModel,
      final OrderShippedEventData orderShippedEventData) {
    if (Objects.nonNull(orderModel.getDeliveryAddress())) {
      final AddressModel shippingAddress = orderModel.getDeliveryAddress();
      try {
        final Document shippingInfoInXMLDocument = createNewXMLDocument();
        final Element root = createRootElementForDocument(shippingInfoInXMLDocument,
            BlCoreConstants.SHIPPING_ROOT_ELEMENT);
        createElementForRootElement(shippingInfoInXMLDocument, root,
            BlCoreConstants.SHIPPING_FIRST_NAME, getRequestValue(shippingAddress.getFirstname()));
        createElementForRootElement(shippingInfoInXMLDocument, root,
            BlCoreConstants.SHIPPING_LAST_NAME, getRequestValue(shippingAddress.getLastname()));
        createElementForRootElement(shippingInfoInXMLDocument, root,
            BlCoreConstants.SHIPPING_ORGANIZATION, getRequestValue(shippingAddress.getCompany()));
        createElementForRootElement(shippingInfoInXMLDocument, root,
            BlCoreConstants.SHIPPING_ADDRESS_1, getRequestValue(shippingAddress.getLine1()));
        createElementForRootElement(shippingInfoInXMLDocument, root,
            BlCoreConstants.SHIPPING_ADDRESS_2, getRequestValue(shippingAddress.getLine2()));
        createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_CITY,
            getRequestValue(shippingAddress.getTown()));
        createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_STATE,
            Objects.nonNull(shippingAddress.getRegion()) ? shippingAddress.getRegion().getName()
                : StringUtils.EMPTY);
        createElementForRootElement(shippingInfoInXMLDocument, root,
            BlCoreConstants.SHIPPING_ZIP_CODE, getRequestValue(shippingAddress.getPostalcode()));
        createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_PHONE,
            getRequestValue(shippingAddress.getCellphone()));
        createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_EMAIL,
            getRequestValue(shippingAddress.getEmail()));
        createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_HOURS,
            "Mon-Fri: 8:00 AM - 6:00 PM Sat: 10:00 AM - 5:00 PM Sun: Closed");
        createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_NOTES,
            "In the Safeway Shopping Center");

        final Transformer transformer = getTransformerFactoryObject();
        final StringWriter writer = new StringWriter();

        //transform document to string
        transformer.transform(new DOMSource(shippingInfoInXMLDocument), new StreamResult(writer));
        orderShippedEventData.setShippingInfo(writer.getBuffer().toString());
      } catch (final Exception exception) {
        BlLogger.logMessage(LOG, Level.ERROR, POPULATOR_ERROR, exception);
        throw new BlESPIntegrationException(exception.getMessage(),
            LogErrorCodeEnum.ESP_EVENT_POPULATOR_EXCEPTION.getCode(), exception);
      }
    }
  }
}
