package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.esp.dto.newshipping.OrderNewShippingEventRequest;
import com.bl.esp.dto.ordernewshipping.data.OrderNewshippingData;
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
 * This populator is used to populate New Shipping Event Request.
 * @author Avani Patel
 *
 */
public class BlOrderNewShippingRequestPopulator extends ESPEventCommonPopulator<OrderModel, OrderNewShippingEventRequest> {
  private static final org.apache.log4j.Logger LOG = Logger.getLogger(BlOrderNewShippingRequestPopulator.class);
  private static final String POPULATOR_ERROR = "Error while populating data for ESP Event";
  /**
   * Populate the New Shipping Request instance with values from the OrderModel.
   *
   * @param order the source object
   * @param orderNewShippingEventRequest the target to fill
   * @throws ConversionException if an error occurs
   */
  @Override
  public void populate(final OrderModel order,
     final OrderNewShippingEventRequest orderNewShippingEventRequest) throws ConversionException {
    Assert.notNull(order, "Parameter emailId cannot be null.");
    Assert.notNull(orderNewShippingEventRequest, "Parameter contactRequest cannot be null.");

    final UserModel userModel = order.getUser();
    if(Objects.nonNull(userModel)) {
      orderNewShippingEventRequest.setContactKey(getRequestValue(userModel.getUid()));
    }
    orderNewShippingEventRequest
        .setEventDefinitionKey(getRequestValue(getConfigurationService().getConfiguration().
            getString(BlCoreConstants.ORDER_RNEWSHIPPING_EVENT_DEFINITION_KEY)));
    populateOrderNewShippingData(order, orderNewShippingEventRequest);
  }
  /**
   * This method populate order New Shipping data from order model
   * @param orderModel orderModel
   * @param orderNewShippingEventRequest request to be get updated
   */
  private void populateOrderNewShippingData(final OrderModel orderModel, final OrderNewShippingEventRequest orderNewShippingEventRequest) {
    final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
    final OrderNewshippingData data = new OrderNewshippingData();
    populateCommonData(orderModel , data);
    data.setOldorderid(StringUtils.EMPTY);
    data.setTemplate(getRequestValue(getConfigurationService().getConfiguration().getString(BlCoreConstants.ORDER_NEWSHIPPING_EVENT_TEMPLATE)));
    final UserModel userModel = orderModel.getUser();
    if (Objects.nonNull(userModel)) {
      data.setCustomername(getRequestValue(userModel.getName()));
    }
    data.setType(BooleanUtils.isTrue(orderModel.getIsRentalCart()) ? BlCoreConstants.RENTAL : BlCoreConstants.USED_GEAR);
    data.setReplacement(BooleanUtils.isTrue(orderModel.getIsCartUsedForReplacementOrder())
        ? Boolean.TRUE.toString() : Boolean.FALSE.toString());
    data.setStatus(getRequestValue(Objects.nonNull(orderModel.getStatus()) ? orderModel.getStatus().getCode() : StringUtils.EMPTY));
    data.setDateplaced(formatter.format(orderModel.getDate()));
    if(Objects.nonNull(orderModel.getDeliveryMode())) {
      final ZoneDeliveryModeModel delivery = ((ZoneDeliveryModeModel) orderModel
          .getDeliveryMode());
      data.setShippingmethodtype(getRequestValue(delivery.getShippingGroup().getName()));
      data.setShippingmethod(getRequestValue(delivery.getCode()));
      data.setShippingmethodtext(getRequestValue(delivery.getName()));
    }
    data.setArrivaldate(formatter.format(orderModel.getRentalStartDate()));
    data.setReturndate(formatter.format(orderModel.getRentalEndDate()));
    data.setRentalduration((int) getRentalDuration(orderModel));
    data.setTracking("test");// TODO Setting dummy value, once we got the actual value then set actual value one
    populateShippingInfoInXML(orderModel, data);
    orderNewShippingEventRequest.setData(data);
  }
  /**
   * This method populate shipping info into xml format
   * @param orderModel ordermodel
   * @param data data to be set
   */
  private void populateShippingInfoInXML(final OrderModel orderModel, final OrderNewshippingData data) {
    if (Objects.nonNull(orderModel.getDeliveryAddress())) {
      final AddressModel shippingAddress = orderModel.getDeliveryAddress();
      try {
        final Document shippingInfoInXMLDocument = createNewXMLDocument();
        final Element root = createRootElementForDocument(shippingInfoInXMLDocument, BlCoreConstants.SHIPPING_ROOT_ELEMENT);
        createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_FIRST_NAME, getRequestValue(shippingAddress.getFirstname()));
        createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_LAST_NAME, getRequestValue(shippingAddress.getLastname()));
        createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_ORGANIZATION, getRequestValue(shippingAddress.getCompany()));
        createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_ADDRESS_1, getRequestValue(shippingAddress.getLine1()));
        createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_ADDRESS_2, getRequestValue(shippingAddress.getLine2()));
        createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_CITY, getRequestValue(shippingAddress.getTown()));
        createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_STATE,
            Objects.nonNull(shippingAddress.getRegion()) ? shippingAddress.getRegion().getName() : StringUtils.EMPTY);
        createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_ZIP_CODE, getRequestValue(shippingAddress.getPostalcode()));
        createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_PHONE, getRequestValue(shippingAddress.getCellphone()));
        createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_EMAIL, getRequestValue(shippingAddress.getEmail()));
        createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_HOURS, "Mon-Fri: 8:00 AM - 6:00 PM Sat: 10:00 AM - 5:00 PM Sun: Closed");
        createElementForRootElement(shippingInfoInXMLDocument, root, BlCoreConstants.SHIPPING_NOTES, "In the Safeway Shopping Center");

        final Transformer transformer = getTransformerFactoryObject();
        final StringWriter writer = new StringWriter();

        //transform document to string
        transformer.transform(new DOMSource(shippingInfoInXMLDocument), new StreamResult(writer));
        data.setShippinginfo(writer.getBuffer().toString());
      } catch (final Exception exception) {
        BlLogger.logMessage(LOG , Level.ERROR , POPULATOR_ERROR , exception);
        throw new BlESPIntegrationException(exception.getMessage() , LogErrorCodeEnum.ESP_EVENT_POPULATOR_EXCEPTION.getCode() , exception);
      }
    }
  }

}
