package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.esp.dto.orderverification.OrderVerificationCompletedEventRequest;
import com.bl.esp.dto.orderverification.data.OrderVerificationCompletedEventData;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.text.SimpleDateFormat;
import java.util.Objects;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.Assert;

/**
 * This populator populates verification completed ESP attributes.
 * @author Neeraj Singh
 */
public class BlOrderVerificationCompletedRequestPopulator extends ESPEventCommonPopulator<OrderModel, OrderVerificationCompletedEventRequest>{

  /**
   * Populate the OrderVerificationCompletedEventRequest instance with values from the OrderModel instance.
   *
   * @param orderModel                    the source object
   * @param orderVerificationCompletedEventRequest the target to fill
   * @throws ConversionException if an error occurs
   */
  @Override
  public void populate(final OrderModel orderModel,
      final OrderVerificationCompletedEventRequest orderVerificationCompletedEventRequest) throws ConversionException {

    Assert.notNull(orderModel, "Parameter emailId cannot be null.");
    Assert.notNull(orderVerificationCompletedEventRequest, "Parameter contactRequest cannot be null.");

    final UserModel userModel = orderModel.getUser();
    if(Objects.nonNull(userModel)) {
      orderVerificationCompletedEventRequest.setContactKey(getRequestValue(userModel.getUid()));
    }
    orderVerificationCompletedEventRequest
        .setEventDefinitionKey(getRequestValue(getConfigurationService().getConfiguration().
            getString(BlCoreConstants.ORDER_VERIFICATION_COMPLETED_EVENT_DEFINITION_KEY)));
    populateOrderData(orderModel, orderVerificationCompletedEventRequest);
  }

  /**
   * This method populates order data from order model
   * @param orderModel orderodel
   * @param orderVerificationCompletedEventRequest request to be get updated
   */
  private void populateOrderData(final OrderModel orderModel, final OrderVerificationCompletedEventRequest orderVerificationCompletedEventRequest) {

    final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
    final OrderVerificationCompletedEventData orderVerificationCompletedEventData = new OrderVerificationCompletedEventData();
    populateCommonData(orderModel , orderVerificationCompletedEventData);
    orderVerificationCompletedEventData.setOldOrderId(getRequestValue(orderModel.getCode()));
    orderVerificationCompletedEventData.setTemplate(getRequestValue(getConfigurationService().getConfiguration().
        getString(BlCoreConstants.ORDER_VERIFICATION_COMPLETED_EVENT_TEMPLATE)));
    orderVerificationCompletedEventData.setStatus(getRequestValue(Objects.nonNull(orderModel.getStatus()) ? orderModel.getStatus().getCode() : StringUtils.EMPTY));
    orderVerificationCompletedEventData.setDatePlaced(formatter.format(orderModel.getDate()));
    if(Objects.nonNull(orderModel.getDeliveryMode())) {
      final ZoneDeliveryModeModel zoneDeliveryModeModel = ((ZoneDeliveryModeModel) orderModel
          .getDeliveryMode());
      orderVerificationCompletedEventData.setShippingMethodType(getRequestValue(zoneDeliveryModeModel.getShippingGroup().getName()));
      orderVerificationCompletedEventData.setShippingMethod(getRequestValue(zoneDeliveryModeModel.getCode()));
      orderVerificationCompletedEventData.setExpectedShipping(formatter.format(orderModel.getActualRentalStartDate()));
    }
    orderVerificationCompletedEventData.setArrivalDate(formatter.format(orderModel.getRentalStartDate()));
    orderVerificationCompletedEventData.setReturnDate(formatter.format(orderModel.getRentalEndDate()));
    orderVerificationCompletedEventData.setRentalDuration((int) getRentalDuration(orderModel));
    final UserModel userModel = orderModel.getUser();
    if (Objects.nonNull(userModel)) {
      orderVerificationCompletedEventData.setCustomerName(getRequestValue(userModel.getName()));
    }
    orderVerificationCompletedEventData.setVerificationLevel("1"); // TO-DO setting dummy value, once we get actual value then set actual one.
    orderVerificationCompletedEventRequest.setData(orderVerificationCompletedEventData);
  }
}