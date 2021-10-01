package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.esp.dto.orderverification.OrderVerificationRequiredEventRequest;
import com.bl.esp.dto.orderverification.data.OrderVerificationRequiredEventData;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.text.SimpleDateFormat;
import java.util.Objects;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.Assert;

/**
 * This populator populates verification required ESP attributes.
 * @author Neeraj Singh
 */
public class BlOrderVerificationRequiredRequestPopulator extends ESPEventCommonPopulator<OrderModel, OrderVerificationRequiredEventRequest>{

  /**
   * Populate the OrderVerificationRequiredEventRequest instance with values from the OrderModel instance.
   *
   * @param orderModel                    the source object
   * @param orderVerificationEventRequest the target to fill
   * @throws ConversionException if an error occurs
   */
  @Override
  public void populate(final OrderModel orderModel,
      final OrderVerificationRequiredEventRequest orderVerificationEventRequest) throws ConversionException {

    Assert.notNull(orderModel, "Parameter emailId cannot be null.");
    Assert.notNull(orderVerificationEventRequest, "Parameter contactRequest cannot be null.");

    final UserModel userModel = orderModel.getUser();
    if(Objects.nonNull(userModel)) {
      orderVerificationEventRequest.setContactKey(getRequestValue(userModel.getUid()));
    }
    orderVerificationEventRequest
        .setEventDefinitionKey(getRequestValue(getConfigurationService().getConfiguration().
            getString(BlCoreConstants.ORDER_VERIFICATION_REQUIRED_EVENT_DEFINITION_KEY)));
    populateOrderData(orderModel, orderVerificationEventRequest);
  }

  /**
   * This method populates order data from order model
   * @param orderModel orderodel
   * @param orderVerificationRequiredEventRequest request to be get updated
   */
  private void populateOrderData(final OrderModel orderModel, final OrderVerificationRequiredEventRequest orderVerificationRequiredEventRequest) {

    final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
    final OrderVerificationRequiredEventData orderVerificationRequiredEventData = new OrderVerificationRequiredEventData();
    populateCommonData(orderModel , orderVerificationRequiredEventData);
    orderVerificationRequiredEventData.setOldOrderId(getRequestValue(orderModel.getCode()));
    orderVerificationRequiredEventData.setTemplate(getRequestValue(getConfigurationService().getConfiguration().
        getString(BlCoreConstants.ORDER_VERIFICATION_REQUIRED_EVENT_TEMPLATE)));
    orderVerificationRequiredEventData.setStatus(getRequestValue(Objects.nonNull(orderModel.getStatus()) ? orderModel.getStatus().getCode() : StringUtils.EMPTY));
    orderVerificationRequiredEventData.setDatePlaced(formatter.format(orderModel.getDate()));
    if(Objects.nonNull(orderModel.getDeliveryMode())) {
      final ZoneDeliveryModeModel zoneDeliveryModeModel = ((ZoneDeliveryModeModel) orderModel
          .getDeliveryMode());
      orderVerificationRequiredEventData.setShippingMethodType(getRequestValue(zoneDeliveryModeModel.getShippingGroup().getName()));
      orderVerificationRequiredEventData.setShippingMethod(getRequestValue(zoneDeliveryModeModel.getCode()));
    }
    if(BooleanUtils.isTrue(orderModel.getIsRentalCart()) && BooleanUtils.isFalse(orderModel.isGiftCardOrder()))
    {
      orderVerificationRequiredEventData
          .setArrivalDate(formatter.format(orderModel.getRentalStartDate()));
      orderVerificationRequiredEventData
          .setReturnDate(formatter.format(orderModel.getRentalEndDate()));
      orderVerificationRequiredEventData.setRentalDuration((int) getRentalDuration(orderModel));
      orderVerificationRequiredEventData.setExpectedShipping(formatter.format(orderModel.getActualRentalStartDate()));
    }
    final UserModel userModel = orderModel.getUser();
    if (Objects.nonNull(userModel)) {
      orderVerificationRequiredEventData.setCustomerName(getRequestValue(userModel.getName()));
    }
    orderVerificationRequiredEventData.setVerificationLevel("1"); // TO-DO setting dummy value, once we get actual value then set actual one.
    orderVerificationRequiredEventData.setVerificationText("verification text"); // TO-DO setting dummy value, once we get actual value then set actual one.
    orderVerificationRequiredEventRequest.setData(orderVerificationRequiredEventData);
  }
}