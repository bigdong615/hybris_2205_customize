package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.esp.dto.pickedup.OrderPickedUpEventRequest;
import com.bl.esp.dto.pickedup.data.OrderPickedUpEventData;
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
 * This populator is used to populate PickedUp Event Request.
 *
 * @author Neeraj Singh
 */
public class BlOrderPickedUpRequestPopulator extends
    ESPEventCommonPopulator<OrderModel, OrderPickedUpEventRequest> {

  /**
   * Populate the OrderPickedUpEventRequest instance with values from the OrderModel instance.
   *
   * @param orderModel                the source object
   * @param orderPickedUpEventRequest the target to fill
   * @throws ConversionException if an error occurs
   */
  @Override
  public void populate(final OrderModel orderModel,
      final OrderPickedUpEventRequest orderPickedUpEventRequest)
      throws ConversionException {
    Assert.notNull(orderModel, "Parameter emailId cannot be null.");
    Assert.notNull(orderPickedUpEventRequest, "Parameter contactRequest cannot be null.");

    final UserModel userModel = orderModel.getUser();
    if (Objects.nonNull(userModel)) {
      orderPickedUpEventRequest.setContactKey(getRequestValue(userModel.getUid()));
    }
    orderPickedUpEventRequest
        .setEventDefinitionKey(getRequestValue(getConfigurationService().getConfiguration().
            getString(BlCoreConstants.ORDER_PICKEDUP_EVENT_DEFINITION_KEY)));
    populatePickedUpData(orderModel, orderPickedUpEventRequest);
  }

  /**
   * This method populate order PickedUp data from order model
   *
   * @param orderModel                orderModel
   * @param orderPickedUpEventRequest request to be get updated
   */
  private void populatePickedUpData(final OrderModel orderModel,
      final OrderPickedUpEventRequest orderPickedUpEventRequest) {
    final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
    final OrderPickedUpEventData data = new OrderPickedUpEventData();
    populateCommonData(orderModel, data);
    data.setOldOrderId(StringUtils.EMPTY);
    data.setTemplate(getRequestValue(getConfigurationService().getConfiguration()
        .getString(BlCoreConstants.ORDER_PICKEDUP_EVENT_TEMPLATE)));
    final UserModel userModel = orderModel.getUser();
    if (Objects.nonNull(userModel)) {
      data.setCustomerName(getRequestValue(userModel.getName()));
    }
    data.setType(getOrderType(orderModel));
    data.setReplacement(BooleanUtils.isTrue(orderModel.getIsReplacementOrder())
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
    if(BooleanUtils.isTrue(orderModel.getIsRentalOrder()) && BooleanUtils.isFalse(orderModel.isGiftCardOrder())) {
      data.setArrivalDate(formatter.format(orderModel.getRentalStartDate()));
      data.setReturnDate(formatter.format(orderModel.getRentalEndDate()));
      data.setRentalDuration((int) getRentalDuration(orderModel));
    }
    orderPickedUpEventRequest.setData(data);
  }
}
