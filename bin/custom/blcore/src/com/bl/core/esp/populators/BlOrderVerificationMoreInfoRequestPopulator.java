/**
 *
 */
package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.esp.dto.orderverification.OrderVerificationMoreInfoEventRequest;
import com.bl.esp.dto.orderverification.data.OrderVerificationMoreInfoData;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.text.SimpleDateFormat;
import java.util.Objects;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.Assert;


/**
 * This populator created for order verification more info ESP Event
 *
 * @author Sunil
 */
public class BlOrderVerificationMoreInfoRequestPopulator extends
    ESPEventCommonPopulator<OrderModel, OrderVerificationMoreInfoEventRequest> {

  /**
   * Populate the OrderVerificationMoreInfoEventRequest instance with values from the OrderModel.
   *
   * @param order                                 the source object
   * @param orderVerificationMoreInfoEventRequest the target to fill
   * @throws ConversionException if an error occurs
   */
  @Override
  public void populate(final OrderModel order,
      final OrderVerificationMoreInfoEventRequest orderVerificationMoreInfoEventRequest)
      throws ConversionException {
    Assert.notNull(order, "Parameter emailId cannot be null.");
    Assert
        .notNull(orderVerificationMoreInfoEventRequest, "Parameter contactRequest cannot be null.");

    final UserModel userModel = order.getUser();
    if (Objects.nonNull(userModel)) {
      orderVerificationMoreInfoEventRequest.setContactKey(getRequestValue(userModel.getUid()));
    }
    orderVerificationMoreInfoEventRequest
        .setEventDefinitionKey(getRequestValue(getConfigurationService().getConfiguration().
            getString(BlCoreConstants.ORDER_VERIFICATION_MORE_INFO_EVENT_DEFINITION_KEY)));

    populateOrderData(order, orderVerificationMoreInfoEventRequest);

  }

  /**
   * This method populate order data from order model
   * @param orderModel orderodel
   * @param orderVerificationMoreInfoEventRequest request to be get updated
   */
  private void populateOrderData(final OrderModel orderModel,
      final OrderVerificationMoreInfoEventRequest orderVerificationMoreInfoEventRequest) {

    final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
    final OrderVerificationMoreInfoData data = new OrderVerificationMoreInfoData();

    populateCommonData(orderModel, data);

    data.setOldorderid(getRequestValue(orderModel.getCode()));
    data.setTemplate(getRequestValue(getConfigurationService().getConfiguration()
        .getString(BlCoreConstants.ORDER_VERIFICATION_MORE_INFO_EVENT_TEMPLATE)));
    final UserModel userModel = orderModel.getUser();
    if (Objects.nonNull(userModel)) {
      data.setCustomername(getRequestValue(userModel.getName()));
    }

    data.setStatus(getRequestValue(
        Objects.nonNull(orderModel.getStatus()) ? orderModel.getStatus().getCode()
            : StringUtils.EMPTY));
    data.setDateplaced(formatter.format(orderModel.getDate()));
    if (Objects.nonNull(orderModel.getDeliveryMode())) {
      final ZoneDeliveryModeModel delivery = ((ZoneDeliveryModeModel) orderModel
          .getDeliveryMode());
      data.setShippingmethodtype(getRequestValue(delivery.getShippingGroup().getName()));
      data.setShippingmethod(getRequestValue(delivery.getCode()));
    }

    data.setExpectedshippingdate(formatter.format(orderModel.getActualRentalStartDate()));
    data.setArrivaldate(formatter.format(orderModel.getRentalStartDate()));
    data.setReturndate(formatter.format(orderModel.getRentalEndDate()));
    data.setRentalduration((int) getRentalDuration(orderModel));
    data.setVerificationlevel(1); //TODO NOSONAR setting dummy value here, once we get the confirmation, will set the actual value

    orderVerificationMoreInfoEventRequest.setData(data);
  }

}
