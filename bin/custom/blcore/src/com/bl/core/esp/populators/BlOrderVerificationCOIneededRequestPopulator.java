/**
 *
 */
package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.esp.dto.orderverification.OrderVerificationCOIneededEventRequest;
import com.bl.esp.dto.orderverification.data.OrderVerificationCOIneededData;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.Objects;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.Assert;


/**
 * This populator created for order verification coi needed ESP Event
 *
 * @author Sunil
 */
public class BlOrderVerificationCOIneededRequestPopulator extends
    ESPEventCommonPopulator<OrderModel, OrderVerificationCOIneededEventRequest> {

  /**
   * Populate the OrderVerificationCOIneededEventRequest instance with values from the OrderModel.
   *
   * @param order                                 the source object
   * @param orderVerificationCOIneededEventRequest the target to fill
   * @throws ConversionException if an error occurs
   */
  @Override
  public void populate(final OrderModel order,
      final OrderVerificationCOIneededEventRequest orderVerificationCOIneededEventRequest)
      throws ConversionException {
    Assert.notNull(order, "Parameter emailId cannot be null.");
    Assert
        .notNull(orderVerificationCOIneededEventRequest, "Parameter contactRequest cannot be null.");

    final UserModel userModel = order.getUser();
    if (Objects.nonNull(userModel)) {
      orderVerificationCOIneededEventRequest.setContactKey(getRequestValue(userModel.getUid()));
    }
    orderVerificationCOIneededEventRequest
        .setEventDefinitionKey(getRequestValue(getConfigurationService().getConfiguration().
            getString(BlCoreConstants.ORDER_VERIFICATION_COI_NEEDED_EVENT_DEFINITION_KEY)));

    populateOrderData(order, orderVerificationCOIneededEventRequest);

  }

  /**
   * This method populate order data from order model
   * @param orderModel orderodel
   * @param orderVerificationCOIneededEventRequest request to be get updated
   */
  private void populateOrderData(final OrderModel orderModel,
      final OrderVerificationCOIneededEventRequest orderVerificationCOIneededEventRequest) {

    final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
    final OrderVerificationCOIneededData data = new OrderVerificationCOIneededData();

    populateCommonData(orderModel, data);

    data.setOldorderid(StringUtils.EMPTY);
    data.setTemplate(getRequestValue(getConfigurationService().getConfiguration()
        .getString(BlCoreConstants.ORDER_VERIFICATION_COI_NEEDED_EVENT_TEMPLATE)));
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
    data.setVerificationlevel(Integer.valueOf(orderModel.getVerificationLevel()));
    data.setVerificationtext("Dummy verification text"); //TODO NOSONAR setting dummy value here, once we get the confirmation, will set the actual value
    data.setTotalvalue(getTotalValueFromOrder(orderModel));
    data.setReturningcustomer(String.valueOf(isReturningCustomer(orderModel)));
    orderVerificationCOIneededEventRequest.setData(data);
  }

}
