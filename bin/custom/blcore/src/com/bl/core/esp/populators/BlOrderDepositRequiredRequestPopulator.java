package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.esp.dto.depositrequired.OrderDepositRequiredEventRequest;
import com.bl.esp.dto.depositrequired.data.OrderDepositRequired;
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
 * This class created to populate Order Deposit Required Request
 * @author Manikandan
 */
public class BlOrderDepositRequiredRequestPopulator extends ESPEventCommonPopulator<OrderModel, OrderDepositRequiredEventRequest> {

  /**
   * This method created to populate Order Deposit Required Request data from order model
   * @param order ordermodel
   * @param orderDepositRequiredEventRequest request which sends to ESP Event
   * @throws ConversionException ConversionException
   */
  @Override
  public void populate(final OrderModel order, final OrderDepositRequiredEventRequest orderDepositRequiredEventRequest)
      throws ConversionException {

    Assert.notNull(order, "Parameter order cannot be null.");
    Assert.notNull(orderDepositRequiredEventRequest, "Parameter orderConfirmationEventRequest cannot be null.");

    final UserModel userModel = order.getUser();
    if(Objects.nonNull(userModel)) {
      orderDepositRequiredEventRequest.setContactKey(getRequestValue(userModel.getUid()));
    }
    orderDepositRequiredEventRequest
        .setEventDefinitionKey(getRequestValue(getConfigurationService().getConfiguration().
            getString(BlCoreConstants.ORDER_DEPOSIT_REQUIRED_EVENT_DEFINITION_KEY)));
    populateOrderData(order, orderDepositRequiredEventRequest);

  }

  /**
   * This method created to populate data from ordermodel
   * @param orderModel ordermodel
   * @param orderDepositRequiredEventRequest orderDepositRequiredEventRequest
   */
  private void populateOrderData(final OrderModel orderModel, final OrderDepositRequiredEventRequest orderDepositRequiredEventRequest) {
    final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
    final OrderDepositRequired data = new OrderDepositRequired();
    populateCommonData(orderModel , data);
    data.setOldOrderId(StringUtils.EMPTY);
    data.setTemplate(getRequestValue(getConfigurationService().getConfiguration().getString(BlCoreConstants.ORDER_DEPOSIT_REQUIRED_EVENT_TEMPLATE)));
    final UserModel userModel = orderModel.getUser();
    if (Objects.nonNull(userModel)) {
      data.setCustomername(getRequestValue(userModel.getName()));
    }

    data.setStatus(getRequestValue(getOrderStatus(orderModel)));
    data.setDatePlaced(formatter.format(orderModel.getDate()));
    if(Objects.nonNull(orderModel.getDeliveryMode())) {
      final ZoneDeliveryModeModel delivery = ((ZoneDeliveryModeModel) orderModel
          .getDeliveryMode());
      data.setShippingMethodType(getRequestValue(delivery.getShippingGroup().getName()));
      data.setShippingMethod(getRequestValue(delivery.getCode()));
    }

    if(BooleanUtils.isTrue(orderModel.getIsRentalCart()) && BooleanUtils.isFalse(
        orderModel.isGiftCardOrder())) {
      data.setExpectedShipping(formatter.format(orderModel.getRentalStartDate()));
      data.setArrivalDate(formatter.format(orderModel.getRentalStartDate()));
      data.setReturnDate(formatter.format(orderModel.getRentalEndDate()));
      data.setRentalduration((int) getRentalDuration(orderModel));
    }
    data.setVerificationlevel(orderModel.getVerificationLevel());
    data.setDepositamount(formatAmount(orderModel.getDepositAmount()));
    data.setVerificationtext(StringUtils.EMPTY);
    orderDepositRequiredEventRequest.setData(data);
  }

  /**
   * This method created to get order status from order model
   * @param orderModel orderModel
   * @return String
   */
  private String getOrderStatus(final OrderModel orderModel) {
    return Objects.isNull(orderModel.getStatus()) ? StringUtils.EMPTY : orderModel.getStatus().getCode();
  }

}
