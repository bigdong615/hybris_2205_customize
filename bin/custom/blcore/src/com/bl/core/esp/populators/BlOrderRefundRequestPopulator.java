package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.esp.dto.refund.OrderRefundEventRequest;
import com.bl.esp.dto.refund.data.OrderRefundData;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.text.SimpleDateFormat;
import java.util.Objects;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.Assert;

/**
 * This populator is used to populate Refund Event Request.
 * @author Avani Patel
 */
public class BlOrderRefundRequestPopulator extends ESPEventCommonPopulator<OrderModel, OrderRefundEventRequest> {

  /**
   * Populate the OrderRefund Request instance with values from the OrderModel.
   *
   * @param order
   *           the source object
   * @param orderRefundEventRequest
   *           the target to fill
   * @throws ConversionException
   *            if an error occurs
   */
  @Override
  public void populate(final OrderModel order,final OrderRefundEventRequest orderRefundEventRequest)
      throws ConversionException {
    Assert.notNull(order, "Parameter emailId cannot be null.");
    Assert.notNull(orderRefundEventRequest, "Parameter contactRequest cannot be null.");

    final UserModel userModel = order.getUser();
    if (Objects.nonNull(userModel))
    {
      orderRefundEventRequest.setContactKey(getRequestValue(userModel.getUid()));
    }
    orderRefundEventRequest.setEventDefinitionKey(getRequestValue(
        getConfigurationService().getConfiguration().getString(BlCoreConstants.ORDER_REFUND_EVENT_DEFINITION_KEY)));
    populateOrderRefunddData(order, orderRefundEventRequest);
  }
  /**
   * This method populate order data from order model
   *
   * @param orderModel
   *           orderModel
   * @param orderRefundEventRequest
   *           request to be get updated
   */

  private void populateOrderRefunddData(final OrderModel orderModel, final OrderRefundEventRequest orderRefundEventRequest) {
    final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
    final OrderRefundData data = new OrderRefundData();
    populateCommonData(orderModel, data);
    data.setOldorderid(StringUtils.EMPTY);
    data.setTemplate(getRequestValue(
        getConfigurationService().getConfiguration().getString(BlCoreConstants.ORDER_REFUND_EVENT_TEMPLATE)));
    data.setDateplaced(formatter.format(orderModel.getDate()));
    data.setEmail(orderModel.getUser().getUid());
    orderRefundEventRequest.setData(data);
  }
}
