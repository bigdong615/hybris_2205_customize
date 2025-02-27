/**
 *
 */
package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.esp.dto.orderpaymentdeclined.data.OrderPaymentDeclinedData;
import com.bl.esp.dto.paymentdeclined.OrderPaymentDeclinedEventRequest;
import com.braintree.model.BrainTreePaymentInfoModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.text.SimpleDateFormat;
import java.util.Objects;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.Assert;


/**
 * This populator is used to populate PaymentDeclined Event Request.
 * @author Avani Patel
 *
 */
public class BlOrderPaymentDeclinedRequestPopulator extends ESPEventCommonPopulator<OrderModel, OrderPaymentDeclinedEventRequest>
{
  /**
   * Populate the OrderPaymentDeclinedRequest instance with values from the OrderModel.
   *
   * @param order the source object
   * @param orderConfirmationEventRequest the target to fill
   * @throws ConversionException if an error occurs
   */
  @Override
  public void populate(final OrderModel order,
     final OrderPaymentDeclinedEventRequest orderConfirmationEventRequest)
      throws ConversionException {
    Assert.notNull(order, "Parameter emailId cannot be null.");
    Assert.notNull(orderConfirmationEventRequest, "Parameter contactRequest cannot be null.");
    final UserModel userModel = order.getUser();
    if(Objects.nonNull(userModel)) {
      orderConfirmationEventRequest.setContactKey(getRequestValue(userModel.getUid()));
    }
    orderConfirmationEventRequest
        .setEventDefinitionKey(getRequestValue(getConfigurationService().getConfiguration().
            getString(BlCoreConstants.ORDER_PAYMENT_DECLINED_EVENT_DEFINITION_KEY)));

    populateOrderPaymentDeclinedData(order, orderConfirmationEventRequest);
  }
  /**
   * This method populate order Payment Declined data from order model
   * @param orderModel orderModel
   * @param orderPaymentDeclinedEventRequest request to be get updated
   */

  private void populateOrderPaymentDeclinedData(final OrderModel orderModel, final OrderPaymentDeclinedEventRequest orderPaymentDeclinedEventRequest) {
    final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
    final OrderPaymentDeclinedData data = new OrderPaymentDeclinedData();
    populateCommonData(orderModel , data);
    data.setOldorderid(StringUtils.EMPTY);
    data.setTemplate(getRequestValue(getConfigurationService().getConfiguration().getString(BlCoreConstants.ORDER_PAYMENT_DECLINED_EVENT_TEMPLATE)));
    data.setType(getOrderType(orderModel));
    data.setReplacement(BooleanUtils.isTrue(orderModel.getIsReplacementOrder())
        ? Boolean.TRUE.toString() : Boolean.FALSE.toString());
    data.setStatus(getRequestValue(Objects.nonNull(orderModel.getStatus()) ? orderModel.getStatus().getCode() : StringUtils.EMPTY));
    data.setDateplaced(formatter.format(orderModel.getDate()));
    data.setTotalcost(formatAmount(getDoubleValueForRequest(orderModel.getTotalPrice())));
    if (Objects.nonNull(orderModel.getPaymentInfo())) {
           final BrainTreePaymentInfoModel brainTreePaymentInfoModel = (BrainTreePaymentInfoModel) orderModel.getPaymentInfo();
      data.setPaymenttype(StringUtils.equalsIgnoreCase(BlCoreConstants.PAY_PAL_PROVIDER,brainTreePaymentInfoModel.getPaymentProvider())
          ? BlCoreConstants.PAY_PAL :checkIsGiftCardUsed(getRequestValue(brainTreePaymentInfoModel.getPaymentProvider())));

    }
    else if(StringUtils.isNotBlank(orderModel.getPoNumber())){
      data.setPaymenttype(BlCoreConstants.PO);
    }
    data.setPaymenttext(StringUtils.EMPTY);
    orderPaymentDeclinedEventRequest.setData(data);
  }

}
