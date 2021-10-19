package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.esp.dto.orderdeposit.OrderDepositRequest;
import com.bl.esp.dto.orderdeposit.data.OrderDepositData;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.SimpleDateFormat;
import java.util.List;
import java.util.Objects;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.Assert;

/**
 * This populator created for order deposit ESP Event
 * @author Manikandan
 */
public class BlOrderDepositRequestPopulator extends ESPEventCommonPopulator<OrderModel, OrderDepositRequest> {

  /**
   *  This method created to populate order deposit ESP event
   * @param orderModel ordermodel
   * @param orderDepositRequest orderDepositRequest
   * @throws ConversionException conversionException
   */
  @Override
  public void populate(final OrderModel orderModel, final OrderDepositRequest orderDepositRequest)
      throws ConversionException {

    Assert.notNull(orderModel, "Parameter order cannot be null.");
    Assert.notNull(orderDepositRequest, "Parameter orderDepositRequest cannot be null.");

    final UserModel userModel = orderModel.getUser();
    if(Objects.nonNull(userModel)) {
      orderDepositRequest.setContactKey(getRequestValue(userModel.getUid()));
    }
    orderDepositRequest
        .setEventDefinitionKey(getRequestValue(getConfigurationService().getConfiguration().
            getString(BlCoreConstants.ORDER_DEPOSIT_EVENT_DEFINITION_KEY)));
    populateOrderDepositData(orderModel, orderDepositRequest);

  }



  /**
   * This method populate order data from order model
   * @param orderModel orderodel
   * @param orderDepositRequest request to be get updated
   */
  private void populateOrderDepositData(final OrderModel orderModel, final OrderDepositRequest orderDepositRequest) {

    final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
    final OrderDepositData data = new OrderDepositData();
    populateCommonData(orderModel , data);
    data.setOldorderid(StringUtils.EMPTY);
    data.setTemplate(getRequestValue(getConfigurationService().getConfiguration().getString(BlCoreConstants.ORDER_DEPOSIT_EVENT_TEMPLATE)));

    data.setStatus(getRequestValue(Objects.nonNull(orderModel.getStatus()) ? orderModel.getStatus().getCode() : StringUtils.EMPTY));
    data.setDateplaced(formatter.format(orderModel.getDate()));
    if(Objects.nonNull(orderModel.getDeliveryMode())) {
      final ZoneDeliveryModeModel delivery = ((ZoneDeliveryModeModel) orderModel
          .getDeliveryMode());
      data.setShippingmethodtype(getRequestValue(delivery.getShippingGroup().getName()));
      data.setShippingmethod(getRequestValue(delivery.getCode()));
    }else {
      data.setShippingmethodtype(StringUtils.EMPTY);
      data.setShippingmethod(StringUtils.EMPTY);
    }

    if(BooleanUtils.isTrue(orderModel.getIsRentalCart()) && BooleanUtils.isFalse(orderModel.isGiftCardOrder())) {
      data.setExpectedshipping(formatter.format(orderModel.getActualRentalStartDate()));
      data.setArrivaldate(formatter.format(orderModel.getRentalStartDate()));
      data.setReturndate(formatter.format(orderModel.getRentalEndDate()));
      data.setRentalduration((int) getRentalDuration(orderModel));
    }
    final UserModel userModel = orderModel.getUser();
    if (Objects.nonNull(userModel)) {
      data.setCustomername(getRequestValue(userModel.getName()));
    }
    data.setVerificationlevel(Integer.valueOf(orderModel.getVerificationLevel()));
    data.setVerificationtext(StringUtils.EMPTY);
    final List<PaymentTransactionModel> paymentTransactionModelList = orderModel.getDepositPaymentTransactions();
    if(Objects.nonNull(paymentTransactionModelList)){
      final BigDecimal depositAmount = paymentTransactionModelList.get(paymentTransactionModelList.size()-1).getPlannedAmount().setScale(2, RoundingMode.HALF_DOWN);
      data.setDepositamount(depositAmount);
    }
    orderDepositRequest.setData(data);
  }

}
