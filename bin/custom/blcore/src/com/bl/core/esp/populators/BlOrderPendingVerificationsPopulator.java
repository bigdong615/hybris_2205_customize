package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.esp.dto.orderdeposit.OrderDepositRequest;
import com.bl.esp.dto.orderdeposit.data.OrderDepositData;
import com.bl.esp.dto.pendingverification.PendingVerificationEventRequest;
import com.bl.esp.dto.pendingverification.data.PendingVerification;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.Assert;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.SimpleDateFormat;
import java.util.List;
import java.util.Objects;

/**
 * This populator created for order deposit ESP Event
 * @author Jyoti Swamy
 */
public class BlOrderPendingVerificationsPopulator extends ESPEventCommonPopulator<OrderModel, PendingVerificationEventRequest> {

  /**
   *  This method created to populate order deposit ESP event
   * @param orderModel ordermodel
   * @param pendingVerificationEventRequest orderDepositRequest
   * @throws ConversionException conversionException
   */
  @Override
  public void populate(final OrderModel orderModel, final PendingVerificationEventRequest pendingVerificationEventRequest)
      throws ConversionException {

    Assert.notNull(orderModel, "Parameter order cannot be null.");
    Assert.notNull(pendingVerificationEventRequest, "Parameter pendingVerificationEventRequest cannot be null.");

    final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
    final UserModel userModel = orderModel.getUser();
    if(Objects.nonNull(userModel)) {
      pendingVerificationEventRequest.setContactKey(getRequestValue(userModel.getUid()));
    }
    pendingVerificationEventRequest
        .setEventDefinitionKey(getRequestValue(getConfigurationService().getConfiguration().
            getString(BlCoreConstants.PENDING_VERIFICATION_EVENT_DEFINITION_KEY)));
    PendingVerification data=new PendingVerification();
    populateCommonData(orderModel , data);
    data.setStatus(getRequestValue(getOrderStatus(orderModel)));
    data.setDateplaced(formatter.format(orderModel.getDate()));
    if(Objects.nonNull(orderModel.getDeliveryMode())) {
      final ZoneDeliveryModeModel delivery = ((ZoneDeliveryModeModel) orderModel
              .getDeliveryMode());
      data.setShippingmethodtype(getRequestValue(delivery.getShippingGroup().getName()));
      data.setShippingmethod(getRequestValue(delivery.getCode()));
      data.setVerificationlevel(orderModel.getVerificationLevel());
    }
    data.setTemplate(getRequestValue(getConfigurationService().getConfiguration().getString(BlCoreConstants.PENDING_VERIFICATION_EVENT_TEMPLATE)));
    data.setExpectedShipDate(orderModel.getActualRentalStartDate());
    pendingVerificationEventRequest.setData(data);

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
