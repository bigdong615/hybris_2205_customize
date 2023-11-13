package com.bl.core.esp.populators;

import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

import java.text.SimpleDateFormat;
import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.springframework.util.Assert;

import com.bl.core.constants.BlCoreConstants;
import com.bl.esp.dto.pendingverification.VerificationReminderEventRequest;
import com.bl.esp.dto.pendingverification.data.VerificationReminder;


public class BlOrderVerificationReminderPopulator extends ESPEventCommonPopulator<OrderModel, VerificationReminderEventRequest>
{

  	/**
	 * This method created to populate verification Reminder Event Request ESP event
	 *
	 * @param orderModel
	 *           ordermodel
	 * @param VerificationReminderEventRequest
	 *           verificationReminderEventRequest
	 * @throws ConversionException
	 *            conversionException
	 */
  @Override
  public void populate(final OrderModel orderModel, final VerificationReminderEventRequest verificationReminderEventRequest)
      throws ConversionException {
    Assert.notNull(orderModel, "Parameter order cannot be null.");
	 Assert.notNull(verificationReminderEventRequest, "Parameter pendingVerificationEventRequest cannot be null.");

    final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
    final UserModel userModel = orderModel.getUser();
    if(Objects.nonNull(userModel)) {
		 verificationReminderEventRequest.setContactKey(getRequestValue(userModel.getUid()));
    }
	 verificationReminderEventRequest
        .setEventDefinitionKey(getRequestValue(getConfigurationService().getConfiguration().
					 getString(BlCoreConstants.VERIFICATION_REMINDER_EVENT_DEFINITION_KEY)));
	 final VerificationReminder data = new VerificationReminder();
    populateCommonData(orderModel , data);
    data.setStatus(getRequestValue(getOrderStatus(orderModel)));
    data.setDateplaced(formatter.format(orderModel.getDate()));
	 if (Objects.nonNull(userModel))
	 {
		 data.setCustomername(getRequestValue(userModel.getName()));
	 }
    if(Objects.nonNull(orderModel.getDeliveryMode())) {
      final ZoneDeliveryModeModel delivery = ((ZoneDeliveryModeModel) orderModel
              .getDeliveryMode());
      data.setShippingmethodtype(getRequestValue(delivery.getShippingGroup().getName()));
      data.setShippingmethod(getRequestValue(delivery.getCode()));
      data.setVerificationlevel(orderModel.getVerificationLevel());
    }
	 data.setTemplate(getRequestValue(
			 getConfigurationService().getConfiguration().getString(BlCoreConstants.VERIFICATION_REMINDER_EVENT_TEMPLATE)));
    data.setExpectedShipDate(orderModel.getActualRentalStartDate());
	 verificationReminderEventRequest.setData(data);
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
