/**
 *
 */
package com.bl.core.esp.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.esp.dto.canceledEvent.OrderCanceledEventRequest;
import com.bl.esp.dto.ordercanceled.data.OrderCanceledData;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.text.SimpleDateFormat;
import java.util.Objects;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.Assert;


/**
 * This populator is used to populate OrderCanceled Event Request.
 * @author Avani Patel
 */
public class BlOrderCanceledRequestPopulator extends ESPEventCommonPopulator<OrderModel, OrderCanceledEventRequest>
{
	/**
	 * Populate the OrderCanceledRequest instance with values from the OrderModel.
	 *
	 * @param order
	 *           the source object
	 * @param orderCanceledEventRequest
	 *           the target to fill
	 * @throws ConversionException
	 *            if an error occurs
	 */
	@Override
	public void populate(final OrderModel order, final OrderCanceledEventRequest orderCanceledEventRequest)
			throws ConversionException
	{
		Assert.notNull(order, "Parameter emailId cannot be null.");
		Assert.notNull(orderCanceledEventRequest, "Parameter contactRequest cannot be null.");

		final UserModel userModel = order.getUser();
		if (Objects.nonNull(userModel))
		{
			orderCanceledEventRequest.setContactKey(getRequestValue(userModel.getUid()));
		}
		orderCanceledEventRequest.setEventDefinitionKey(getRequestValue(
				getConfigurationService().getConfiguration().getString(BlCoreConstants.ORDER_CANCELED_EVENT_DEFINITION_KEY)));
		populateOrderCanceledData(order, orderCanceledEventRequest);

	}

	/**
	 * This method populate order data from order model
	 *
	 * @param orderModel
	 *           orderModel
	 * @param orderConfirmationEventRequest
	 *           request to be get updated
	 */

	private void populateOrderCanceledData(final OrderModel orderModel,
			final OrderCanceledEventRequest orderConfirmationEventRequest)
	{
		final SimpleDateFormat formatter = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
		final OrderCanceledData data = new OrderCanceledData();
		populateCommonData(orderModel, data);
		data.setOldorderid(StringUtils.EMPTY);
		data.setTemplate(getRequestValue(
				getConfigurationService().getConfiguration().getString(BlCoreConstants.ORDER_CANCELED_EVENT_TEMPLATE)));
		data.setType(BooleanUtils.isTrue(orderModel.getIsRentalCart()) ? BlCoreConstants.RENTAL : BlCoreConstants.USED_GEAR);
		data.setReplacement(BooleanUtils.isTrue(orderModel.getIsCartUsedForReplacementOrder())
				? Boolean.TRUE.toString() : Boolean.FALSE.toString());
		data.setStatus(getRequestValue(Objects.nonNull(orderModel.getStatus()) ? orderModel.getStatus().getCode() : StringUtils.EMPTY));
		data.setDateplaced(formatter.format(orderModel.getDate()));
		data.setTotalcost(getDoubleValueForRequest(orderModel.getTotalPrice()));
		data.setReason(getRequestValue("test")); // TODO Setting dummy value, once we got the actual value then set actual value one
		orderConfirmationEventRequest.setData(data);
	}

}
