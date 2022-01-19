/**
 *
 *//*

package com.bl.backoffice.widget.controller.blespevent;

import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.util.localization.Localization;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.WrongValueException;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Textbox;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.esp.service.impl.DefaultBlESPEventService;
import com.bl.logging.BlLogger;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;


*/
/**
 * ############## BL-1361 : Trigger More Info Required ESP event ##############
 *
 * @author Avani Patel
 *
 *//*

public class MoreInfoRequiredWidgetController extends DefaultWidgetController
{
	private static final Logger LOG = Logger.getLogger(MoreInfoRequiredWidgetController.class);

	protected static final String OUT_CONFIRM = "moreinforequired";
	protected static final String COMPLETE = "completed";
	private static final String MESSAGE_BOX_TITLE = "success.message.inforequired.title";
	private static final String MESSAGE_BOX_TEXT = "success.message.inforequired.emailsent";
	private static final String MESSAGE_BOX_ERROR_TEXT = "error.message.inforequired.emailsent";

	@Wire
	private Textbox verificationText;

	private OrderModel orderModel;

	private DefaultBlESPEventService blEspEventService;

	*/
/**
	 * This method is used to show the default values of shipping and address
	 *
	 * @param inputObject
	 *//*

	@SocketEvent(socketId = "inputObject")
	public void initMoreInfoRequiredForm(final OrderModel inputObject)
	{
		this.setOrderModel(inputObject);
		this.verificationText.setValue("");
		this.getWidgetInstanceManager()
				.setTitle(String.valueOf(this.getWidgetInstanceManager().getLabel("blbackoffice.inforequired.confirm.title"))
						.concat(BlInventoryScanLoggingConstants.EMPTY_SPACE).concat(this.getOrderModel().getCode()));
	}

	*/
/**
	 * This method will be used to reset the popup values
	 *//*

	@ViewEvent(componentID = "undochanges", eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void reset()
	{
		this.verificationText.setValue("");
	}

	*/
/**
	 * This method will be used to confirm/ Save the modified values to trigger ESP event
	 *//*

	@ViewEvent(componentID = "confirmTriggerEmail", eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void triggerESPEvent()
	{

		this.validateRequest();

		try
		{
			triggerInfoRequiredESPEvent(this.orderModel, this.verificationText.getValue());

		}
		catch (final Exception ex)
		{

			BlLogger.logMessage(LOG, Level.ERROR, "Failed to trigger More Info required event.", ex);
			this.showMessageBox(true);
		}

		this.sendOutput(OUT_CONFIRM, COMPLETE);
	}

	*/
/**
	 * This method will be used to trigger More Info required ESP event
	 *//*

	private void triggerInfoRequiredESPEvent(final OrderModel orderModel, final String verificationText)
	{

		try
		{
			getBlEspEventService().sendOrderMoreInfoRequiredEvent(orderModel, verificationText);
			this.showMessageBox(false);

		}
		catch (final Exception e)
		{
			BlLogger.logMessage(LOG, Level.ERROR, "Failed to trigger More Info required event.", e);
		}
	}


	*/
/**
	 * This method will be used to validate modified data for shipping
	 *//*

	protected void validateRequest()
	{

		if (StringUtils.isEmpty(this.verificationText.getValue()))
		{
			throw new WrongValueException(this.verificationText,
					this.getLabel("blbackoffice.inforequired.missing.verification.text"));
		}
	}

	*/
/**
	 * This method will be used to show success message
	 *
	 * @param isErrorMesg
	 *           the is error mesg
	 *//*

	protected void showMessageBox(final boolean isErrorMesg)
	{

		if (isErrorMesg)
		{

			Messagebox.show(Localization.getLocalizedString(MESSAGE_BOX_ERROR_TEXT),
					Localization.getLocalizedString(MESSAGE_BOX_TITLE), Messagebox.OK, Messagebox.ERROR);

		}
		else
		{
			Messagebox.show(Localization.getLocalizedString(MESSAGE_BOX_TEXT), Localization.getLocalizedString(MESSAGE_BOX_TITLE),
					Messagebox.OK, Messagebox.INFORMATION);
		}
	}

	*/
/**
	 * @return the orderModel
	 *//*

	public OrderModel getOrderModel()
	{
		return orderModel;
	}

	*/
/**
	 * @param orderModel
	 *           the orderModel to set
	 *//*

	public void setOrderModel(final OrderModel orderModel)
	{
		this.orderModel = orderModel;
	}

	*/
/**
	 * @return the blEspEventService
	 *//*

	public DefaultBlESPEventService getBlEspEventService()
	{
		return blEspEventService;
	}

	*/
/**
	 * @param blEspEventService
	 *           the blEspEventService to set
	 *//*

	public void setBlEspEventService(final DefaultBlESPEventService blEspEventService)
	{
		this.blEspEventService = blEspEventService;
	}

}
*/
