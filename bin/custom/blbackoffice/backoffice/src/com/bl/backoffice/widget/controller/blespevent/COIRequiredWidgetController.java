package com.bl.backoffice.widget.controller.blespevent;

import com.bl.constants.BlDeliveryModeLoggingConstants;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.esp.service.impl.DefaultBlESPEventService;
import com.bl.core.model.BlPickUpZoneDeliveryModeModel;
import com.bl.core.model.BlRushDeliveryModeModel;
import com.bl.facades.fexEx.data.SameDayCityReqData;
import com.bl.facades.fexEx.data.SameDayCityResData;
import com.bl.facades.shipping.BlCheckoutFacade;
import com.bl.integration.services.BlFedExSameDayService;
import com.bl.logging.BlLogger;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import de.hybris.platform.commercefacades.i18n.I18NFacade;
import de.hybris.platform.commercefacades.user.data.RegionData;
import de.hybris.platform.commerceservices.delivery.DeliveryService;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.model.c2l.RegionModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.order.delivery.DeliveryModeModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.order.CalculationService;
import de.hybris.platform.order.exceptions.CalculationException;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.exceptions.UnknownIdentifierException;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.util.Config;
import de.hybris.platform.util.localization.Localization;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import javax.annotation.Resource;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.zkoss.zk.ui.Component;
import org.zkoss.zk.ui.WrongValueException;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zk.ui.util.Clients;
import org.zkoss.zul.Combobox;
import org.zkoss.zul.ListModelList;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Textbox;


/**
 * ############## BL-1363 : Trigger COI Required ESP event ##############
 *
 * @author Sunil Sahu
 */
public class COIRequiredWidgetController extends DefaultWidgetController
{
	private static final Logger LOG = Logger.getLogger(COIRequiredWidgetController.class);

	protected static final String OUT_CONFIRM = "confirmOutput";
	protected static final String COMPLETE = "completed";
	private static final String MESSAGE_BOX_TITLE = "success.message.coirequired.title";
	private static final String MESSAGE_BOX_TEXT = "success.message.coirequired.emailsent";
	private static final String MESSAGE_BOX_ERROR_TEXT = "error.message.coirequired.emailsent";

	@Wire
	private Textbox amount;

	private static final int DURATION = 2000;

	private OrderModel orderModel;

	private transient ModelService modelService;

	private static ApplicationContext context;

	@Autowired
	private static I18NFacade i18NFacade;

	@Autowired
	private transient CommonI18NService commonI18NService;

	private DefaultBlESPEventService blEspEventService;

	@Autowired
	CalculationService calculationService;



	/**
	 * This method is used to show the default values of shipping and address
	 *
	 * @param inputObject
	 */
	@SocketEvent(socketId = "inputObject")
	public void initCustomerAddressForm(final OrderModel inputObject)
	{
		this.setOrderModel(inputObject);
		this.amount.setValue("");
		this.getWidgetInstanceManager()
				.setTitle(String.valueOf(this.getWidgetInstanceManager().getLabel("blbackoffice.coirequired.confirm.title"))
						.concat(BlInventoryScanLoggingConstants.EMPTY_SPACE).concat(this.getOrderModel().getCode()));

	}




	/**
	 * This method will be used to reset the popup values
	 */
	@ViewEvent(componentID = "undochanges", eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void reset()
	{
		this.amount.setValue("");
	}

	/**
	 * This method will be used to confirm/ Save the modified values to trigger ESP event
	 */
	@ViewEvent(componentID = "confirmTriggerEmail", eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void triggerESPEvent() {
		this.validateRequest();

		try {
			triggerCOIRequiredESPEvent(this.orderModel, getDoubleValue(this.amount.getValue()));
		}catch (final Exception ex){
			BlLogger.logMessage(LOG, Level.ERROR, "Failed to trigger COI required event.", ex);
		}


		this.showMessageBox(true);   // inside trycatch
	}

	/**
	 * This method will be used to trigger COI required ESP event
	 */
	private void triggerCOIRequiredESPEvent(final OrderModel orderModel, final Double amount) {

		try {
			getBlEspEventService().sendOrderVerificationCOIRequiredEvent(orderModel, amount);

		} catch (final Exception e) {
			BlLogger.logMessage(LOG, Level.ERROR, "Failed to trigger COI required event.", e);
		}

	}


	/**
	 * This method will be used to validate modified data for shipping
	 */
	protected void validateRequest() {

		if (StringUtils.isEmpty(this.amount.getValue())) {
			throw new WrongValueException(this.amount,
					this.getLabel("blbackoffice.coirequired.missing.amount"));
		} else {
			getDoubleValue(this.amount.getValue());
		}

	}

	private Double getDoubleValue(final String amount) {

		Double doubleValue = 0d;
		try {
			doubleValue = Double.parseDouble(amount);
		} catch (final NumberFormatException e) {
			throw new WrongValueException(this.amount,
					this.getLabel("blbackoffice.coirequired.amount.notanumber"));
		}
		return doubleValue;
	}


	/**
	 * This method will be used to show success message
	 *  @param isErrorMesg the is error mesg
	 */
	protected void showMessageBox(final boolean isErrorMesg) {

		if (isErrorMesg) {

			Messagebox
					.show(Localization.getLocalizedString(MESSAGE_BOX_ERROR_TEXT),
							Localization.getLocalizedString(MESSAGE_BOX_TITLE), Messagebox.OK,
							Messagebox.ERROR);

		} else {
			Messagebox
					.show(Localization.getLocalizedString(MESSAGE_BOX_TEXT),
							Localization.getLocalizedString(MESSAGE_BOX_TITLE), Messagebox.OK,
							Messagebox.INFORMATION);
		}

		this.sendOutput(OUT_CONFIRM, COMPLETE);

	}

	/**
	 * This method will be used to show notification
	 *
	 * @param msg
	 * @param ref
	 */
	private void showNotify(final String msg, final Component ref)
	{
		Clients.showNotification(msg, "info", ref, "end_center", DURATION);
	}

	/**
	 * @return the orderModel
	 */
	public OrderModel getOrderModel()
	{
		return orderModel;
	}

	/**
	 * @param orderModel
	 *           the orderModel to set
	 */
	public void setOrderModel(final OrderModel orderModel)
	{
		this.orderModel = orderModel;
	}

	/**
	 * @return the blEspEventService
	 */
	public DefaultBlESPEventService getBlEspEventService(){
		return blEspEventService;
	}

	/**
	 * @param blEspEventService
	 *           the blEspEventService to set
	 */
	public void setBlEspEventService(final DefaultBlESPEventService blEspEventService){
		this.blEspEventService = blEspEventService;
	}

}

