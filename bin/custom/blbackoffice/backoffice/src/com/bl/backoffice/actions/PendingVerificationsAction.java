package com.bl.backoffice.actions;

import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.util.localization.Localization;

import javax.annotation.Resource;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zul.Messagebox;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.esp.service.impl.DefaultBlESPEventService;
import com.bl.logging.BlLogger;
import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;

public class PendingVerificationsAction extends AbstractComponentWidgetAdapterAware implements CockpitAction<OrderModel, OrderModel> {
    protected static final String SOCKET_OUT_CONTEXT = "blPendingVerificationsContext";

    private static final Logger LOG = Logger.getLogger(PendingVerificationsAction.class);

    private OrderModel order;
	 private ConfigurationService configurationService;

    @Resource
    private DefaultBlESPEventService defaultBlESPEventService;

    /**
     * This method is responsible for trigger Pending Verifications ESP event
     *
     * @param actionContext the action context
     * @return the boolean
     */
    @Override
    public boolean canPerform(final ActionContext<OrderModel> actionContext) {
        order = actionContext.getData();

        return (order != null);
    }

    /**
     * This method will fetch the action context data for blPendingVerificationsContext
     *
     * @param actionContext the action context
     * @return the action result
     */
    public ActionResult<OrderModel> perform(final ActionContext<OrderModel> actionContext) {
        order = actionContext.getData();
        try {
            getDefaultBlESPEventService().sendOrderPendingVerificationsEvent(order);
            this.showMessageBox(false);
        } catch (final Exception e) {
            BlLogger.logMessage(LOG, Level.ERROR, "Failed to trigger pending verification event.", e);
        }
        this.sendOutput(SOCKET_OUT_CONTEXT, actionContext.getData());
        return new ActionResult("success");
    }


    /**
     * This method will be used to show success message
     *
     * @param isErrorMessage the is error message
     */
    protected void showMessageBox(final boolean isErrorMessage) {

        if (isErrorMessage) {

            Messagebox
						.show(getConfigurationService().getConfiguration()
								.getString(BlCoreConstants.PENDING_VERIFICATION_MESSAGE_BOX_ERROR_TEXT),
                            Localization.getLocalizedString(BlCoreConstants.PENDING_VERIFICATION_MESSAGE_BOX_TITLE), Messagebox.OK,
                            Messagebox.ERROR);

        } else {
            Messagebox
						.show(getConfigurationService().getConfiguration()
								.getString(BlCoreConstants.PENDING_VERIFICATION_MESSAGE_BOX_TEXT),
                            Localization.getLocalizedString(BlCoreConstants.PENDING_VERIFICATION_MESSAGE_BOX_TITLE), Messagebox.OK,
                            Messagebox.INFORMATION);
        }
    }

    public DefaultBlESPEventService getDefaultBlESPEventService() {
        return defaultBlESPEventService;
    }

    public void setDefaultBlESPEventService(final DefaultBlESPEventService defaultBlESPEventService) {
        this.defaultBlESPEventService = defaultBlESPEventService;
    }

	 public ConfigurationService getConfigurationService()
	 {
		 return configurationService;
	 }

	 public void setConfigurationService(final ConfigurationService configurationService)
	 {
		 this.configurationService = configurationService;
	 }
}