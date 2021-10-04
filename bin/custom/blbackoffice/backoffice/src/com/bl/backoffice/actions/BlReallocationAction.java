/**
 *
 */
package com.bl.backoffice.actions;

import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;
import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import java.util.List;
import javax.annotation.Resource;
import org.apache.commons.collections.CollectionUtils;


/**
 * This action class is responsible to find the consignment for re allocation
 *
 * @author Sunil
 *
 */
public class BlReallocationAction extends AbstractComponentWidgetAdapterAware implements
		CockpitAction<ConsignmentModel, ConsignmentModel> {

	protected static final String CAPTURE_PAYMENT_ON_CONSIGNMENT = "warehousing.capturepaymentonconsignment";
	protected static final String SOCKET_OUT_CONTEXT = "reallocateContext";

	@Resource
	private List<ConsignmentStatus> reallocableConsignmentStatuses;
	@Resource
	private ConfigurationService configurationService;

	public BlReallocationAction() {
		//constructor
	}

	@Override
	public boolean canPerform(ActionContext<ConsignmentModel> actionContext) {

		Object data = actionContext.getData();
		boolean decision = false;
		boolean captureOnConsignmentReallocationAllowed = true;

		if (data instanceof ConsignmentModel) {

			ConsignmentModel consignment = (ConsignmentModel) data;
			if (!CollectionUtils.isEmpty(consignment.getConsignmentEntries()) && !(consignment
					.isInternalTransferConsignment()) && !(consignment.getStatus()
					.equals(ConsignmentStatus.BL_SHIPPED))
					&& consignment.getFulfillmentSystemConfig() == null) {

				decision = consignment.getConsignmentEntries().stream().anyMatch(consignmentEntry ->
					 consignmentEntry.getQuantityPending() > 0L);
			}

			if (this.getConfigurationService().getConfiguration()
					.getBoolean(CAPTURE_PAYMENT_ON_CONSIGNMENT, false)) {
				captureOnConsignmentReallocationAllowed = this.getReallocableConsignmentStatuses()
						.contains(consignment.getStatus());
			}
		}

		return decision && captureOnConsignmentReallocationAllowed;
	}

	@Override
	public String getConfirmationMessage(ActionContext<ConsignmentModel> actionContext) {
		return null;
	}

	@Override
	public boolean needsConfirmation(ActionContext<ConsignmentModel> actionContext) {
		return false;
	}

	public ActionResult<ConsignmentModel> perform(ActionContext<ConsignmentModel> actionContext) {
		this.sendOutput(SOCKET_OUT_CONTEXT, actionContext.getData());

		return new ActionResult<>("success");
	}

	protected ConfigurationService getConfigurationService() {
		return this.configurationService;
	}

	protected List<ConsignmentStatus> getReallocableConsignmentStatuses() {
		return this.reallocableConsignmentStatuses;
	}
}
