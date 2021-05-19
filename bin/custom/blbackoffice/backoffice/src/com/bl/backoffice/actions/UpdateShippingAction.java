//
// Decompiled by Procyon v0.5.36
//

package com.bl.backoffice.actions;

import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.constants.GeneratedCoreConstants.Enumerations.OrderStatus;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordercancel.OrderCancelService;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.servicelayer.user.UserService;

import java.util.List;

import javax.annotation.Resource;

import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;


public class UpdateShippingAction extends AbstractComponentWidgetAdapterAware implements CockpitAction<OrderModel, OrderModel>
{
	protected static final String SOCKET_OUT_CONTEXT = "blCustomShippingContext";
	protected static final String CAPTURE_PAYMENT_ON_CONSIGNMENT = "warehousing.capturepaymentonconsignment";
	@Resource
	private UserService userService;
	@Resource
	private OrderCancelService orderCancelService;
	@Resource
	private List<OrderStatus> notCancellableOrderStatus;
	@Resource
	private List<ConsignmentStatus> notCancellableConsignmentStatus;
	@Resource
	private ConfigurationService configurationService;

	public boolean canPerform(final ActionContext<OrderModel> actionContext)
	{
		final OrderModel order = actionContext.getData();

		return (order != null);
	}

	public String getConfirmationMessage(final ActionContext<OrderModel> actionContext)
	{
		return null;
	}

	public boolean needsConfirmation(final ActionContext<OrderModel> actionContext)
	{
		return false;
	}

	public ActionResult<OrderModel> perform(final ActionContext<OrderModel> actionContext)
	{
		this.sendOutput(SOCKET_OUT_CONTEXT, actionContext.getData());
		return new ActionResult("success");
	}

	protected OrderCancelService getOrderCancelService()
	{
		return this.orderCancelService;
	}

	protected UserService getUserService()
	{
		return this.userService;
	}

	protected List<OrderStatus> getNotCancellableOrderStatus()
	{
		return this.notCancellableOrderStatus;
	}

	protected List<ConsignmentStatus> getNotCancellableConsignmentStatus()
	{
		return this.notCancellableConsignmentStatus;
	}

	protected ConfigurationService getConfigurationService()
	{
		return this.configurationService;
	}
}
