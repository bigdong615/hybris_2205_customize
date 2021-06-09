package com.braintree.customersupportbackoffice.actions.order;

import com.braintree.customersupportbackoffice.services.BrainTreeOrderManagementActionsService;
import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;
import de.hybris.platform.core.model.order.OrderModel;

import javax.annotation.Resource;

public abstract class AbstractOrderAction extends AbstractComponentWidgetAdapterAware implements CockpitAction<OrderModel, OrderModel> {

    @Resource(name = "brainTreeOrderManagementActionsService")
    private BrainTreeOrderManagementActionsService brainTreeOrderManagementActionsService;

    public String getConfirmationMessage(ActionContext<OrderModel> actionContext) {
        return null;
    }

    public boolean needsConfirmation(ActionContext<OrderModel> actionContext) {
        return false;
    }

    public BrainTreeOrderManagementActionsService getBrainTreeOrderManagementActionsService() {
        return brainTreeOrderManagementActionsService;
}

    public void setBrainTreeOrderManagementActionsService(BrainTreeOrderManagementActionsService brainTreeOrderManagementActionsService) {
        this.brainTreeOrderManagementActionsService = brainTreeOrderManagementActionsService;
    }
}
