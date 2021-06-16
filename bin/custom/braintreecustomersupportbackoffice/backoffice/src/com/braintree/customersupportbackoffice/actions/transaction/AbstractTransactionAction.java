package com.braintree.customersupportbackoffice.actions.transaction;

import com.braintree.converters.BraintreeTransactionDetailConverter;
import com.braintree.customersupportbackoffice.facade.BrainTreeCustomerSupportFacade;
import com.braintree.customersupportbackoffice.services.BrainTreeTransactionManagementActionService;
import com.braintree.model.BrainTreeTransactionDetailModel;
import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;

import javax.annotation.Resource;

public abstract class AbstractTransactionAction extends AbstractComponentWidgetAdapterAware implements CockpitAction<BrainTreeTransactionDetailModel, BrainTreeTransactionDetailModel> {
    @Resource(name = "brainTreeCustomerSupportFacade")
    private BrainTreeCustomerSupportFacade brainTreeCustomerSupportFacade;

    @Resource(name = "brainTreeTransactionManagementActionService")
    private BrainTreeTransactionManagementActionService brainTreeTransactionManagementActionService;

    @Resource(name = "transactionDetailPopulator")
    private BraintreeTransactionDetailConverter transactionDetailPopulator;

    @Override
    public boolean canPerform(final ActionContext<BrainTreeTransactionDetailModel> ctx)
    {
        return false;
    }

    @Override
    public boolean needsConfirmation(final ActionContext<BrainTreeTransactionDetailModel> ctx)
    {
        return false;
    }

    @Override
    public String getConfirmationMessage(final ActionContext<BrainTreeTransactionDetailModel> ctx)
    {
        return null;
    }

    public BrainTreeCustomerSupportFacade getBrainTreeCustomerSupportFacade() {
        return brainTreeCustomerSupportFacade;
    }

    public void setBrainTreeCustomerSupportFacade(BrainTreeCustomerSupportFacade brainTreeCustomerSupportFacade) {
        this.brainTreeCustomerSupportFacade = brainTreeCustomerSupportFacade;
    }

    public BrainTreeTransactionManagementActionService getBrainTreeTransactionManagementActionService() {
        return brainTreeTransactionManagementActionService;
    }

    public void setBrainTreeTransactionManagementActionService(BrainTreeTransactionManagementActionService brainTreeTransactionManagementActionService) {
        this.brainTreeTransactionManagementActionService = brainTreeTransactionManagementActionService;
    }

    public BraintreeTransactionDetailConverter getTransactionDetailPopulator() {
        return transactionDetailPopulator;
    }

    public void setTransactionDetailPopulator(BraintreeTransactionDetailConverter transactionDetailPopulator) {
        this.transactionDetailPopulator = transactionDetailPopulator;
    }
}
