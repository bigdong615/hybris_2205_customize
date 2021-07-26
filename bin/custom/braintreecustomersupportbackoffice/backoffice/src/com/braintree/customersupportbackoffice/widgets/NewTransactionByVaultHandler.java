package com.braintree.customersupportbackoffice.widgets;

import com.braintree.customersupportbackoffice.data.BrainTreeTransactionInfo;
import com.braintree.customersupportbackoffice.facade.BrainTreeCustomerSupportFacade;
import com.braintree.customfield.service.CustomFieldsService;
import com.braintree.hybris.data.BrainTreeResponseResultData;
import com.hybris.backoffice.widgets.notificationarea.event.NotificationEvent;
import com.hybris.backoffice.widgets.notificationarea.event.NotificationEventTypes;
import com.hybris.cockpitng.config.jaxb.wizard.CustomType;
import com.hybris.cockpitng.core.impl.NotificationStack;
import com.hybris.backoffice.widgets.notificationarea.NotificationService;
import com.hybris.cockpitng.widgets.configurableflow.FlowActionHandler;
import com.hybris.cockpitng.widgets.configurableflow.FlowActionHandlerAdapter;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Map;

public class NewTransactionByVaultHandler implements FlowActionHandler {

    private static final String FORM = "newTransactionByVaultForm";

    private CustomFieldsService customFieldsService;
    private BrainTreeCustomerSupportFacade brainTreeCustomerSupportFacade;

    private NotificationStack notificationStack;

    @Autowired
    NotificationService notificationService;

    @Override
    public void perform(CustomType customType, FlowActionHandlerAdapter adapter, Map<String, String> parameters) {
        NewTransactionByVaultForm form = adapter.getWidgetInstanceManager().getModel().getValue(FORM, NewTransactionByVaultForm.class);
        if (form != null) {
            final BrainTreeTransactionInfo brainTreeInfo = new BrainTreeTransactionInfo();
            brainTreeInfo.setAmount(form.getAmount()).setTax(form.getTaxAmount());
            setCustomFields(brainTreeInfo, form.getCustomFields());
            brainTreeInfo.setPaymentMethodToken(form.getPaymentMethodToken());

            final BrainTreeResponseResultData transaction = brainTreeCustomerSupportFacade.createTransaction(brainTreeInfo);

            if (transaction.isSuccess())
            {
                notificationService.notifyUser("Transaction successfully created", NotificationEventTypes.EVENT_TYPE_OBJECT_CREATION, NotificationEvent.Level.SUCCESS);
                form.setStatus("Transaction was successfully created with id: " + transaction.getTransactionId());
            }
            else
            {
                notificationService.notifyUser("Error occurs", NotificationEventTypes.EVENT_TYPE_OBJECT_CREATION, NotificationEvent.Level.FAILURE);
                form.setStatus("Error occurs: " + transaction.getErrorMessage());
            }
        }
        adapter.next();
    }

    private void setCustomFields(final BrainTreeTransactionInfo brainTreeInfo, final String custom)
    {
        if (StringUtils.isNotBlank(custom))
        {
            final String[] splitByParametersPair = StringUtils.split(custom, ",");

            final Map<String, String> defaultCustomFields = customFieldsService.getDefaultCustomFieldsMap();

            for (final String key : defaultCustomFields.keySet()) {
                brainTreeInfo.setCustom(key, defaultCustomFields.get(key));
            }

            if (splitByParametersPair.length > 0) {
                for (final String parameter : splitByParametersPair) {
                    final String[] spitedParameter = StringUtils.split(parameter, ":");
                    if (spitedParameter.length == 2) {
                        brainTreeInfo.setCustom(spitedParameter[0], spitedParameter[1]);
                    }
                }
            }
        }
    }

    @SuppressWarnings("unused")
    public void setNotificationStack(NotificationStack notificationStack) {
        this.notificationStack = notificationStack;
    }

    public void setBrainTreeCustomerSupportFacade(BrainTreeCustomerSupportFacade brainTreeCustomerSupportFacade) {
        this.brainTreeCustomerSupportFacade = brainTreeCustomerSupportFacade;
    }

    public void setCustomFieldsService(CustomFieldsService customFieldsService) {
        this.customFieldsService = customFieldsService;
    }
}
