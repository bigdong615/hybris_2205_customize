package com.braintree.customersupportbackoffice.widgets.order.authorization;

import com.braintree.customersupportbackoffice.data.BrainTreeTransactionInfo;
import com.braintree.customersupportbackoffice.facade.BrainTreeCustomerSupportFacade;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.omsbackoffice.widgets.returns.dtos.ReturnEntryToCreateDto;
import de.hybris.platform.order.OrderService;
import de.hybris.platform.payment.AdapterException;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.WrongValueException;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Textbox;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.util.Map;
import java.util.Set;

public class BrainTreeOrderAuthorizationController extends DefaultWidgetController {
    private static final long serialVersionUID = 1L;
    private static final Logger LOG = Logger.getLogger(BrainTreeOrderAuthorizationController.class.getName());
    protected static final String IN_SOCKET = "inputObject";
    private static final String OUT_MODIFIED_ITEM = "modifiedItem";
    private Set<ReturnEntryToCreateDto> returnEntriesToCreate;
    private OrderModel order;
    @Wire
    private Textbox orderCode;
    @Wire
    private Textbox customer;
    @Wire
    private Textbox amount;

    @Resource(name = "brainTreeCustomerSupportFacade")
    private BrainTreeCustomerSupportFacade brainTreeCustomerSupportFacade;

    @Resource(name = "orderService")
    private OrderService orderService;

    public BrainTreeOrderAuthorizationController() {
    }

    @SocketEvent(
            socketId = IN_SOCKET
    )
    public void initCreateReturnRequestForm(OrderModel inputOrder) {
        this.setOrder(inputOrder);
        this.getWidgetInstanceManager().setTitle(this.getWidgetInstanceManager().getLabel("braintreecustomersupportbackoffice.authorizeorder.confirm.title") + " " + this.getOrder().getCode());
        this.orderCode.setValue(this.getOrder().getCode());
        this.customer.setValue(this.getOrder().getUser().getDisplayName());
        this.amount.setValue(this.getOrder().getTotalPrice().toString());
    }

    @ViewEvent(
            componentID = "authorizeorderrequest",
            eventName = "onClick"
    )
    public void confirmAuthorization() {
        validate();
        processAuthorization();
        sendOutput(OUT_MODIFIED_ITEM, order);
    }

    private void processAuthorization() {
        final BrainTreeTransactionInfo brainTreeInfo = new BrainTreeTransactionInfo();
        BrainTreePaymentInfoModel paymentInfoModel = (BrainTreePaymentInfoModel) order.getPaymentInfo();
        brainTreeInfo.setAmount(new BigDecimal(amount.getValue()));
        brainTreeInfo.setPaymentMethodToken(paymentInfoModel.getPaymentMethodToken());
        setCustomFields(brainTreeInfo, paymentInfoModel.getCustomFields());

        final PaymentTransactionEntryModel transaction;
        try {
            transaction = brainTreeCustomerSupportFacade.authorizePayment(order, paymentInfoModel.getCustomFields(), BigDecimal.valueOf(Double.valueOf(amount.getValue())));
            processResult(transaction);
        }
        catch (final AdapterException e) {
            LOG.error("Exception, message : " + e.getMessage());
            Messagebox.show("Error message: " + e.getMessage(), getLabel("bt.customersupport.order.authorizeTransaction.failed"), Messagebox.OK, Messagebox.ERROR);
        }
    }

    private void processResult(final PaymentTransactionEntryModel result)
    {
        if (!TransactionStatus.REJECTED.name().equalsIgnoreCase(result.getTransactionStatus()))
        {
            showSuccessMessage();
        }
        else
        {
            Messagebox.show("Transaction is rejected!");
        }
    }

    private void setCustomFields(BrainTreeTransactionInfo braintreeInfo, Map<String, String> customFields)
    {
        for (final String key : customFields.keySet())
        {
            braintreeInfo.setCustom(key, customFields.get(key));
        }
    }

    private void validate()
    {
        String value = amount.getValue();
        if (StringUtils.isBlank(value))
        {
            throw new WrongValueException(amount, getLabel("bt.customersupport.order.amount.error.empty"));
        }
        try{
            if (BigDecimal.ZERO.equals(new BigDecimal(value)))
            {
                throw new WrongValueException(amount, getLabel("bt.customersupport.order.amount.error.zero"));
            }
        } catch (NumberFormatException e){
            throw new WrongValueException(amount, getLabel("bt.customersupport.order.amount.error.number.format"));
        }
    }

    private void showSuccessMessage()
    {
        Messagebox.show("Order with code " + order.getCode() + " was successfully authorized.");
    }

    public OrderModel getOrder() {
        return order;
    }

    public void setOrder(OrderModel order) {
        this.order = order;
    }
}


