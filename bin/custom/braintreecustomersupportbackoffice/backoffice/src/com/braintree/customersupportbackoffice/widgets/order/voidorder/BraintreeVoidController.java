package com.braintree.customersupportbackoffice.widgets.order.voidorder;

import com.braintree.exceptions.BraintreeErrorException;
import com.braintree.facade.backoffice.BraintreeBackofficeVoidFacade;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zul.Combobox;
import org.zkoss.zul.Comboitem;
import org.zkoss.zul.Messagebox;

import java.math.RoundingMode;
import java.util.List;

import static com.braintree.customersupportbackoffice.constants.BraintreecustomersupportbackofficeConstants.BraintreeVoidAction.WIDGET_VOID_TRANSACTION_TITLE;

public class BraintreeVoidController extends DefaultWidgetController {
    private static final Logger LOG = Logger.getLogger(BraintreeVoidController.class);

    private static final String IN_SOCKET = "inputObject";
    private static final String OUT_MODIFIED_ITEM = "modifiedItem";

    private OrderModel order;

    @Autowired
    private BraintreeBackofficeVoidFacade braintreeBackofficeOrderFacade;

    @Wire
    private Combobox transactions;


    @SocketEvent(socketId = IN_SOCKET)
    public void initVoidRequestForm(OrderModel inputOrder) {
        setOrder(inputOrder);
        List<PaymentTransactionEntryModel> trans = braintreeBackofficeOrderFacade.getVoidableTransactions(inputOrder);
        LOG.info("Voidable transactions: " + trans);

        for (PaymentTransactionEntryModel v : trans) {
            Comboitem ci = new Comboitem();
            ci.setValue(v);
            ci.setLabel(v.getRequestId());
            ci.setLabel(getRequestCurrencyAmount(v));

            transactions.appendChild(ci);
        }

        if (!transactions.getItems().isEmpty()) {
            transactions.setSelectedItem(transactions.getItems().get(0));
        }

    }

    @ViewEvent(componentID = "voidrequest", eventName = "onClick")
    public void submitted() {
        Comboitem ci = transactions.getSelectedItem();

        if (ci != null) {
            try {
                braintreeBackofficeOrderFacade.executeVoid((PaymentTransactionEntryModel) ci.getValue());
                Messagebox.show("Transaction was successfully voided", getLabel(WIDGET_VOID_TRANSACTION_TITLE), Messagebox.OK, Messagebox.INFORMATION);
            } catch (final BraintreeErrorException e) {
                LOG.error("Error during void process: " + e.getMessage(), e);
                Messagebox.show(e.getMessage(), getLabel(WIDGET_VOID_TRANSACTION_TITLE), Messagebox.OK, Messagebox.ERROR);
            }
        }
        sendOutput(OUT_MODIFIED_ITEM, order);
    }

    private String getRequestCurrencyAmount(final PaymentTransactionEntryModel paymentEntry) {
        return paymentEntry.getRequestId() + ", " + paymentEntry.getCurrency().getSymbol()
                + paymentEntry.getAmount().setScale(paymentEntry.getCurrency().getDigits().intValue(), RoundingMode.HALF_UP);
    }

    public OrderModel getOrder() {
        return order;
    }

    public void setOrder(OrderModel order) {
        this.order = order;
    }
}
