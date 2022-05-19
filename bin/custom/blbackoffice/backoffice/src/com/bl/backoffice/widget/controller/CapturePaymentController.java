/**
 *
 */
package com.bl.backoffice.widget.controller;

import com.bl.core.payment.service.BlPaymentService;
import com.bl.logging.BlLogger;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.util.localization.Localization;
import javax.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.event.Events;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zul.Combobox;
import org.zkoss.zul.ListModelList;
import org.zkoss.zul.Messagebox;


/**
 * ########## BL-749 & BL-863 ######################
 *
 * Controller class used to capture the payment for frontdesk team and warehouse agent
 *
 * @author Krishan Vashishth
 *
 */
public class CapturePaymentController extends DefaultWidgetController {

    private static final Logger LOG = Logger.getLogger(CapturePaymentController.class);

    protected static final String OUT_CONFIRM = "confirmOutput";
    private static final String TITLE_MESSG = "Capture payment for order";
    protected static final String COMPLETE = "completed";
    private static final String CONNECTOR = " : ";
    private static final String ERR_MESG_FOR_ALREADY_CAPTURED_ORDER = "error.message.already.captured.order";
    private static final String ERR_MESG_FOR_ORDER_TRANSFER = "error.message.payment.capture.order.transfer";
    private static final String SUCC_MSG_FOR_PAYMENT_CAPTURED = "success.message.payment.captured";
    private static final String SUCC_MSG_FOR_PAYMENT_CAPTURED_GIFT_CARD = "success.message.payment.captured.giftCard";
    private static final String ERR_MSG_FOR_PAYMENT_CAPTURED = "error.message.payment.captured";
    private static final String MESSAGE_BOX_TITLE = "payment.capture.message.box.title";
    private static final String CANCEL_BUTTON = "cancelChanges";
    private static final String CAPTURE_BUTTON = "captureOrderPayment";
    private static final String INPUT_OBJECT = "inputObject";

    @Resource
    private BlPaymentService blPaymentService;

    private OrderModel orderModel;
    private ConsignmentModel consignmentModel;

    @Wire
    private Combobox paymentTransactions;

    @SocketEvent(socketId = INPUT_OBJECT)
    public void init(final ConsignmentModel inputObject) {
        this.getWidgetInstanceManager()
                .setTitle(new StringBuilder(TITLE_MESSG).append(CONNECTOR).append(inputObject.getOrder()
                        .getCode()).toString());
        if (inputObject.getOrder() instanceof OrderModel) {
            this.setOrderModel((OrderModel) inputObject.getOrder());
        }

        this.setConsignmentModel(inputObject);

        if (CollectionUtils.isNotEmpty(inputObject.getOrder().getPaymentTransactions())) {
            final ListModelList<PaymentTransactionModel> listModelList = new ListModelList<>();
            listModelList.addAll(inputObject.getOrder().getPaymentTransactions());
            listModelList.forEach(listModelList::addToSelection);
            paymentTransactions.setModel(listModelList);
        }
    }

    @ViewEvent(componentID = CAPTURE_BUTTON, eventName = Events.ON_CLICK)
    public void capturePayment() {
        BlLogger.logMessage(LOG, Level.DEBUG, "Payment Capturing starts");
        if (getConsignmentModel().isOrderTransferConsignment() || getConsignmentModel()
            .isInternalTransferConsignment()) {
            showMessageBox(Localization.getLocalizedString(ERR_MESG_FOR_ORDER_TRANSFER), true);
            return;
        }
        if (getOrderModel() == null || StringUtils.isEmpty(getOrderModel().getCode()) || getOrderModel().getIsCaptured()
            || OrderStatus.CANCELLING.equals(getOrderModel().getStatus())) {
            showMessageBox(Localization.getLocalizedString(ERR_MESG_FOR_ALREADY_CAPTURED_ORDER), true);
            return;
        }
        if (blPaymentService.capturePaymentForOrder(getOrderModel())) {
            if(Double.compare(getOrderModel().getTotalPrice(), 0.0) == 0 && CollectionUtils.isNotEmpty(getOrderModel().getGiftCard())) {
                showMessageBox(Localization.getLocalizedString(SUCC_MSG_FOR_PAYMENT_CAPTURED_GIFT_CARD));
            } else {
                showMessageBox(Localization.getLocalizedString(SUCC_MSG_FOR_PAYMENT_CAPTURED));
            }
        } else {
            BlLogger.logMessage(LOG, Level.ERROR, "Error occurred while capturing the payment");
            showMessageBox(Localization.getLocalizedString(ERR_MSG_FOR_PAYMENT_CAPTURED), true);
        }
    }

    @ViewEvent(componentID = CANCEL_BUTTON, eventName = Events.ON_CLICK)
    public void close() {
        this.sendOutput(OUT_CONFIRM, COMPLETE);
    }

    public OrderModel getOrderModel() {
        return orderModel;
    }

    public void setOrderModel(OrderModel orderModel) {
        this.orderModel = orderModel;
    }

    /**
     * Show message box.
     *
     * @param message     the message
     * @param isErrorMesg the is error mesg
     */
    protected void showMessageBox(final String message, final boolean isErrorMesg) {
        if (isErrorMesg) {
            Messagebox
                    .show(message, this.getLabel(MESSAGE_BOX_TITLE), Messagebox.OK, Messagebox.ERROR);
        } else {
            Messagebox
                    .show(message, this.getLabel(MESSAGE_BOX_TITLE), Messagebox.OK, Messagebox.INFORMATION);
        }
        this.sendOutput(OUT_CONFIRM, COMPLETE);
    }

    /**
     * Show message box.
     *
     * @param message the message
     */
    protected void showMessageBox(final String message) {
        showMessageBox(message, false);
    }

    public ConsignmentModel getConsignmentModel() {
        return consignmentModel;
    }

    public void setConsignmentModel(
        final ConsignmentModel consignmentModel) {
        this.consignmentModel = consignmentModel;
    }
}
