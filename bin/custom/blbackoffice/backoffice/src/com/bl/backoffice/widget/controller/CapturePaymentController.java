/**
 *
 */
package com.bl.backoffice.widget.controller;

import com.bl.core.payment.service.BlPaymentService;
import com.bl.logging.BlLogger;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.util.localization.Localization;
import javax.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zul.Combobox;
import org.zkoss.zul.ListModelList;
import org.zkoss.zul.Messagebox;


/**
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
  private static final String SUCC_MSG_FOR_PAYMENT_CAPTURED = "success.message.payment.captured";
  private static final String ERR_MSG_FOR_PAYMENT_CAPTURED = "error.message.payment.captured";

  @Resource
  private BlPaymentService blPaymentService;

  private OrderModel orderModel;

  @Wire
  private Combobox paymentTransactions;

  @SocketEvent(socketId = "inputObject")
  public void init(final ConsignmentModel inputObject) {
    this.getWidgetInstanceManager()
        .setTitle(new StringBuilder(TITLE_MESSG).append(CONNECTOR).append(inputObject.getOrder()
            .getCode()).toString());
    if (inputObject.getOrder() instanceof OrderModel) {
      this.setOrderModel((OrderModel) inputObject.getOrder());
    }
    if (CollectionUtils.isNotEmpty(inputObject.getOrder().getPaymentTransactions())) {
      final ListModelList<PaymentTransactionModel> listModelList = new ListModelList<>();
      listModelList.addAll(inputObject.getOrder().getPaymentTransactions());
      listModelList.forEach(listModelList::addToSelection);
      paymentTransactions.setModel(listModelList);
    }
  }

  @ViewEvent(componentID = "captureOrderPayment", eventName = "onClick")
  public void capturePayment() {
    BlLogger.logMessage(LOG, Level.DEBUG, "Payment Capturing starts");
    if (getOrderModel() == null || StringUtils.isEmpty(getOrderModel().getCode())
        || getOrderModel().getIsCaptured()) {
      showMessageBox(Localization.getLocalizedString(ERR_MESG_FOR_ALREADY_CAPTURED_ORDER));
      return;
    }
    if (blPaymentService.capturePaymentForOrder(getOrderModel())) {
      showMessageBox(Localization.getLocalizedString(SUCC_MSG_FOR_PAYMENT_CAPTURED));
    } else {
      BlLogger.logMessage(LOG, Level.ERROR, "Error occurred while capturing the payment");
      showMessageBox(Localization.getLocalizedString(ERR_MSG_FOR_PAYMENT_CAPTURED));
    }
  }

  @ViewEvent(componentID = "cancelChanges", eventName = "onClick")
  public void close() {
    this.sendOutput(OUT_CONFIRM, COMPLETE);
  }

  public OrderModel getOrderModel() {
    return orderModel;
  }

  public void setOrderModel(OrderModel orderModel) {
    this.orderModel = orderModel;
  }

  protected void showMessageBox(final String message) {
    Messagebox.show(message);
    this.sendOutput(OUT_CONFIRM, COMPLETE);
  }
}
