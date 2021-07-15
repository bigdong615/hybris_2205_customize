/**
 *
 */
package com.bl.backoffice.widget.controller;

import com.bl.core.payment.service.BlPaymentService;
import com.bl.logging.BlLogger;
import com.hybris.cockpitng.annotations.ViewEvent;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;

import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.util.localization.Localization;
import javax.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zul.Combobox;
import org.zkoss.zul.ListModelList;

import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import org.zkoss.zul.Messagebox;


/**
 * @author Aditi Sharma
 *
 */
public class CapturePaymentController extends DefaultWidgetController {

  private static final Logger LOG = Logger.getLogger(CapturePaymentController.class);

  private static final String ERR_MESG_FOR_ALREADY_CAPTURED_ORDER = "error.message.already.captured.order";
  private static final String SUCC_MSG_FOR_PAYMENT_CAPTURED = "success.message.payment.captured";

  @Resource
  private BlPaymentService blPaymentService;

  private OrderModel orderModel;

  @Wire
  private Combobox paymentTransactions;

  @SocketEvent(socketId = "inputObject")
  public void init(final ConsignmentModel inputObject) {
    this.getWidgetInstanceManager().setTitle(
        this.getWidgetInstanceManager().getLabel("blbackoffice.capture.payment.confirm.title")
            + " : " + inputObject.getOrder().getCode());
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
        || BooleanUtils.isTrue(getOrderModel().getIsCaptured())) {
      showMessageBox(Localization.getLocalizedString(ERR_MESG_FOR_ALREADY_CAPTURED_ORDER));
      return;
    }
    blPaymentService.capturePaymentForOrder(getOrderModel());
    showMessageBox(Localization.getLocalizedString(SUCC_MSG_FOR_PAYMENT_CAPTURED));
  }

  public OrderModel getOrderModel() {
    return orderModel;
  }

  public void setOrderModel(OrderModel orderModel) {
    this.orderModel = orderModel;
  }

  protected void showMessageBox(final String message) {
    Messagebox.show(message);
  }
}
