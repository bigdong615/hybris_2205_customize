package com.bl.backoffice.widget.controller.order.refund;

import com.bl.constants.BlloggingConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.logging.BlLogger;
import com.braintree.command.request.BrainTreeRefundTransactionRequest;
import com.braintree.command.result.BrainTreeRefundTransactionResult;
import com.braintree.method.BrainTreePaymentService;
import com.hybris.backoffice.widgets.notificationarea.event.NotificationEvent;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import com.hybris.cockpitng.util.notifications.NotificationService;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.payment.AdapterException;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import javax.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.util.Locales;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zul.Doublebox;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Textbox;

/**
 * ######################### BL-986 ####################### This controller is being used to
 * initiate the refund amount.
 *
 * @author Krishan Vashishth
 */
public class BlOrderRefundController extends DefaultWidgetController {

  private static final Logger LOG = Logger.getLogger(BlOrderRefundController.class);

  private static final String OUT_CONFIRM = "confirmRefund";
  private static final String COMPLETE = "completed";
  private static final String IN_SOCKET = "inputObject";

  @Resource
  private BrainTreePaymentService brainTreePaymentService;

  private OrderModel orderModel;

  @Resource
  private transient NotificationService notificationService;

  @Wire
  private Textbox transactionId;
  @Wire
  private Textbox totalTax;
  @Wire
  private Textbox totalAmount;
  @Wire
  private Textbox totalDamageWaiverCost;
  @Wire
  private Textbox totalShippingCost;
  @Wire
  private Textbox totalLineItemPrice;
  @Wire
  private Doublebox amount;

  @SocketEvent(socketId = IN_SOCKET)
  public void initPartialRefundForm(final OrderModel inputOrder) {
    this.setOrderModel(inputOrder);
    this.getWidgetInstanceManager().setTitle(new StringBuilder(this.getWidgetInstanceManager()
        .getLabel("customersupportbackoffice.refundorder.confirm.title")).append(
        this.getOrderModel().getCode()).toString());
    this.transactionId.setValue(getPaymentTxnId());
    this.setAmountInTextBox();
  }

  /**
   * Sets amount in text box.
   */
  private void setAmountInTextBox() {
    final OrderModel order = this.getOrderModel();
    this.totalLineItemPrice.setValue(formatAmount(order.getSubtotal()));
    this.totalTax.setValue(formatAmount(order.getTotalTax()));
    this.totalDamageWaiverCost.setValue(formatAmount(order.getTotalDamageWaiverCost()));
    this.totalShippingCost.setValue(formatAmount(order.getDeliveryCost()));
    this.totalAmount.setValue(formatAmount(order.getTotalPrice()));
  }

  /**
   * Format amount string.
   *
   * @param amount the amount
   * @return the string
   */
  private String formatAmount(final Double amount) {
    final DecimalFormat decimalFormat = (DecimalFormat) NumberFormat.getNumberInstance(Locales.getCurrent());
    decimalFormat.applyPattern("#0.00");
    return decimalFormat.format(amount);
  }

  @ViewEvent(componentID = "refundrequest", eventName = "onClick")
  public void refundOrderAmount() {
    final String errorMessage = this.getValidateOrderMessage();
    if (StringUtils.isNotEmpty(errorMessage)) {
      showMessageBox(errorMessage, this.getLabel("customersupportbackoffice.refundorder.input.error"));
      return;
    }
    final double refundAmount = this.amount.getValue();
    try {
      final BrainTreeRefundTransactionRequest request = new BrainTreeRefundTransactionRequest(
          transactionId.getValue());
      request.setAmount(BigDecimal.valueOf(refundAmount));
      request.setOrderId(this.getOrderModel().getCode());
      request.setTransactionId(transactionId.getValue());
      final BrainTreeRefundTransactionResult result =
          brainTreePaymentService.refundTransaction(request);
      if (result.isSuccess()) {
        BlLogger.logMessage(LOG, Level.DEBUG, "Refund Txn has been initiated successfully.");
        showMessageBox(this.getLabel("customersupportbackoffice.refundorder.successfull.mesg"));
        return;
      }
      final String refundErrorMessage = new StringBuilder(result.getErrorCode()).append(
          BlCoreConstants.HYPHEN).append(result.getErrorMessage()).toString();
      BlLogger.logMessage(LOG, Level.ERROR, refundErrorMessage);
      this.showMessageBox(refundErrorMessage);
      notificationService.notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST,
          NotificationEvent.Level.FAILURE, refundErrorMessage);
    } catch (final AdapterException e) {
      BlLogger.logMessage(LOG, Level.ERROR,
          this.getLabel("customersupportbackoffice.refundorder.error.mesg"));
      notificationService.notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST,
          NotificationEvent.Level.FAILURE,
          this.getLabel("customersupportbackoffice.refundorder.error.mesg"));
    }
  }

  /**
   * Gets validate order message.
   *
   * @return the validate order message
   */
  private String getValidateOrderMessage() {
    final OrderModel order = this.getOrderModel();
    String message = StringUtils.EMPTY;
    if (OrderStatus.COMPLETED.equals(order.getStatus())) {
      message = this.getLabel("customersupportbackoffice.refundorder.invalid.order.type");
    } else if (CollectionUtils.isEmpty(order.getGiftCard())
        && BooleanUtils.isFalse(order.getIsCaptured())) {
      message = this.getLabel("customersupportbackoffice.refundorder.payment.not.captured");
    } else if (this.amount.getValue() == null) {
      message = this.getLabel("customersupportbackoffice.refundorder.empty.amount");
    } else if (StringUtils.isEmpty(this.transactionId.getValue())) {
      message = this.getLabel("customersupportbackoffice.refundorder.empty.transactionId");
    } else if (this.amount.getValue() <= 0) {
      message = this.getLabel("customersupportbackoffice.refundorder.lessthanzero.amount");
    } else if (this.amount.getValue() > order.getTotalPrice()) {
      message = this.getLabel("customersupportbackoffice.refundorder.higheramount");
    }
    return message;
  }

  /**
   * Method to get the payment transaction Id from the current order.
   *
   * @return the requestId of the payment transaction.
   */
  private String getPaymentTxnId() {
    if (CollectionUtils.isNotEmpty(this.getOrderModel().getPaymentTransactions())) {
      return this.getOrderModel().getPaymentTransactions().get(0).getRequestId();
    }
    return StringUtils.EMPTY;
  }

  /**
   * Show message box.
   *
   * @param message the message
   */
  protected void showMessageBox(final String message) {
    this.showMessageBox(message, StringUtils.EMPTY);
  }

  /**
   * Show message box.
   *
   * @param message the message
   * @param errorMessage the input Error message
   */
  protected void showMessageBox(final String message, final String errorMessage) {
    if (StringUtils.isNotEmpty(errorMessage)) {
      Messagebox.show(message,
          this.getLabel("customersupportbackoffice.refundorder.input.error"),
          Messagebox.OK, Messagebox.ERROR);
    } else {
      Messagebox.show(message);
    }
    this.sendOutput(OUT_CONFIRM, COMPLETE);
  }

  /**
   * Method will be invoked when user will click on CANCEL button
   */
  @ViewEvent(componentID = "cancelChanges", eventName = "onClick")
  public void close() {
    this.sendOutput(OUT_CONFIRM, COMPLETE);
  }

  public OrderModel getOrderModel() {
    return orderModel;
  }

  public void setOrderModel(final OrderModel orderModel) {
    this.orderModel = orderModel;
  }

  public NotificationService getNotificationService() {
    return notificationService;
  }

  public void setNotificationService(final NotificationService notificationService) {
    this.notificationService = notificationService;
  }
}
