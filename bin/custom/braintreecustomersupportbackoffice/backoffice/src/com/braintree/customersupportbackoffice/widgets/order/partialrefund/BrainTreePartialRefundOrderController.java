package com.braintree.customersupportbackoffice.widgets.order.partialrefund;

import static com.braintree.customersupportbackoffice.constants.BraintreecustomersupportbackofficeConstants.PartialRefundOrderAction.WIDGET_MESSAGE_PARTIAL_REFUND_CREATE_SUCCESS_POSTFIX;
import static com.braintree.customersupportbackoffice.constants.BraintreecustomersupportbackofficeConstants.PartialRefundOrderAction.WIDGET_MESSAGE_PARTIAL_REFUND_SUCCESS;
import static com.braintree.customersupportbackoffice.constants.BraintreecustomersupportbackofficeConstants.PartialRefundOrderAction.WIDGET_PARTIAL_REFUND_TITLE;

import com.bl.constants.BlloggingConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.logging.BlLogger;
import com.braintree.customersupportbackoffice.widgets.order.partialrefund.dto.BlOrderPartialRefundDto;
import com.braintree.exceptions.BraintreeErrorException;
import com.braintree.facade.backoffice.BraintreeBackofficePartialRefundFacade;
import com.braintree.hybris.data.BrainTreeResponseResultData;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.payment.enums.PaymentTransactionType;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.zkoss.util.Locales;
import org.zkoss.zk.ui.Component;
import org.zkoss.zk.ui.WrongValueException;
import org.zkoss.zk.ui.event.Event;
import org.zkoss.zk.ui.event.Events;
import org.zkoss.zk.ui.event.InputEvent;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zul.Checkbox;
import org.zkoss.zul.Doublebox;
import org.zkoss.zul.Grid;
import org.zkoss.zul.ListModelList;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Messagebox.Button;
import org.zkoss.zul.Row;
import org.zkoss.zul.Textbox;


/**
 * The type Brain tree partial refund order controller.
 *
 * @author Krishan Vashishth
 */
public class BrainTreePartialRefundOrderController extends DefaultWidgetController {

  private static final Logger LOG = Logger.getLogger(BrainTreePartialRefundOrderController.class);

  private static final String INPUT_OBJECT = "inputObject";
  protected static final String OUT_CONFIRM = "confirmOutput";
  protected static final String COMPLETE = "completed";
  private static final String REFUND_BUTTON = "refundrequest";
  private static final String CANCEL_BUTTON = "cancelChanges";
    private static final String PAYMENT_NOT_CAPTURED = "partial.refundorder.captured.entries.unavailable";
  private static final String DECIMAL_PRECISION = "#0.00";

  private OrderModel order;

  private Double refundAmount;

  @Autowired
  private BraintreeBackofficePartialRefundFacade braintreeBackofficePartialRefundFacade;

  @Wire
  private Textbox totalShippingCost;
  @Wire
  private Textbox orderStatus;
  @Wire
  private Textbox totalTax;
  @Wire
  private Grid orderEntries;
  @Wire
  private Textbox totalOrderAmount;

  /**
   * Initialize.
   *
   * @param order the order
   */
  @SocketEvent(socketId = INPUT_OBJECT)
  public void initialize(final OrderModel order) {
    this.setOrder(order);
    this.getWidgetInstanceManager().setTitle(new StringBuilder(this.getWidgetInstanceManager()
        .getLabel("braintree.backoffice.partial.refund.confirm.title"))
        .append(StringUtils.SPACE).append(this.getOrder().getCode()).toString());
    if (order.getStatus() != null) {
      this.orderStatus.setValue(order.getStatus().getCode());
    }
    setInitialAmount(order);
    final Set<BlOrderPartialRefundDto> orderEntriesToRefund = new HashSet<>();
    order.getEntries()
        .forEach(entry -> orderEntriesToRefund.add(new BlOrderPartialRefundDto(entry)));
    this.getOrderEntries().setModel(new ListModelList<>(orderEntriesToRefund));
    this.getOrderEntries().renderAll();
    this.addListeners();
  }

  /**
   * Add listeners.
   */
  private void addListeners() {
    final List<Component> listComponent = getOrderEntriesGridRows();
    if (CollectionUtils.isEmpty(listComponent)) {
      BlLogger.logMessage(LOG, Level.ERROR,
          this.getLabel("customersupportbackoffice.partial.refundorder.empty.entries"));
      return;
    }
    for (final Component component : listComponent) {
      if (component instanceof Doublebox) {
        component.addEventListener(Events.ON_CHANGE, event -> {
          this.autoSelect(event);
          ((BlOrderPartialRefundDto) ((Row) event.getTarget().getParent()).getValue())
              .setLineItemRefundAmount(Double.valueOf(((InputEvent) event).getValue()));
        });
      }
    }
  }

  /**
   * Gets order entries grid rows.
   *
   * @return the order entries grid rows
   */
  private List<Component> getOrderEntriesGridRows() {
    return this.getOrderEntries().getRows().getChildren();
  }

  /**
   * Validate request.
   */
  private void validateRequest() {
    if (CollectionUtils.isEmpty(getOrderEntriesGridRows())) {
      BlLogger.logMessage(LOG, Level.DEBUG,
          this.getLabel("customersupportbackoffice.partial.refundorder.empty.entries"));
      throw new WrongValueException(String
          .format(this.getLabel("customersupportbackoffice.partial.refundorder.empty.entries"),
              order.getCode()));
    }
    Double amount = 0.0;
    for (final Component component : getOrderEntriesGridRows()) {
      final Doublebox refundElement = (Doublebox) component.getChildren().get(
          BlloggingConstants.FOUR);
      if (refundElement.getValue() > 0) {
        amount += refundElement.getValue();
      }
    }
    if (amount == 0) {
      BlLogger.logMessage(LOG, Level.DEBUG,
          this.getLabel("customersupportbackoffice.partial.refundorder.not.updated.amount"));
      throw new WrongValueException(String.format(this.
              getLabel("customersupportbackoffice.partial.refundorder.not.updated.amount"),
          this.getOrder().getCode()));
    }
    refundAmount = amount;
  }

  /**
   * Process refund.
   *
   * @param obj the obj
   */
  private void processRefund(final Event obj) {
    if (Button.YES.event.equals(obj.getName())) {
      BlLogger.logFormattedMessage(LOG, Level.INFO,  StringUtils.EMPTY,
          "Refunding the amount : {}", refundAmount);
      final Optional<PaymentTransactionEntryModel> capturedEntry =
          this.getOrder().getPaymentTransactions().get(0).getEntries().stream().filter(entry ->
              BlCoreConstants.ACCEPTED.equalsIgnoreCase(entry.getTransactionStatus())
                  && entry.getAmount().doubleValue() > 1
                  && (PaymentTransactionType.CAPTURE.equals(entry.getType()))).findFirst();
      if (capturedEntry.isPresent()) {
        try {
          final BrainTreeResponseResultData refundResult = braintreeBackofficePartialRefundFacade.
              partialRefundTransaction(this.order, capturedEntry.get(),
                  new BigDecimal(refundAmount));
          if (refundResult.isSuccess()) {
            showSuccessMessage(refundResult);
          }
        } catch (final BraintreeErrorException e) {
          BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY,
              "Error while making the refund : {}", refundAmount, e);
          Messagebox.show(e.getMessage(), this.getLabel(WIDGET_PARTIAL_REFUND_TITLE),
              Messagebox.OK, Messagebox.ERROR);
        }
      } else {
          Messagebox.show(String.format(this.getLabel(PAYMENT_NOT_CAPTURED),
              this.getOrder().getCode()), this.getLabel(WIDGET_PARTIAL_REFUND_TITLE),
              Messagebox.OK, Messagebox.ERROR);
          this.close();
      }
    }
  }

  /**
   * Auto select.
   *
   * @param event the event
   */
  private void autoSelect(final Event event) {
    ((Checkbox) event.getTarget().getParent().getChildren().iterator().next()).setChecked(true);
  }

  /**
   * Sets initial amount.
   *
   * @param order the order
   */
  private void setInitialAmount(final OrderModel order) {
    this.totalOrderAmount.setValue(formatAmount(order.getTotalPrice()));
    this.totalTax.setValue(formatAmount(order.getTotalTax()));
    this.totalShippingCost.setValue(formatAmount(order.getDeliveryCost()));
  }

  /**
   * Format amount string.
   *
   * @param amount the amount
   * @return the string
   */
  private String formatAmount(final Double amount) {
    final DecimalFormat decimalFormat = (DecimalFormat) NumberFormat
        .getNumberInstance(Locales.getCurrent());
    decimalFormat.applyPattern(DECIMAL_PRECISION);
    return decimalFormat.format(amount);
  }

  /**
   * Gets order entries.
   *
   * @return the order entries
   */
  public Grid getOrderEntries() {
    return orderEntries;
  }

  /**
   * Confirm refund.
   */
  @ViewEvent(componentID = REFUND_BUTTON, eventName = Events.ON_CLICK)
  public void confirmRefund() {
    try {
      validateRequest();
      this.showMessageBox();
    } catch (final WrongValueException e) {
      BlLogger.logMessage(LOG, Level.DEBUG,
          this.getLabel("customersupportbackoffice.refundorder.input.error"));
      Messagebox.show(e.getMessage(),
          this.getLabel("customersupportbackoffice.refundorder.input.error"),
          Messagebox.OK, Messagebox.ERROR);
    }
    this.close();
  }

  /**
   * Show message box.
   */
  private void showMessageBox() {
    Messagebox.show(this.getLabel("customersupportbackoffice.refundorder.confirm.msg"),
        new StringBuilder(this.getLabel("customersupportbackoffice.refundorder.confirm.title"))
            .append(StringUtils.SPACE).append(refundAmount).toString(),
        new Button[]{Button.NO, Button.YES}, "oms-widget-cancelorder-confirm-icon",
        this::processRefund);
  }

  /**
   * Show success message.
   *
   * @param result the result
   */
  private void showSuccessMessage(final BrainTreeResponseResultData result) {
    Messagebox.show(createSuccessMessage(result), getLabel(WIDGET_PARTIAL_REFUND_TITLE),
        Messagebox.OK, Messagebox.INFORMATION);
  }

  /**
   * Create success message string.
   *
   * @param resendResult the resend result
   * @return the string
   */
  private String createSuccessMessage(final BrainTreeResponseResultData resendResult) {
    final String message = getLabel(WIDGET_MESSAGE_PARTIAL_REFUND_SUCCESS);
    final String messagePostfix = getLabel(WIDGET_MESSAGE_PARTIAL_REFUND_CREATE_SUCCESS_POSTFIX);
    if (StringUtils.isNotBlank(resendResult.getTransactionId())) {
      return String.format("%s %s: %s", message, messagePostfix, resendResult.getTransactionId());
    }
    return message;
  }

  /**
   * Close.
   */
  @ViewEvent(componentID = CANCEL_BUTTON, eventName = Events.ON_CLICK)
  public void close() {
    this.sendOutput(OUT_CONFIRM, COMPLETE);
  }

  public OrderModel getOrder() {
    return order;
  }

  public void setOrder(OrderModel order) {
    this.order = order;
  }

}

