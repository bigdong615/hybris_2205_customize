/**
 *
 */
package com.bl.backoffice.widget.controller.order;

import com.bl.constants.BlloggingConstants;
import com.bl.logging.BlLogger;
import com.braintree.facade.backoffice.BraintreeBackofficeVoidFacade;
import com.hybris.backoffice.i18n.BackofficeLocaleService;
import com.hybris.backoffice.widgets.notificationarea.event.NotificationEvent.Level;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.core.events.CockpitEventQueue;
import com.hybris.cockpitng.core.events.impl.DefaultCockpitEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import com.hybris.cockpitng.util.notifications.NotificationService;
import de.hybris.platform.basecommerce.enums.CancelReason;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.enumeration.EnumerationService;
import de.hybris.platform.omsbackoffice.dto.OrderEntryToCancelDto;
import de.hybris.platform.ordercancel.OrderCancelEntry;
import de.hybris.platform.ordercancel.OrderCancelException;
import de.hybris.platform.ordercancel.OrderCancelRequest;
import de.hybris.platform.ordercancel.OrderCancelService;
import de.hybris.platform.ordercancel.model.OrderCancelRecordEntryModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.util.localization.Localization;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CancellationException;
import javax.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.Component;
import org.zkoss.zk.ui.WrongValueException;
import org.zkoss.zk.ui.event.Event;
import org.zkoss.zk.ui.event.Events;
import org.zkoss.zk.ui.event.InputEvent;
import org.zkoss.zk.ui.event.SelectEvent;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zk.ui.select.annotation.WireVariable;
import org.zkoss.zk.ui.util.Clients;
import org.zkoss.zul.Checkbox;
import org.zkoss.zul.Combobox;
import org.zkoss.zul.Comboitem;
import org.zkoss.zul.Grid;
import org.zkoss.zul.Intbox;
import org.zkoss.zul.Label;
import org.zkoss.zul.ListModelArray;
import org.zkoss.zul.ListModelList;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Messagebox.Button;
import org.zkoss.zul.Row;
import org.zkoss.zul.Textbox;
import org.zkoss.zul.impl.InputElement;

/**
 *  ##################### Bl-986 ###################
 *  This controller is used for cancelling the order by CS agent and refund the amount if payment
 *  has been captured.
 *
 * @author Krishan Vashishth
 */
public class BlCustomCancelOrderController extends DefaultWidgetController {

  private static final Logger LOGGER = Logger.getLogger(BlCustomCancelOrderController.class);

  private static final long serialVersionUID = 1L;
  protected static final Object COMPLETED = "completed";
  private static final String CONFIRM_CANCELLATION = "confirmcancellation";
  private static final String CREATE_GIFT_CARD_MESSAGE = "create.gift.card.refund.process";
  private static final String PAYMENT_VOID_MESSAGE = "void.payment.transaction.message";
  private static final String REFUND_MESSAGE = "success.refund.transaction.message";
  private final List<String> cancelReasons = new ArrayList<>();
  private transient Map<AbstractOrderEntryModel, Long> orderCancellableEntries;
  private transient Set<OrderEntryToCancelDto> orderEntriesToCancel;
  private OrderModel orderModel;
  @Wire
  private Textbox orderNumber;
  @Wire
  private Textbox customerName;
  @Wire
  private Combobox globalCancelReasons;
  @Wire
  private Textbox globalCancelComment;
  @Wire
  private Grid orderEntries;
  @Wire
  private Checkbox globalCancelEntriesSelection;
  @WireVariable
  private transient BackofficeLocaleService cockpitLocaleService;
  @WireVariable
  private transient OrderCancelService orderCancelService;
  @WireVariable
  private transient EnumerationService enumerationService;
  @WireVariable
  private transient ModelService modelService;
  @WireVariable
  private transient CockpitEventQueue cockpitEventQueue;
  @WireVariable
  private transient UserService userService;
  @WireVariable
  private transient NotificationService notificationService;

  @Resource
  private BraintreeBackofficeVoidFacade braintreeBackofficeOrderFacade;


  /**
   * Confirm cancellation.
   */
  @ViewEvent(componentID = "confirmcancellation", eventName = "onClick")
  public void confirmCancellation() {
    this.validateRequest();
    this.showMessageBox();
  }

  /**
   * Init cancellation order form.
   *
   * @param inputObject the input object
   */
  @SocketEvent(socketId = "inputObject")
  public void initCancellationOrderForm(OrderModel inputObject) {
    this.cancelReasons.clear();
    this.globalCancelEntriesSelection.setChecked(false);
    this.setOrderModel(inputObject);
    this.getWidgetInstanceManager().setTitle(new StringBuilder(this.getWidgetInstanceManager()
        .getLabel("customersupportbackoffice.cancelorder.confirm.title"))
        .append(StringUtils.SPACE).append(this.getOrderModel().getCode()).toString());
    this.orderNumber.setValue(this.getOrderModel().getCode());
    this.customerName.setValue(this.getOrderModel().getUser().getDisplayName());
    final Locale locale = this.getLocale();
    this.getEnumerationService().getEnumerationValues(CancelReason.class).forEach(reason ->
        this.cancelReasons.add(this.getEnumerationService().getEnumerationName(reason, locale)));
    this.globalCancelReasons.setModel(new ListModelArray<>(this.cancelReasons));
    this.orderEntriesToCancel = new HashSet<>();
    this.orderCancellableEntries = this.getOrderCancelService()
        .getAllCancelableEntries(this.getOrderModel(), this.getUserService().getCurrentUser());
    if (!this.orderCancellableEntries.isEmpty()) {
      this.orderCancellableEntries.forEach((entry, cancellableQty) -> this.orderEntriesToCancel.add(
          new OrderEntryToCancelDto(entry, this.cancelReasons, cancellableQty,
              this.determineDeliveryMode(entry))));
    }

    this.getOrderEntries().setModel(new ListModelList<>(this.orderEntriesToCancel));
    this.getOrderEntries().renderAll();
    this.addListeners();
  }

  /**
   * Determine delivery mode string.
   *
   * @param orderEntry the order entry
   * @return the string
   */
  private String determineDeliveryMode(final AbstractOrderEntryModel orderEntry) {
    String deliveryModeResult;
    if (orderEntry.getDeliveryMode() != null) {
      deliveryModeResult = orderEntry.getDeliveryMode().getName();
    } else if (orderEntry.getDeliveryPointOfService() != null) {
      deliveryModeResult = this.getLabel("customersupportbackoffice.cancelorder.pickup");
    } else {
      if (orderEntry.getOrder().getDeliveryMode() != null) {
        deliveryModeResult =
            orderEntry.getOrder().getDeliveryMode().getName() != null ? orderEntry.getOrder()
                .getDeliveryMode().getName() : orderEntry.getOrder().getDeliveryMode().getCode();
      } else {
        deliveryModeResult = null;
      }
    }
    return deliveryModeResult;
  }

  /**
   * Reset.
   */
  @ViewEvent(componentID = "undocancellation", eventName = "onClick")
  public void reset() {
    this.globalCancelReasons.setSelectedItem(null);
    this.globalCancelComment.setValue("");
    this.initCancellationOrderForm(this.getOrderModel());
  }

  /**
   * Add listeners.
   */
  private void addListeners() {
    final List<Component> rows = this.getOrderEntries().getRows().getChildren();
    final Iterator rowIterator = rows.iterator();

    while (rowIterator.hasNext()) {
      final Component row = (Component) rowIterator.next();
      final Iterator childrentIterator = row.getChildren().iterator();

      while (childrentIterator.hasNext()) {
        final Component myComponent = (Component) childrentIterator.next();
        if (myComponent instanceof Checkbox) {
          myComponent.addEventListener("onCheck", event ->
              this.handleRow((Row) event.getTarget().getParent()));
        } else if (myComponent instanceof Combobox) {
          myComponent.addEventListener("onCustomChange", event ->
              Events.echoEvent("onLaterCustomChange", myComponent, event.getData()));
          myComponent.addEventListener("onLaterCustomChange", event -> {
            Clients.clearWrongValue(myComponent);
            myComponent.invalidate();
            this.handleIndividualCancelReason(event);
          });
        } else if (myComponent instanceof Intbox) {
          myComponent.addEventListener("onChange", event -> {
            this.autoSelect(event);
            ((OrderEntryToCancelDto) ((Row) event.getTarget().getParent()).getValue())
                .setQuantityToCancel(Long.valueOf(((InputEvent) event).getValue()));
          });
        } else if (myComponent instanceof Textbox) {
          myComponent.addEventListener("onChanging", event -> {
            this.autoSelect(event);
            ((OrderEntryToCancelDto) ((Row) event.getTarget().getParent()).getValue())
                .setCancelOrderEntryComment(((InputEvent) event).getValue());
          });
        }
      }
    }

    this.globalCancelReasons.addEventListener("onSelect", this::handleGlobalCancelReason);
    this.globalCancelComment.addEventListener("onChanging", this::handleGlobalCancelComment);
    this.globalCancelEntriesSelection.addEventListener("onCheck", event ->
        this.selectAllEntries());
  }

  /**
   * Apply to grid.
   *
   * @param data          the data
   * @param childrenIndex the children index
   */
  private void applyToGrid(Object data, int childrenIndex) {
    this.getOrderEntriesGridRows().stream()
        .filter(entry -> ((Checkbox) entry.getChildren().iterator().next()).isChecked())
        .forEach(entry -> this.applyToRow(data, childrenIndex, entry));
  }

  /**
   * Apply to row.
   *
   * @param data          the data
   * @param childrenIndex the children index
   * @param row           the row
   */
  private void applyToRow(final Object data, final int childrenIndex, final Component row) {
    int index = 0;
    final Iterator childrentIterator = row.getChildren().iterator();

    while (childrentIterator.hasNext()) {
      final Component myComponent = (Component) childrentIterator.next();
      if (index != childrenIndex) {
        ++index;
      } else {
        setValueInRow(data, myComponent);
        ++index;
      }
    }

  }


  /**
   * Sets value in row.
   *
   * @param data        the data
   * @param myComponent the my component
   */
  private void setValueInRow(final Object data, final Component myComponent) {
    if (myComponent instanceof Checkbox && data != null) {
      ((Checkbox) myComponent).setChecked((Boolean) data);
    }

    if (myComponent instanceof Combobox) {
      if (data == null) {
        ((Combobox) myComponent).setSelectedItem(null);
      } else {
        ((Combobox) myComponent).setSelectedIndex((Integer) data);
      }
    } else if (myComponent instanceof Intbox) {
      ((Intbox) myComponent).setValue((Integer) data);
    } else if (myComponent instanceof Textbox) {
      ((Textbox) myComponent).setValue((String) data);
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
   * Build cancel request order cancel request.
   *
   * @return the order cancel request
   */
  private OrderCancelRequest buildCancelRequest() {
    if (this.getOrderModel() != null) {
      final List<OrderCancelEntry> orderCancelEntries = new ArrayList<>();
      this.getOrderEntriesGridRows().stream()
          .filter(entry -> ((Checkbox) entry.getFirstChild()).isChecked()).forEach(
          entry -> this.createOrderCancelEntry(orderCancelEntries, ((Row) entry).getValue()));
      final OrderCancelRequest orderCancelRequest = new OrderCancelRequest(this.getOrderModel(),
          orderCancelEntries);
      orderCancelRequest.setCancelReason(
          this.matchingComboboxCancelReason(this.globalCancelReasons.getValue())
              .orElse(null));
      orderCancelRequest.setNotes(this.globalCancelComment.getValue());
      return orderCancelRequest;
    }
    return null;
  }

  /**
   * Create order cancel entry.
   *
   * @param orderCancelEntries the order cancel entries
   * @param entry              the entry
   */
  private void createOrderCancelEntry(final List<OrderCancelEntry> orderCancelEntries,
      final Object entry) {
    final OrderEntryToCancelDto orderEntryToCancel = (OrderEntryToCancelDto) entry;
    final OrderCancelEntry orderCancelEntry = new OrderCancelEntry(
        orderEntryToCancel.getOrderEntry(),
        orderEntryToCancel.getQuantityToCancel(), orderEntryToCancel.getCancelOrderEntryComment(),
        orderEntryToCancel.getSelectedReason());
    orderCancelEntries.add(orderCancelEntry);
  }

  /**
   * Gets reason index.
   *
   * @param cancelReason the cancel reason
   * @return the reason index
   */
  private int getReasonIndex(final CancelReason cancelReason) {
    int index = 0;
    final String myReason = this.getEnumerationService()
        .getEnumerationName(cancelReason, this.getCockpitLocaleService().getCurrentLocale());

    for (Iterator reasonsIterator = this.cancelReasons.iterator(); reasonsIterator.hasNext(); ++index) {
      final String reason = (String) reasonsIterator.next();
      if (myReason.equals(reason)) {
        break;
      }
    }

    return index;
  }

  /**
   * Gets selected cancel reason.
   *
   * @param event the event
   * @return the selected cancel reason
   */
  private Optional<CancelReason> getSelectedCancelReason(final Event event) {
    Optional<CancelReason> result = Optional.empty();
    if (!((SelectEvent) event).getSelectedItems().isEmpty()) {
      Object selectedValue = ((Comboitem) ((SelectEvent) event).getSelectedItems().iterator()
          .next()).getValue();
      result = this.matchingComboboxCancelReason(selectedValue.toString());
    }

    return result;
  }

  /**
   * Handle global cancel comment.
   *
   * @param event the event
   */
  private void handleGlobalCancelComment(final Event event) {
    this.applyToGrid(((InputEvent) event).getValue(), BlloggingConstants.SEVEN);
    this.getOrderEntriesGridRows().stream()
        .filter(entry -> ((Checkbox) entry.getChildren().iterator().next()).isChecked())
        .forEach(entry -> {
          final OrderEntryToCancelDto myEntry = ((Row) entry).getValue();
          myEntry.setCancelOrderEntryComment(((InputEvent) event).getValue());
        });
  }

  /**
   * Handle global cancel reason.
   *
   * @param event the event
   */
  private void handleGlobalCancelReason(final Event event) {
    final Optional<CancelReason> cancelReason = this.getSelectedCancelReason(event);
    if (cancelReason.isPresent()) {
      this.applyToGrid(this.getReasonIndex(cancelReason.get()), BlloggingConstants.SIX);
      this.getOrderEntriesGridRows().stream().filter(entry ->
          ((Checkbox) entry.getChildren().iterator().next()).isChecked()
      ).forEach(entry -> {
        final OrderEntryToCancelDto myEntry = ((Row) entry).getValue();
        myEntry.setSelectedReason(cancelReason.get());
      });
    }

  }

  /**
   * Handle individual cancel reason.
   *
   * @param event the event
   */
  private void handleIndividualCancelReason(final Event event) {
    final Optional<CancelReason> cancelReason = this.getCustomSelectedCancelReason(event);
    if (cancelReason.isPresent()) {
      this.autoSelect(event);
      ((OrderEntryToCancelDto) ((Row) event.getTarget().getParent()).getValue())
          .setSelectedReason(cancelReason.get());
    }
  }

  /**
   * Handle row.
   *
   * @param row the row
   */
  private void handleRow(final Row row) {
    final OrderEntryToCancelDto myEntry = row.getValue();
    if (!((Checkbox) row.getChildren().iterator().next()).isChecked()) {
      this.applyToRow(0, BlloggingConstants.FIVE, row);
      this.applyToRow(null, BlloggingConstants.SIX, row);
      this.applyToRow(null, BlloggingConstants.SEVEN, row);
      myEntry.setQuantityToCancel(0L);
      myEntry.setSelectedReason(null);
      myEntry.setCancelOrderEntryComment(null);
    } else {
      this.applyToRow(this.globalCancelReasons.getSelectedIndex(), BlloggingConstants.SIX, row);
      this.applyToRow(this.globalCancelComment.getValue(), BlloggingConstants.SEVEN, row);
      final Optional<CancelReason> reason = this.matchingComboboxCancelReason(
          this.globalCancelReasons.getSelectedItem() != null ? this.globalCancelReasons
              .getSelectedItem().getLabel() : null);
      myEntry.setSelectedReason(reason.orElse((CancelReason) null));
      myEntry.setCancelOrderEntryComment(this.globalCancelComment.getValue());
    }

  }

  /**
   * Gets custom selected cancel reason.
   *
   * @param event the event
   * @return the custom selected cancel reason
   */
  private Optional<CancelReason> getCustomSelectedCancelReason(final Event event) {
    Optional<CancelReason> reason = Optional.empty();
    if (event.getTarget() instanceof Combobox) {
      final Object selectedValue = event.getData();
      reason = this.matchingComboboxCancelReason(selectedValue.toString());
    }

    return reason;
  }

  /**
   * Matching combobox cancel reason optional.
   *
   * @param cancelReasonLabel the cancel reason label
   * @return the optional
   */
  private Optional<CancelReason> matchingComboboxCancelReason(final String cancelReasonLabel) {
    return this.getEnumerationService().getEnumerationValues(CancelReason.class).stream()
        .filter(
            reason -> this.getEnumerationService().getEnumerationName(reason, this.getLocale())
                .equals(cancelReasonLabel)).findFirst();
  }

  /**
   * Process cancellation.
   *
   * @param obj the obj
   */
  private void processCancellation(final Event obj) {
    BlLogger.logFormattedMessage(LOGGER, org.apache.log4j.Level.INFO, StringUtils.EMPTY,
        "Cancelling the order for code : {}", this.getOrderModel().getCode());
    if (Button.YES.event.equals(obj.getName())) {
      try {
        final OrderCancelRecordEntryModel orderCancelRecordEntry = this.getOrderCancelService()
            .requestOrderCancel(this.buildCancelRequest(), this.getUserService().getCurrentUser());
        switch (orderCancelRecordEntry.getCancelResult()) {
          case FULL:
          case PARTIAL:
            final OrderModel order = this.getOrderModel();
            if (CollectionUtils.isNotEmpty(order.getGiftCard())) { //Add amount of giftccard
              this.showMessageBox(Localization.getLocalizedString(CREATE_GIFT_CARD_MESSAGE));
            } else if (CollectionUtils.isEmpty(order.getGiftCard())
                && CollectionUtils.isNotEmpty(braintreeBackofficeOrderFacade.getVoidableTransactions(order))) {
              this.showMessageBox(Localization.getLocalizedString(PAYMENT_VOID_MESSAGE));
            } else if (order.getIsCaptured()) {
              this.showMessageBox(Localization.getLocalizedString(REFUND_MESSAGE));
            }
            this.getNotificationService()
                .notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST, Level.SUCCESS, new Object[]{
                    this.getLabel("customersupportbackoffice.cancelorder.confirm.success")});
            break;
          case DENIED:
            this.getNotificationService()
                .notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST, Level.FAILURE,
                    new Object[]{
                        this.getLabel("customersupportbackoffice.cancelorder.confirm.error")});
        }
      } catch (final OrderCancelException | CancellationException | IllegalArgumentException e) {
        BlLogger.logMessage(LOGGER, org.apache.log4j.Level.ERROR, "Error occurred while ", e);
        this.getNotificationService().notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST, Level.FAILURE,
            new Object[]{this.getLabel("customersupportbackoffice.cancelorder.confirm.error")});
      }

      final OrderModel orderModel = this.getModelService().get(this.getOrderModel().getPk());
      orderModel.getEntries().forEach(entry -> this.getCockpitEventQueue()
          .publishEvent(new DefaultCockpitEvent("objectsUpdated", entry, (Object) null)));
      this.sendOutput(CONFIRM_CANCELLATION, COMPLETED);
    }

  }

  /**
   * Select all entries.
   */
  private void selectAllEntries() {
    this.applyToGrid(Boolean.TRUE, 0);
    final Iterator entriesIterator = this.getOrderEntriesGridRows().iterator();

    while (entriesIterator.hasNext()) {
      final Component row = (Component) entriesIterator.next();
      final Component firstComponent = row.getChildren().iterator().next();
      if (firstComponent instanceof Checkbox) {
        ((Checkbox) firstComponent).setChecked(this.globalCancelEntriesSelection.isChecked());
      }

      this.handleRow((Row) row);
      if (this.globalCancelEntriesSelection.isChecked()) {
        final int cancellableQuantity = Integer
            .parseInt(((Label) row.getChildren().get(BlloggingConstants.FOUR)).getValue());
        this.applyToRow(cancellableQuantity, BlloggingConstants.FIVE, row);
      }
    }

    if (this.globalCancelEntriesSelection.isChecked()) {
      this.orderEntriesToCancel.forEach(entry -> entry
          .setQuantityToCancel(this.orderCancellableEntries.get(entry.getOrderEntry())));
    }

  }

  /**
   * Show message box.
   */
  private void showMessageBox() {
    Messagebox.show(this.getLabel("customersupportbackoffice.cancelorder.confirm.msg"),
        this.getLabel("customersupportbackoffice.cancelorder.confirm.title") + " " + this
            .getOrderModel().getCode(), new Button[]{Button.NO, Button.YES},
        "oms-widget-cancelorder-confirm-icon", this::processCancellation);
  }

  /**
   * Target field to apply validation component.
   *
   * @param stringToValidate     the string to validate
   * @param indexLabelToCheck    the index label to check
   * @param indexTargetComponent the index target component
   * @return the component
   */
  private Component targetFieldToApplyValidation(final String stringToValidate,
      final int indexLabelToCheck, final int indexTargetComponent) {
    final Iterator gridRowIterator = this.getOrderEntriesGridRows().iterator();

    while (gridRowIterator.hasNext()) {
      final Component component = (Component) gridRowIterator.next();
      final Label label = (Label) component.getChildren().get(indexLabelToCheck);
      if (label.getValue().equals(stringToValidate)) {
        return component.getChildren().get(indexTargetComponent);
      }
    }

    return null;
  }

  /**
   * Validate order entry.
   *
   * @param entry the entry
   */
  private void validateOrderEntry(final OrderEntryToCancelDto entry) {
    InputElement quantity;
    if (entry.getQuantityToCancel() > this.orderCancellableEntries
        .get(entry.getOrderEntry())) {
      quantity = (InputElement) this
          .targetFieldToApplyValidation(entry.getOrderEntry().getProduct().getCode(),
              BlloggingConstants.ONE, BlloggingConstants.FIVE);
      BlLogger.logMessage(LOGGER, org.apache.log4j.Level.DEBUG,
          this.getLabel("customersupportbackoffice.cancelorder.error.qtycancelled.invalid"));
      throw new WrongValueException(quantity,
          this.getLabel("customersupportbackoffice.cancelorder.error.qtycancelled.invalid"));
    } else if (entry.getSelectedReason() != null && entry.getQuantityToCancel() == 0L) {
      quantity = (InputElement) this
          .targetFieldToApplyValidation(entry.getOrderEntry().getProduct().getCode(),
              BlloggingConstants.ONE, BlloggingConstants.FIVE);
      BlLogger.logMessage(LOGGER, org.apache.log4j.Level.DEBUG,
          this.getLabel("customersupportbackoffice.cancelorder.missing.quantity"));
      throw new WrongValueException(quantity,
          this.getLabel("customersupportbackoffice.cancelorder.missing.quantity"));
    } else if (entry.getSelectedReason() == null && entry.getQuantityToCancel() > 0L) {
      final Combobox reason = (Combobox) this
          .targetFieldToApplyValidation(entry.getOrderEntry().getProduct().getCode(),
              BlloggingConstants.ONE, BlloggingConstants.SIX);
      BlLogger.logMessage(LOGGER, org.apache.log4j.Level.DEBUG,
          this.getLabel("customersupportbackoffice.cancelorder.error.reason"));
      throw new WrongValueException(reason,
          this.getLabel("customersupportbackoffice.cancelorder.error.reason"));
    }
  }

  /**
   * Validate request.
   */
  private void validateRequest() {
    final Iterator gridRowIterator = this.getOrderEntriesGridRows().iterator();

    while (gridRowIterator.hasNext()) {
      final Component row = (Component) gridRowIterator.next();
      if (((Checkbox) row.getChildren().iterator().next()).isChecked()) {
        final InputElement cancelQty = (InputElement) row.getChildren().get(BlloggingConstants.FIVE);
        if (cancelQty.getRawValue().equals(0)) {
          BlLogger.logMessage(LOGGER, org.apache.log4j.Level.DEBUG,
              this.getLabel("customersupportbackoffice.cancelorder.missing.quantity"));
          throw new WrongValueException(cancelQty,
              this.getLabel("customersupportbackoffice.cancelorder.missing.quantity"));
        }
      }
    }

    final ListModelList<OrderEntryToCancelDto> modelList = (ListModelList) this.getOrderEntries()
        .getModel();
    if (modelList.stream().allMatch(entry -> entry.getQuantityToCancel() == 0L)) {
      BlLogger.logMessage(LOGGER, org.apache.log4j.Level.DEBUG,
          this.getLabel("customersupportbackoffice.cancelorder.missing.selectedLine"));
      throw new WrongValueException(this.globalCancelEntriesSelection,
          this.getLabel("customersupportbackoffice.cancelorder.missing.selectedLine"));
    } else {
      modelList.forEach(this::validateOrderEntry);
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

  private Locale getLocale() {
    return this.getCockpitLocaleService().getCurrentLocale();
  }

  private BackofficeLocaleService getCockpitLocaleService() {
    return this.cockpitLocaleService;
  }

  protected Grid getOrderEntries() {
    return this.orderEntries;
  }

  protected OrderModel getOrderModel() {
    return this.orderModel;
  }

  public void setOrderModel(OrderModel orderModel) {
    this.orderModel = orderModel;
  }

  protected OrderCancelService getOrderCancelService() {
    return this.orderCancelService;
  }

  protected EnumerationService getEnumerationService() {
    return this.enumerationService;
  }

  protected ModelService getModelService() {
    return this.modelService;
  }

  protected CockpitEventQueue getCockpitEventQueue() {
    return this.cockpitEventQueue;
  }

  protected UserService getUserService() {
    return this.userService;
  }

  protected NotificationService getNotificationService() {
    return this.notificationService;
  }

  /**
   * Show message box.
   *
   * @param message the message
   */
  protected void showMessageBox(final String message) {
    Messagebox.show(message);
    this.sendOutput(CONFIRM_CANCELLATION, COMPLETED);
  }
}
