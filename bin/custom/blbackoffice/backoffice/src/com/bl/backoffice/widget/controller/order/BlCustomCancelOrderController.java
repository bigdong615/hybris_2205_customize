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
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderEntryModel;
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
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.zkoss.util.Locales;
import org.zkoss.zk.ui.Component;
import org.zkoss.zk.ui.WrongValueException;
import org.zkoss.zk.ui.event.Event;
import org.zkoss.zk.ui.event.Events;
import org.zkoss.zk.ui.event.InputEvent;
import org.zkoss.zk.ui.event.SelectEvent;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zk.ui.select.annotation.WireVariable;
import org.zkoss.zk.ui.util.Clients;
import org.zkoss.zul.*;
import org.zkoss.zul.Messagebox.Button;
import org.zkoss.zul.impl.InputElement;

import javax.annotation.Resource;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.*;
import java.util.concurrent.CancellationException;
import java.util.stream.Collector;
import java.util.stream.Collectors;

/**
 * ##################### Bl-986, Bl-987, Bl-988 ###################
 * This controller is used for cancelling the order by CS agent and refund the amount if payment
 * has been captured.
 *
 * @author Namrata Lohar
 */
public class BlCustomCancelOrderController extends DefaultWidgetController {
    private static final Logger LOGGER = Logger.getLogger(BlCustomCancelOrderController.class);
    private final List<String> cancelReasons = new ArrayList<>();
    private transient Map<AbstractOrderEntryModel, Long> orderCancellableEntries;
    private transient Set<OrderEntryToCancelDto> orderEntriesToCancel;
    private OrderModel orderModel;

    @Wire
    private Textbox customerName;
    @Wire
    private Textbox globalCancelComment;
    @Wire
    private Textbox transactionId;
    @Wire
    private Textbox totalLineItemPrice;
    @Wire
    private Textbox totalShippingCost;
    @Wire
    private Textbox totalDamageWaiverCost;
    @Wire
    private Textbox totalTax;
    @Wire
    private Textbox totalRefundedAmount;
    @Wire
    private Textbox totalAmount;

    @Wire
    private Combobox globalCancelReasons;

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
     * Init cancellation order form.
     *
     * @param inputObject the input object
     */
    @SocketEvent(socketId = BlCustomCancelRefundConstants.INPUT_OBJECT)
    public void initCancellationOrderForm(final OrderModel inputObject) {
        this.cancelReasons.clear();
        this.globalCancelEntriesSelection.setChecked(false);
        this.setOrderModel(inputObject);
        this.getWidgetInstanceManager().setTitle(this.getWidgetInstanceManager().getLabel(BlCustomCancelRefundConstants.CANCEL_CONFIRM_TITLE)
                + StringUtils.SPACE + this.getOrderModel().getCode());
        this.customerName.setValue(this.getOrderModel().getUser().getDisplayName());
        this.setAmountInTextBox();

        final Locale locale = this.getLocale();
        this.getEnumerationService().getEnumerationValues(CancelReason.class).forEach(reason ->
                this.cancelReasons.add(this.getEnumerationService().getEnumerationName(reason, locale)));
        this.globalCancelReasons.setModel(new ListModelArray<>(this.cancelReasons));

        if(CollectionUtils.isNotEmpty(this.orderModel.getEntries())) {
            this.orderCancellableEntries =  this.orderModel.getEntries().stream().collect(
                    Collectors.toMap(entryModel -> entryModel, entryModel -> ((OrderEntryModel) entryModel).getQuantityPending(),
                            (a, b) -> b));
        }
        /*this.orderCancellableEntries = this.getOrderCancelService().getAllCancelableEntries(this.getOrderModel(), this.getUserService().getCurrentUser());*/

        this.orderEntriesToCancel = new HashSet<>();
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
     * Confirm cancellation.
     */
    @ViewEvent(componentID = BlCustomCancelRefundConstants.CONFIRM_CANCELLATION, eventName = BlCustomCancelRefundConstants.ON_CLICK)
    public void confirmCancellation() {
        for (Component row : this.getOrderEntriesGridRows()) {
            if (((Checkbox) row.getChildren().iterator().next()).isChecked()) {
                final InputElement cancellableQty = (InputElement) row.getChildren().get(BlloggingConstants.EIGHT);
                final InputElement cancelQty = (InputElement) row.getChildren().get(BlloggingConstants.NINE);
                if (cancelQty.getRawValue().equals(BlCustomCancelRefundConstants.ZERO) &&
                        !cancellableQty.getRawValue().equals(BlCustomCancelRefundConstants.ZERO)) {
                    logErrorAndThrowException(cancelQty, BlCustomCancelRefundConstants.CUSTOMERSUPPORTBACKOFFICE_CANCELORDER_MISSING_QUANTITY);
                }
                this.getValidateRefundAmountMessage((InputElement) row.getChildren().get(BlloggingConstants.NINE));
            }
        }

        final ListModelList<OrderEntryToCancelDto> modelList = (ListModelList) this.getOrderEntries().getModel();
        if (modelList.stream().allMatch(entry -> entry.getQuantityToCancel() == BlCustomCancelRefundConstants.ZERO_LONG)) {
            BlLogger.logMessage(LOGGER, org.apache.log4j.Level.DEBUG, this.getLabel(BlCustomCancelRefundConstants.CANCEL_CONFIRM_MISSING_SELECT_LINE));
            throw new WrongValueException(this.globalCancelEntriesSelection, this.getLabel(BlCustomCancelRefundConstants.CANCEL_CONFIRM_MISSING_SELECT_LINE));
        } else {
            modelList.forEach(this::validateOrderEntry);
        }

        //Show confirmation to user
        this.showMessageBox();
    }

    /**
     * Show message box.
     */
    private void showMessageBox() {
        Messagebox.show(this.getLabel(BlCustomCancelRefundConstants.CUSTOMERSUPPORTBACKOFFICE_CANCELORDER_CONFIRM_MSG),
                this.getLabel(BlCustomCancelRefundConstants.CUSTOMERSUPPORTBACKOFFICE_CANCELORDER_CONFIRM_TITLE) + StringUtils.SPACE
                        + this.getOrderModel().getCode(), new Button[]{Button.NO, Button.YES},
                BlCustomCancelRefundConstants.OMS_WIDGET_CANCELORDER_CONFIRM_ICON, this::processCancellation);
    }

    /**
     * Process cancellation.
     *
     * @param obj the obj
     */
    private void processCancellation(final Event obj) {
        BlLogger.logFormattedMessage(LOGGER, org.apache.log4j.Level.INFO, StringUtils.EMPTY,
                BlCustomCancelRefundConstants.CANCELLING_THE_ORDER_FOR_CODE, this.getOrderModel().getCode());
        if (Button.YES.event.equals(obj.getName())) {
            try {
                final OrderCancelRecordEntryModel orderCancelRecordEntry = this.getOrderCancelService()
                        .requestOrderCancel(this.buildCancelRequest(), this.getUserService().getCurrentUser());
                switch (orderCancelRecordEntry.getCancelResult()) {
                    case FULL:

                        break;

                    case PARTIAL:
                        final OrderModel order = this.getOrderModel();
                        if (CollectionUtils.isNotEmpty(order.getGiftCard())) { //Add amount of giftccard
                            this.showMessageBox(Localization.getLocalizedString(BlCustomCancelRefundConstants.CREATE_GIFT_CARD_MESSAGE));
                        } else if (CollectionUtils.isEmpty(order.getGiftCard())
                                && CollectionUtils.isNotEmpty(braintreeBackofficeOrderFacade.getVoidableTransactions(order))) {
                            this.showMessageBox(Localization.getLocalizedString(BlCustomCancelRefundConstants.PAYMENT_VOID_MESSAGE));
                        } else if (order.getIsCaptured()) {
                            this.showMessageBox(Localization.getLocalizedString(BlCustomCancelRefundConstants.REFUND_MESSAGE));
                        }
                        this.getNotificationService().notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST, Level.SUCCESS,
                                this.getLabel(BlCustomCancelRefundConstants.CUSTOMERSUPPORTBACKOFFICE_CANCELORDER_CONFIRM_SUCCESS));
                        break;

                    case DENIED:
                        this.getNotificationService().notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST, Level.FAILURE,
                                this.getLabel(BlCustomCancelRefundConstants.CUSTOMERSUPPORTBACKOFFICE_CANCELORDER_CONFIRM_ERROR));
                }
            } catch (final OrderCancelException | CancellationException | IllegalArgumentException e) {
                BlLogger.logFormattedMessage(LOGGER, org.apache.log4j.Level.ERROR, StringUtils.EMPTY, e,
                        BlCustomCancelRefundConstants.ERROR_OCCURRED_WHILE_PROCESSING_CANCELLATION_OF_ORDER_WITH_NUMBER,
                        this.getOrderModel().getCode());
                this.getNotificationService().notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST, Level.FAILURE,
                        this.getLabel(BlCustomCancelRefundConstants.CUSTOMERSUPPORTBACKOFFICE_CANCELORDER_CONFIRM_ERROR));
            }

            final OrderModel order = this.getModelService().get(this.getOrderModel().getPk());
            order.getEntries().forEach(entry -> this.getCockpitEventQueue()
                    .publishEvent(new DefaultCockpitEvent(BlCustomCancelRefundConstants.OBJECTS_UPDATED, entry, (Object) null)));
            this.sendOutput(BlCustomCancelRefundConstants.CONFIRM_CANCELLATION, BlCustomCancelRefundConstants.COMPLETED);
        }

    }

    /**
     * Gets validate order message.
     *
     * @return the validate order message
     */
    private void getValidateRefundAmountMessage(final InputElement refundAmount) {
        //TODO: consider already selected quantity also while validation!!
        if(refundAmount.getRawValue() == null) {
            logErrorAndThrowException(refundAmount, BlCustomCancelRefundConstants.EMPTY_AMOUNT);
        } else {
            final double amount = Double.parseDouble(String.valueOf(refundAmount.getRawValue()));
            if(amount <= BlCustomCancelRefundConstants.ZERO) {
                logErrorAndThrowException(refundAmount, BlCustomCancelRefundConstants.ZERO_ORDER_AMOUNT);
            } else if(amount > Double.parseDouble(this.totalAmount.getValue())) {
                logErrorAndThrowException(refundAmount, BlCustomCancelRefundConstants.INVALID_ORDER_AMOUNT);
            }
        }
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
        this.transactionId.setValue(CollectionUtils.isEmpty(this.getOrderModel().getPaymentTransactions()) ? StringUtils.EMPTY
                : this.getOrderModel().getPaymentTransactions().get(BlCustomCancelRefundConstants.ZERO).getRequestId());
    }

    /**
     * Format amount string.
     *
     * @param amount the amount
     * @return the string
     */
    private String formatAmount(final Double amount) {
        final DecimalFormat decimalFormat = (DecimalFormat) NumberFormat.getNumberInstance(Locales.getCurrent());
        decimalFormat.applyPattern(BlCustomCancelRefundConstants.ZERO_DOUBLE);
        return decimalFormat.format(amount);
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
            deliveryModeResult = this.getLabel(BlCustomCancelRefundConstants.CANCEL_CONFIRM_PICKUP);
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
    @ViewEvent(componentID = BlCustomCancelRefundConstants.UNDO_CANCELLATION, eventName = BlCustomCancelRefundConstants.ON_CLICK)
    public void reset() {
        this.globalCancelReasons.setSelectedItem(null);
        this.globalCancelComment.setValue(StringUtils.EMPTY);
        this.initCancellationOrderForm(this.getOrderModel());
    }

    /**
     * Add listeners.
     */
    private void addListeners() {
        final List<Component> rows = this.getOrderEntries().getRows().getChildren();
        for (Component row : rows) {
            for (Component myComponent : row.getChildren()) {
                if (myComponent instanceof Checkbox) {
                    myComponent.addEventListener(BlCustomCancelRefundConstants.ON_CHECK, event ->
                            this.handleRow((Row) event.getTarget().getParent()));
                } else if (myComponent instanceof Combobox) {
                    myComponent.addEventListener(BlCustomCancelRefundConstants.ON_CUSTOM_CHANGE, event ->
                            Events.echoEvent(BlCustomCancelRefundConstants.ON_LATER_CUSTOM_CHANGE, myComponent, event.getData()));
                    myComponent.addEventListener(BlCustomCancelRefundConstants.ON_LATER_CUSTOM_CHANGE, event -> {
                        Clients.clearWrongValue(myComponent);
                        myComponent.invalidate();
                        this.handleIndividualCancelReason(event);
                    });
                } else if (myComponent instanceof Intbox) {
                    myComponent.addEventListener(BlCustomCancelRefundConstants.ON_CHANGE, event -> {
                        this.autoSelect(event);
                        ((OrderEntryToCancelDto) ((Row) event.getTarget().getParent()).getValue())
                                .setQuantityToCancel(Long.valueOf(((InputEvent) event).getValue()));
                    });
                } else if (myComponent instanceof Textbox) {
                    myComponent.addEventListener(BlCustomCancelRefundConstants.ON_CHANGING, event -> {
                        this.autoSelect(event);
                        ((OrderEntryToCancelDto) ((Row) event.getTarget().getParent()).getValue())
                                .setCancelOrderEntryComment(((InputEvent) event).getValue());
                    });
                }
            }
        }

        this.globalCancelReasons.addEventListener(BlCustomCancelRefundConstants.ON_SELECT, this::handleGlobalCancelReason);
        this.globalCancelComment.addEventListener(BlCustomCancelRefundConstants.ON_CHANGING, this::handleGlobalCancelComment);
        this.globalCancelEntriesSelection.addEventListener(BlCustomCancelRefundConstants.ON_CHECK, event ->
                this.selectAllEntries());
    }

    /**
     * Apply to grid.
     *
     * @param data          the data
     * @param childrenIndex the children index
     */
    private void applyToGrid(Object data, int childrenIndex) {
        this.getOrderEntriesGridRows().stream().filter(entry -> ((Checkbox) entry.getChildren().iterator().next()).isChecked())
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
        for (Component myComponent : row.getChildren()) {
            if (index == childrenIndex) {
                setValueInRow(data, myComponent);
            }
            ++index;
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
        int index = BlCustomCancelRefundConstants.ZERO;
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
        this.applyToGrid(((InputEvent) event).getValue(), BlloggingConstants.ELEVEN);
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
            this.applyToGrid(this.getReasonIndex(cancelReason.get()), BlloggingConstants.ELEVEN);
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
            this.applyToRow(BlCustomCancelRefundConstants.ZERO, BlloggingConstants.NINE, row);
            this.applyToRow(null, BlloggingConstants.ELEVEN, row);
            this.applyToRow(null, BlloggingConstants.TWELVE, row);
            myEntry.setQuantityToCancel(BlCustomCancelRefundConstants.ZERO_LONG);
            myEntry.setSelectedReason(null);
            myEntry.setCancelOrderEntryComment(null);
        } else {
            this.applyToRow(this.globalCancelReasons.getSelectedIndex(), BlloggingConstants.ELEVEN, row);
            this.applyToRow(this.globalCancelComment.getValue(), BlloggingConstants.TWELVE, row);
            final Optional<CancelReason> reason = this.matchingComboboxCancelReason(
                    this.globalCancelReasons.getSelectedItem() != null ? this.globalCancelReasons.getSelectedItem().getLabel() : null);
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
        return this.getEnumerationService().getEnumerationValues(CancelReason.class).stream().filter(reason ->
                this.getEnumerationService().getEnumerationName(reason, this.getLocale()).equals(cancelReasonLabel)).findFirst();
    }



    /**
     * Select all entries.
     */
    private void selectAllEntries() {
        this.applyToGrid(Boolean.TRUE, BlCustomCancelRefundConstants.ZERO);
        for (Component row : this.getOrderEntriesGridRows()) {
            final Component firstComponent = row.getChildren().iterator().next();
            if (firstComponent instanceof Checkbox) {
                ((Checkbox) firstComponent).setChecked(this.globalCancelEntriesSelection.isChecked());
            }

            this.handleRow((Row) row);
            if (this.globalCancelEntriesSelection.isChecked()) {
                final int cancellableQuantity = Integer
                        .parseInt(((Label) row.getChildren().get(BlloggingConstants.EIGHT)).getValue());
                this.applyToRow(cancellableQuantity, BlloggingConstants.EIGHT, row);
            }
        }

        if (this.globalCancelEntriesSelection.isChecked()) {
            this.orderEntriesToCancel.forEach(entry -> entry
                    .setQuantityToCancel(this.orderCancellableEntries.get(entry.getOrderEntry())));
        }
    }

    /**
     * Target field to apply validation component.
     *
     * @param stringToValidate     the string to validate
     * @param indexTargetComponent the index target component
     * @return the component
     */
    private Component targetFieldToApplyValidation(final String stringToValidate, final int indexTargetComponent) {
        for (Component component : this.getOrderEntriesGridRows()) {
            final Label label = (Label) component.getChildren().get(BlloggingConstants.ONE);
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
        if (entry.getQuantityToCancel() > this.orderCancellableEntries.get(entry.getOrderEntry())) {
            quantity = (InputElement) this.targetFieldToApplyValidation(entry.getOrderEntry().getProduct().getCode(), BlloggingConstants.NINE);
            logErrorAndThrowException(quantity, BlCustomCancelRefundConstants.CUSTOMERSUPPORTBACKOFFICE_CANCELORDER_ERROR_QTYCANCELLED_INVALID);
        } else if (entry.getSelectedReason() != null && entry.getQuantityToCancel() == BlCustomCancelRefundConstants.ZERO_LONG) {
            quantity = (InputElement) this.targetFieldToApplyValidation(entry.getOrderEntry().getProduct().getCode(), BlloggingConstants.NINE);
            logErrorAndThrowException(quantity, BlCustomCancelRefundConstants.CUSTOMERSUPPORTBACKOFFICE_CANCELORDER_MISSING_QUANTITY);
        } else if (entry.getSelectedReason() == null && !entry.getQuantityToCancel().equals(BlCustomCancelRefundConstants.ZERO)) {
            final Combobox reason = (Combobox) this.targetFieldToApplyValidation(entry.getOrderEntry().getProduct().getCode(), BlloggingConstants.ELEVEN);
            logErrorAndThrowException(reason, BlCustomCancelRefundConstants.CUSTOMERSUPPORTBACKOFFICE_CANCELORDER_ERROR_REASON);
        }
    }

    /**
     * This method will log error and throw exception for validation
     *
     * @param cancelQty qty
     * @param message notification
     */
    private void logErrorAndThrowException(final InputElement cancelQty, final String message) {
        BlLogger.logMessage(LOGGER, org.apache.log4j.Level.DEBUG, this.getLabel(message));
        throw new WrongValueException(cancelQty, this.getLabel(message));
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
     * Show message box.
     *
     * @param message the message
     */
    protected void showMessageBox(final String message) {
        Messagebox.show(message);
        this.sendOutput(BlCustomCancelRefundConstants.CONFIRM_CANCELLATION, BlCustomCancelRefundConstants.COMPLETED);
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
}
