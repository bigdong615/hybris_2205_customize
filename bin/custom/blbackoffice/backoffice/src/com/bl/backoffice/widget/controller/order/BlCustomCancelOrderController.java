package com.bl.backoffice.widget.controller.order;

import com.bl.constants.BlCancelRefundLoggingConstants;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.constants.BlloggingConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.services.cancelandrefund.service.BlCustomCancelRefundService;
import com.bl.logging.BlLogger;
import com.braintree.command.result.BrainTreeRefundTransactionResult;
import com.braintree.exceptions.BraintreeErrorException;
import com.braintree.facade.backoffice.BraintreeBackofficePartialRefundFacade;
import com.braintree.facade.backoffice.BraintreeBackofficeVoidFacade;
import com.braintree.method.BrainTreePaymentService;
import com.hybris.backoffice.i18n.BackofficeLocaleService;
import com.hybris.backoffice.widgets.notificationarea.event.NotificationEvent;
import com.hybris.backoffice.widgets.notificationarea.event.NotificationEvent.Level;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.core.events.CockpitEventQueue;
import com.hybris.cockpitng.core.events.impl.DefaultCockpitEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import com.hybris.cockpitng.util.notifications.NotificationService;
import de.hybris.platform.basecommerce.enums.CancelReason;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.enumeration.EnumerationService;
import de.hybris.platform.ordercancel.*;
import de.hybris.platform.ordercancel.model.OrderCancelRecordEntryModel;
import de.hybris.platform.payment.AdapterException;
import de.hybris.platform.payment.enums.PaymentTransactionType;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.returns.ReturnService;
import de.hybris.platform.returns.model.ReturnRequestModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.util.localization.Localization;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
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
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.*;
import java.util.concurrent.CancellationException;
import java.util.stream.Collectors;

import static org.apache.log4j.Level.ERROR;
import static org.apache.log4j.Level.INFO;

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
    private transient Set<BlOrderEntryToCancelDto> orderEntriesToCancel;
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

    @Wire
    private Checkbox globalShippingSelection;
    @Wire
    private Checkbox globalTaxSelection;
    @Wire
    private Checkbox globalWaiverSelection;
    @Wire
    private Textbox globalTotalRefundAmount;

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

    @Autowired
    private BraintreeBackofficePartialRefundFacade braintreeBackofficePartialRefundFacade;
    @Resource
    private BrainTreePaymentService brainTreePaymentService;
    @Resource
    private ReturnService returnService;

    @Resource
    private BraintreeBackofficeVoidFacade braintreeBackofficeOrderFacade;

    private List<BlOrderEntryToCancelDto> cancelAndRefundEntries;
    private List<BlOrderEntryToCancelDto> refundEntries;
    @Resource
    private BlCustomCancelRefundService blCustomCancelRefundService;

    /**
     * Init cancellation order form.
     *
     * @param inputObject the input object
     */
    @SocketEvent(socketId = BlCustomCancelRefundConstants.INPUT_OBJECT)
    public void initCancellationOrderForm(final OrderModel inputObject) {
        cancelAndRefundEntries = new ArrayList<>();
        refundEntries = new ArrayList<>();

        this.setOrderModel(inputObject);
        this.setAmountInTextBox(this.getOrderModel());

        populateOrderCancelReasons(this.getLocale());
        this.orderEntriesToCancel = new HashSet<>();
        populateOrderCancellableEntries();

        this.getOrderEntries().setModel(new ListModelList<>(this.orderEntriesToCancel));
        this.getOrderEntries().renderAll();
        this.addListeners();
    }

    /**
     * Reset button event of popup
     */
    @ViewEvent(componentID = BlCustomCancelRefundConstants.UNDO_CANCELLATION, eventName = BlCustomCancelRefundConstants.ON_CLICK)
    public void reset() {
        this.globalCancelReasons.setSelectedItem(null);
        this.globalCancelComment.setValue(StringUtils.EMPTY);
        this.initCancellationOrderForm(this.getOrderModel());
    }

    /**
     * Confirm cancellation.
     */
    @ViewEvent(componentID = BlCustomCancelRefundConstants.CONFIRM_CANCELLATION, eventName = BlCustomCancelRefundConstants.ON_CLICK)
    public void confirmCancellation() {
        validateOrderEnteredQuantityAmountReason();
        validateOrderCancellableEntries();

        Messagebox.show(this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_CONFIRM_MSG),
                this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_CONFIRM_TITLE) + StringUtils.SPACE
                        + this.getOrderModel().getCode(), new Button[]{Button.NO, Button.YES},
                BlCustomCancelRefundConstants.OMS_WIDGET_CANCELORDER_CONFIRM_ICON, this::processCancelAndRefund);
    }

    /**
     * This method will process the confirmation with Yes/No event and perform cancellation and refund on order entries!!
     *
     * @param obj event
     */
    private void processCancelAndRefund(final Event obj) {
        BlLogger.logFormattedMessage(LOGGER, INFO, StringUtils.EMPTY, BlCustomCancelRefundConstants.CANCELLING_THE_ORDER_FOR_CODE,
                this.getOrderModel().getCode());
        if (Button.YES.event.equals(obj.getName())) {
            try {
                //TODO: only refund process
                final OrderCancelRequest orderCancelRequest = this.buildCancelRequest();
                if (CollectionUtils.isNotEmpty(cancelAndRefundEntries)) {
                    populateAndRefundOnCancelResult(this.getOrderCancelService().requestOrderCancel(orderCancelRequest,
                            this.getUserService().getCurrentUser()));
                }
            } catch (final OrderCancelException | CancellationException | IllegalArgumentException e) {
                BlLogger.logFormattedMessage(LOGGER, ERROR, StringUtils.EMPTY, e,
                        BlCustomCancelRefundConstants.ERROR_OCCURRED_WHILE_PROCESSING_CANCELLATION_OF_ORDER_WITH_NUMBER,
                        this.getOrderModel().getCode());
                this.getNotificationService().notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST, Level.FAILURE,
                        this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_CONFIRM_ERROR));
            }

            final OrderModel order = this.getModelService().get(this.getOrderModel().getPk());
            order.getEntries().forEach(entry -> this.getCockpitEventQueue()
                    .publishEvent(new DefaultCockpitEvent(BlCustomCancelRefundConstants.OBJECTS_UPDATED, entry, (Object) null)));
            this.sendOutput(BlCustomCancelRefundConstants.CONFIRM_CANCELLATION, BlCustomCancelRefundConstants.COMPLETED);
        }
    }

    /**
     * This method will populate the reponse of cancel result and do refund accordingly
     *
     * @param orderCancelRecordEntry entry
     */
    private void populateAndRefundOnCancelResult(final OrderCancelRecordEntryModel orderCancelRecordEntry) {
        switch (orderCancelRecordEntry.getCancelResult()) {
            case FULL:
            case PARTIAL:
                //TODO: get this values from popup checkbox
                final boolean entryTax = true;
                final boolean entryWaiver = true;
                final Boolean shipping = true;

                this.refund(this.getOrderModel(), this.globalCancelEntriesSelection.isChecked(),
                        this.collectSelectionCheckboxAndCreateMap(entryTax, entryWaiver, shipping, null));
                this.getNotificationService().notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST, Level.SUCCESS,
                        this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_CONFIRM_SUCCESS));
                break;

            case DENIED:
                this.getNotificationService().notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST, Level.FAILURE,
                        this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_CONFIRM_ERROR));
        }
    }

    /**
     * This method will start the refund process
     *
     * @param order            model
     * @param fullRefund       boolean
     * @param costSelectionMap map entries
     */
    private void refund(final OrderModel order, final boolean fullRefund, final Map<String, Object> costSelectionMap) {
        if (Boolean.TRUE.equals(order.getIsCaptured())) {
            //TODO: Gift-Card scenario pending!!
            final Optional<PaymentTransactionEntryModel> capturedEntry = blCustomCancelRefundService.getCapturedPaymentTransaction(order);
            if (capturedEntry.isPresent()) {
                this.doRefund(order, fullRefund, costSelectionMap);
                this.showMessageBox(Localization.getLocalizedString(BlCustomCancelRefundConstants.REFUND_MESSAGE));
            }
            //amount is not captured
        } else {
            voidPaymentTransaction(order);
        }
    }

    /**
     * This method will start the refund process whether full refund or part refund
     *
     * @param order            model
     * @param fullRefund       boolean
     * @param costSelectionMap map entries
     */
    private void doRefund(final OrderModel order, final boolean fullRefund, final Map<String, Object> costSelectionMap) {
        if (fullRefund) {
            final double totalRefundAmount = blCustomCancelRefundService.calculateAmountOnCheckboxStatusFull(
                    ((boolean) costSelectionMap.get("Tax")), ((boolean) costSelectionMap.get("Waiver")),
                    ((boolean) costSelectionMap.get("Shipping")), order);
            double totalOrderRefundedAmount = blCustomCancelRefundService.getTotalRefundedAmountOnOrder(order);
            if (totalOrderRefundedAmount > BlCustomCancelRefundConstants.ZERO) {
                this.fullRefund(totalRefundAmount - totalOrderRefundedAmount);
            } else {
                this.fullRefund(totalRefundAmount);
            }
        } else {
            this.partialRefund(order, this.cancelAndRefundEntries);
        }
    }

    /**
     * This method will do full refund by taking how much amount need to be refunded
     *
     * @param refundAmount amount to refund
     */
    private void fullRefund(final double refundAmount) {
        try {
            final BrainTreeRefundTransactionResult result = blCustomCancelRefundService.createBrainTreeRefundTransactionRequest(
                    transactionId.getValue(), BigDecimal.valueOf(refundAmount), orderModel.getCode());
            if (result.isSuccess()) {
                final Map<AbstractOrderEntryModel, Long> returnableEntries = returnService.getAllReturnableEntries(this.getOrderModel());
                if (MapUtils.isNotEmpty(returnableEntries)) {
                    final ReturnRequestModel returnRequestModel = returnService.createReturnRequest(this.getOrderModel());
                    returnableEntries.forEach((orderEntry, qty) ->
                            blCustomCancelRefundService.createRefundAndPaymentEntry(result, returnRequestModel, orderEntry, qty,
                                    orderEntry.getTotalPrice(), this.getOrderModel(), PaymentTransactionType.REFUND_STANDALONE,
                                    BlCustomCancelRefundConstants.NOTES));
                }
                BlLogger.logMessage(LOGGER, org.apache.log4j.Level.DEBUG, "Refund Txn has been initiated successfully.");
                showMessageBox(this.getLabel(BlCustomCancelRefundConstants.REFUND_COMPLETE_MSG));
                return;
            }
            final String refundErrorMessage = result.getErrorCode() + BlCoreConstants.HYPHEN + result.getErrorMessage();
            BlLogger.logMessage(LOGGER, ERROR, refundErrorMessage);
            this.showMessageBox(refundErrorMessage, this.getLabel(BlCustomCancelRefundConstants.REFUND_STATUS_TITLE));
        } catch (final AdapterException e) {
            BlLogger.logMessage(LOGGER, ERROR, this.getLabel(BlCustomCancelRefundConstants.REFUND_ERROR_MSG), e);
            this.showMessageBox(e.getMessage(), this.getLabel(BlCustomCancelRefundConstants.REFUND_STATUS_TITLE));
            notificationService.notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST, NotificationEvent.Level.FAILURE,
                    this.getLabel(BlCustomCancelRefundConstants.REFUND_ERROR_MSG));
        }
    }

    /**
     * This method will do partial refund by taking how much amount need to be refunded
     *
     * @param order                  model
     * @param orderEntryToCancelDtos pojo
     */
    private void partialRefund(final OrderModel order, final Collection<BlOrderEntryToCancelDto> orderEntryToCancelDtos) {
        if (CollectionUtils.isNotEmpty(orderEntryToCancelDtos)) {
            for (final BlOrderEntryToCancelDto orderEntryToCancelDto : orderEntryToCancelDtos) {
                final AbstractOrderEntryModel orderEntryModel = orderEntryToCancelDto.getOrderEntry();

                final boolean entryTax = true;
                final boolean entryWaiver = true;
                final double amount = 11.0;

                this.doPartRefund(order, orderEntryToCancelDto, orderEntryModel, blCustomCancelRefundService.calculateAmountOnCheckboxStatusPartial(
                        order, orderEntryModel, this.collectSelectionCheckboxAndCreateMap(entryTax, entryWaiver, null, amount),
                        orderEntryToCancelDto.getQuantityToCancel()));
            }
        }
    }

    /**
     * This method will do partial refund by taking how much amount need to be refunded
     *
     * @param order                 model
     * @param orderEntryToCancelDto pojo
     * @param orderEntryModel       entry
     * @param totalAmt              refund amount
     */
    private void doPartRefund(final OrderModel order, final BlOrderEntryToCancelDto orderEntryToCancelDto,
                              final AbstractOrderEntryModel orderEntryModel, final double totalAmt) {
        if (totalAmt <= orderEntryToCancelDto.getOrderEntry().getTotalPrice()) {
            final BrainTreeRefundTransactionResult result = blCustomCancelRefundService.createBrainTreeRefundTransactionRequest(
                    transactionId.getValue(), BigDecimal.valueOf(totalAmt), orderModel.getCode());
            if (result.isSuccess()) {
                blCustomCancelRefundService.createRefundAndPaymentEntry(result, returnService.createReturnRequest(
                        this.getOrderModel()), orderEntryModel, orderEntryToCancelDto.getQuantityToCancel(), totalAmt,
                        order, PaymentTransactionType.REFUND_PARTIAL, BlCustomCancelRefundConstants.NOTES);
            }
        }
    }

    /**
     * Build cancel request order cancel request.
     *
     * @return the order cancel request
     */
    private OrderCancelRequest buildCancelRequest() {
        if (this.getOrderModel() != null) {
            final List<OrderCancelEntry> orderCancelEntries = new ArrayList<>();
            this.getOrderEntriesGridRows().stream().filter(entry -> ((Checkbox) entry.getFirstChild()).isChecked()).forEach(
                    entry -> {
                        final BlOrderEntryToCancelDto orderEntry = (BlOrderEntryToCancelDto) entry;
                        if (orderEntry.getQuantityAvailableToCancel() > BlCustomCancelRefundConstants.ZERO) {
                            cancelAndRefundEntries.add(orderEntry);
                            this.createOrderCancelEntry(orderCancelEntries, ((Row) entry).getValue());
                        } else {
                            refundEntries.add(orderEntry);
                        }
                    });
            if (CollectionUtils.isNotEmpty(orderCancelEntries)) {
                final OrderCancelRequest orderCancelRequest = new OrderCancelRequest(this.getOrderModel(), orderCancelEntries);
                orderCancelRequest.setCancelReason(this.matchingComboboxCancelReason(this.globalCancelReasons.getValue()).orElse(null));
                orderCancelRequest.setNotes(this.globalCancelComment.getValue());
                return orderCancelRequest;
            }
        }
        return null;
    }

    /**
     * This method will validate input events of cancel and refund popup
     */
    private void validateOrderEnteredQuantityAmountReason() {
        for (final Component row : this.getOrderEntriesGridRows()) {
            if (((Checkbox) row.getChildren().iterator().next()).isChecked()) {
                final InputElement cancellableQty = (InputElement) row.getChildren().get(BlloggingConstants.NINE);
                final InputElement cancelQty = (InputElement) row.getChildren().get(BlloggingConstants.TEN);
                if (cancelQty.getRawValue().equals(BlCustomCancelRefundConstants.ZERO) &&
                        !cancellableQty.getRawValue().equals(BlCustomCancelRefundConstants.ZERO)) {
                    logErrorAndThrowException(cancelQty, BlCustomCancelRefundConstants.CANCELORDER_MISSING_QUANTITY);
                }

                final Combobox combobox = (Combobox) row.getChildren().get(BlloggingConstants.TWELVE);
                if (combobox.getSelectedIndex() == -BlInventoryScanLoggingConstants.ONE &&
                        !cancellableQty.getRawValue().equals(BlCustomCancelRefundConstants.ZERO)) {
                    logErrorAndThrowException(combobox, BlCustomCancelRefundConstants.CANCELORDER_ERROR_REASON);
                }

                this.getValidateRefundAmountMessage((InputElement) row.getChildren().get(BlloggingConstants.TEN));
            }
        }
    }

    /**
     * Gets validate order message.
     *
     * @return the validate order message
     */
    private void getValidateRefundAmountMessage(final InputElement refundAmount) {
        //TODO: consider already selected quantity also while validation!!
        if (refundAmount.getRawValue() == null) {
            logErrorAndThrowException(refundAmount, BlCustomCancelRefundConstants.EMPTY_AMOUNT);
        } else {
            final double amount = Double.parseDouble(String.valueOf(refundAmount.getRawValue()));
            if (amount <= BlCustomCancelRefundConstants.ZERO) {
                logErrorAndThrowException(refundAmount, BlCustomCancelRefundConstants.ZERO_ORDER_AMOUNT);
            } else if (amount > Double.parseDouble(this.totalAmount.getValue())) {
                logErrorAndThrowException(refundAmount, BlCustomCancelRefundConstants.INVALID_ORDER_AMOUNT);
            }
        }
    }

    /**
     * This method will validate order cancellable entries of cancel and refund popup
     */
    private void validateOrderCancellableEntries() {
        final ListModelList<BlOrderEntryToCancelDto> modelList = (ListModelList) this.getOrderEntries().getModel();
        if (modelList.stream().allMatch(entry -> entry.getQuantityToCancel() == BlCustomCancelRefundConstants.ZERO_LONG)) {
            BlLogger.logMessage(LOGGER, org.apache.log4j.Level.DEBUG, this.getLabel(BlCustomCancelRefundConstants.CANCEL_CONFIRM_MISSING_SELECT_LINE));
            throw new WrongValueException(this.globalCancelEntriesSelection, this.getLabel(BlCustomCancelRefundConstants.CANCEL_CONFIRM_MISSING_SELECT_LINE));
        } else {
            modelList.forEach(entry -> {
                if (entry.getQuantityToCancel() > this.orderCancellableEntries.get(entry.getOrderEntry())) {
                    logErrorAndThrowException((InputElement) this.targetFieldToApplyValidation(entry.getOrderEntry().getProduct().getCode(),
                            BlloggingConstants.TEN), BlCustomCancelRefundConstants.CANCELORDER_ERROR_QTYCANCELLED_INVALID);
                } else if (entry.getSelectedReason() != null && entry.getQuantityToCancel() == BlCustomCancelRefundConstants.ZERO_LONG) {
                    logErrorAndThrowException((InputElement) this.targetFieldToApplyValidation(entry.getOrderEntry().getProduct().getCode(), BlloggingConstants.TEN),
                            BlCustomCancelRefundConstants.CANCELORDER_MISSING_QUANTITY);
                } else if (entry.getSelectedReason() == null && entry.getQuantityToCancel() > BlCustomCancelRefundConstants.ZERO_LONG) {
                    logErrorAndThrowException((Combobox) this.targetFieldToApplyValidation(entry.getOrderEntry().getProduct().getCode(), BlloggingConstants.TWELVE)
                            , BlCustomCancelRefundConstants.CANCELORDER_ERROR_REASON);
                }
            });
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

    private void voidPaymentTransaction(final OrderModel order) {
        final Collection<PaymentTransactionEntryModel> paymentTransactionModels = braintreeBackofficeOrderFacade
                .getVoidableTransactions(order);
        if (CollectionUtils.isNotEmpty(paymentTransactionModels)) {
            paymentTransactionModels.forEach(voidTransaction -> {
                try {
                    braintreeBackofficeOrderFacade.executeVoid(voidTransaction);
                } catch (BraintreeErrorException e) {
                    e.printStackTrace();
                }
            });
            this.showMessageBox(Localization.getLocalizedString(BlCustomCancelRefundConstants.PAYMENT_VOID_MESSAGE));
        }
    }

    /**
     * @return key value pair of popup selection for refund attributes
     */
    private Map<String, Object> collectSelectionCheckboxAndCreateMap(final boolean tax, final boolean waiver, final Boolean shipping,
                                                                     final Double amount) {
        return blCustomCancelRefundService.collectSelectionCheckboxAndCreateMap(tax, waiver, shipping, amount);
    }

    private double deductGiftCartAmount(final OrderModel orderModel, final double refundAmount) {
        return orderModel.isGiftCardOrder() ? (refundAmount - orderModel.getGiftCardAmount()) : refundAmount;
    }

    /**
     * Add listeners.
     */
    private void addListeners() {
        final List<Component> rows = this.getOrderEntries().getRows().getChildren();
        for (Component row : rows) {
            for (Component myComponent : row.getChildren()) {
                if (myComponent instanceof Checkbox) {
                    myComponent.addEventListener(BlCustomCancelRefundConstants.ON_CHECK, event -> {
                        this.handleRow((Row) event.getTarget().getParent());
                    });
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
                        ((BlOrderEntryToCancelDto) ((Row) event.getTarget().getParent()).getValue())
                                .setQuantityToCancel(Long.valueOf(((InputEvent) event).getValue()));
                        ((BlOrderEntryToCancelDto) ((Row) event.getTarget().getParent()).getValue())
                                .setAmount(Long.valueOf(((InputEvent) event).getValue()));
                    });
                } else if (myComponent instanceof Textbox) {
                    myComponent.addEventListener(BlCustomCancelRefundConstants.ON_CHANGING, event -> {
                        this.autoSelect(event);
                        ((BlOrderEntryToCancelDto) ((Row) event.getTarget().getParent()).getValue())
                                .setCancelOrderEntryComment(((InputEvent) event).getValue());
                    });
                }
            }
        }

        this.globalCancelReasons.addEventListener(BlCustomCancelRefundConstants.ON_SELECT, this::handleGlobalCancelReason);
        this.globalCancelComment.addEventListener(BlCustomCancelRefundConstants.ON_CHANGING, this::handleGlobalCancelComment);
        this.globalCancelEntriesSelection.addEventListener(BlCustomCancelRefundConstants.ON_CHECK, event ->
                this.selectAllEntries());

        /*this.globalShippingSelection.addEventListener(BlCustomCancelRefundConstants.ON_CHECK, event ->
        {
            if (this.globalShippingSelection.isChecked()) {
                this.globalShippingSelection.setChecked(Boolean.TRUE);
            }
        });

        this.globalTaxSelection.addEventListener(BlCustomCancelRefundConstants.ON_CHECK, event ->
                this.selectAllEntries());
        this.globalWaiverSelection.addEventListener(BlCustomCancelRefundConstants.ON_CHECK, event ->
                this.selectAllEntries());*/
    }

    /**
     * Handle row.
     *
     * @param row the row
     */
    private void handleRow(final Row row) {
        final BlOrderEntryToCancelDto myEntry = row.getValue();
        if (!((Checkbox) row.getChildren().iterator().next()).isChecked()) {
            this.applyToRow(BlCustomCancelRefundConstants.ZERO, BlloggingConstants.TEN, row);
            this.applyToRow(null, BlloggingConstants.TWELVE, row);
            this.applyToRow(null, BlloggingConstants.THIRTEEN, row);
            myEntry.setQuantityToCancel(BlCustomCancelRefundConstants.ZERO_LONG);
            myEntry.setSelectedReason(null);
            myEntry.setCancelOrderEntryComment(null);
        } else {
            this.applyToRow(this.globalCancelReasons.getSelectedIndex(), BlloggingConstants.TWELVE, row);
            this.applyToRow(this.globalCancelComment.getValue(), BlloggingConstants.THIRTEEN, row);
            final Optional<CancelReason> reason = this.matchingComboboxCancelReason(
                    this.globalCancelReasons.getSelectedItem() != null ? this.globalCancelReasons.getSelectedItem().getLabel() : null);
            myEntry.setSelectedReason(reason.orElse((CancelReason) null));
            myEntry.setCancelOrderEntryComment(this.globalCancelComment.getValue());
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
            ((BlOrderEntryToCancelDto) ((Row) event.getTarget().getParent()).getValue())
                    .setSelectedReason(cancelReason.get());
        }
    }

    /**
     * This method will populate reasons
     *
     * @param locale local
     */
    private void populateOrderCancelReasons(final Locale locale) {
        this.getEnumerationService().getEnumerationValues(CancelReason.class).forEach(reason ->
                this.cancelReasons.add(this.getEnumerationService().getEnumerationName(reason, locale)));
        this.globalCancelReasons.setModel(new ListModelArray<>(this.cancelReasons));
    }

    /**
     * This method will populate cancellable Entries
     */
    private void populateOrderCancellableEntries() {
        if (CollectionUtils.isNotEmpty(this.orderModel.getEntries())) {
            this.orderCancellableEntries = this.orderModel.getEntries().stream().collect(
                    Collectors.toMap(entryModel -> entryModel, entryModel -> ((OrderEntryModel) entryModel).getQuantityPending(),
                            (a, b) -> b));
        }
        if (!this.orderCancellableEntries.isEmpty()) {
            this.orderCancellableEntries.forEach((entry, cancellableQty) ->
            {
                this.orderEntriesToCancel.add(new BlOrderEntryToCancelDto(entry, this.cancelReasons, cancellableQty,
                        this.determineDeliveryMode(entry), 0L, false, false,
                        (long) blCustomCancelRefundService.getTotalRefundedAmountOnOrderEntry(blCustomCancelRefundService
                                .getAllRefundEntriesForOrderEntry(String.valueOf(entry.getEntryNumber()), this.orderModel.getCode(),
                                        Boolean.TRUE))));
            });
        }
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
                deliveryModeResult = orderEntry.getOrder().getDeliveryMode().getName() != null ? orderEntry.getOrder()
                        .getDeliveryMode().getName() : orderEntry.getOrder().getDeliveryMode().getCode();
            } else {
                deliveryModeResult = null;
            }
        }
        return deliveryModeResult;
    }

    /**
     * Sets amount and other field values in text boxes.
     */
    private void setAmountInTextBox(final OrderModel order) {
        this.cancelReasons.clear();
        this.globalCancelEntriesSelection.setChecked(false);
        this.globalWaiverSelection.setChecked(false);
        this.globalTaxSelection.setChecked(false);
        this.globalShippingSelection.setChecked(false);
        this.getWidgetInstanceManager().setTitle(this.getWidgetInstanceManager().getLabel(BlCustomCancelRefundConstants.CANCEL_CONFIRM_TITLE)
                + StringUtils.SPACE + order.getCode());
        this.customerName.setValue(order.getUser().getDisplayName());
        this.totalLineItemPrice.setValue(formatAmount(order.getSubtotal()));
        this.totalTax.setValue(formatAmount(order.getTotalTax()));
        this.totalDamageWaiverCost.setValue(formatAmount(order.getTotalDamageWaiverCost()));
        this.totalShippingCost.setValue(formatAmount(order.getDeliveryCost()));
        this.totalAmount.setValue(formatAmount(order.getTotalPrice()));
        this.transactionId.setValue(CollectionUtils.isEmpty(order.getPaymentTransactions()) ? StringUtils.EMPTY
                : order.getPaymentTransactions().get(BlCustomCancelRefundConstants.ZERO).getRequestId());
        this.totalRefundedAmount.setValue(String.valueOf(blCustomCancelRefundService.getTotalRefundedAmountOnOrder(order)));
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
     * Create order cancel entry.
     *
     * @param orderCancelEntries the order cancel entries
     * @param entry              the entry
     */
    private void createOrderCancelEntry(final List<OrderCancelEntry> orderCancelEntries,
                                        final Object entry) {
        final BlOrderEntryToCancelDto orderEntryToCancel = (BlOrderEntryToCancelDto) entry;
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
        for (final Iterator reasonsIterator = this.cancelReasons.iterator(); reasonsIterator.hasNext(); ++index) {
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
        this.applyToGrid(((InputEvent) event).getValue(), BlloggingConstants.TWELVE);
        this.getOrderEntriesGridRows().stream()
                .filter(entry -> ((Checkbox) entry.getChildren().iterator().next()).isChecked())
                .forEach(entry -> {
                    final BlOrderEntryToCancelDto myEntry = ((Row) entry).getValue();
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
            this.applyToGrid(this.getReasonIndex(cancelReason.get()), BlloggingConstants.TWELVE);
            this.getOrderEntriesGridRows().stream().filter(entry ->
                    ((Checkbox) entry.getChildren().iterator().next()).isChecked()
            ).forEach(entry -> {
                final BlOrderEntryToCancelDto myEntry = ((Row) entry).getValue();
                myEntry.setSelectedReason(cancelReason.get());
            });
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
                final InputElement cancellableQty = (InputElement) row.getChildren().get(BlloggingConstants.NINE);
                this.applyToRow(Integer.parseInt(String.valueOf(cancellableQty.getRawValue())), BlloggingConstants.NINE, row);
            }
        }
        if (this.globalCancelEntriesSelection.isChecked()) {
            this.orderEntriesToCancel.forEach(entry -> entry
                    .setQuantityToCancel(this.orderCancellableEntries.get(entry.getOrderEntry())));
        }
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
        int index = BlInventoryScanLoggingConstants.ZERO;
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
     * Show message box.
     *
     * @param message the message
     */
    private void showMessageBox(final String message) {
        Messagebox.show(message);
        this.sendOutput(BlCustomCancelRefundConstants.CONFIRM_CANCELLATION, BlCustomCancelRefundConstants.COMPLETED);
    }

    /**
     * Show message box.
     *
     * @param message      the message
     * @param errorMessage the input Error message
     */
    private void showMessageBox(final String message, final String errorMessage) {
        if (StringUtils.isEmpty(errorMessage)) {
            Messagebox.show(message, this.getLabel(BlCustomCancelRefundConstants.REFUND_STATUS_TITLE), Messagebox.OK, Messagebox.INFORMATION);
        } else {
            Messagebox.show(message, this.getLabel(BlCustomCancelRefundConstants.REFUND_STATUS_TITLE), Messagebox.OK, Messagebox.ERROR);
        }
        this.sendOutput(BlCustomCancelRefundConstants.OUT_CONFIRM, BlCustomCancelRefundConstants.COMPLETED);
    }

    /**
     * This method will log error and throw exception for validation
     *
     * @param cancelQty qty
     * @param message   notification
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

    public List<BlOrderEntryToCancelDto> getCancelAndRefundEntries() {
        return cancelAndRefundEntries;
    }

    public void setCancelAndRefundEntries(List<BlOrderEntryToCancelDto> cancelAndRefundEntries) {
        this.cancelAndRefundEntries = cancelAndRefundEntries;
    }

    public List<BlOrderEntryToCancelDto> getRefundEntries() {
        return refundEntries;
    }

    public void setRefundEntries(List<BlOrderEntryToCancelDto> refundEntries) {
        this.refundEntries = refundEntries;
    }
}
