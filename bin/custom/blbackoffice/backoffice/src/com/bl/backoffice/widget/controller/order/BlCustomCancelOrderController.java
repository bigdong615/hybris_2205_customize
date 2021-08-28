package com.bl.backoffice.widget.controller.order;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.constants.BlloggingConstants;
import com.bl.core.payment.service.BlPaymentService;
import com.bl.core.services.cancelandrefund.service.BlCustomCancelRefundService;
import com.bl.logging.BlLogger;
import com.braintree.command.request.BrainTreeRefundTransactionRequest;
import com.braintree.command.result.BrainTreeRefundTransactionResult;
import com.braintree.exceptions.BraintreeErrorException;
import com.braintree.facade.backoffice.BraintreeBackofficePartialRefundFacade;
import com.braintree.facade.backoffice.BraintreeBackofficeVoidFacade;
import com.braintree.hybris.data.BrainTreeResponseResultData;
import com.braintree.method.BrainTreePaymentService;
import com.braintree.order.refund.BraintreeRefundService;
import com.braintree.transaction.service.BrainTreeTransactionService;
import com.hybris.backoffice.i18n.BackofficeLocaleService;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.core.events.CockpitEventQueue;
import com.hybris.cockpitng.core.events.impl.DefaultCockpitEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import com.hybris.cockpitng.util.notifications.NotificationService;
import de.hybris.platform.basecommerce.enums.CancelReason;
import de.hybris.platform.basecommerce.enums.RefundReason;
import de.hybris.platform.basecommerce.enums.ReturnAction;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.enumeration.EnumerationService;
import de.hybris.platform.ordercancel.*;
import de.hybris.platform.ordercancel.model.OrderCancelRecordEntryModel;
import de.hybris.platform.payment.AdapterException;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.returns.ReturnService;
import de.hybris.platform.returns.model.RefundEntryModel;
import de.hybris.platform.returns.model.ReturnRequestModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.zkoss.util.Locales;
import org.zkoss.zk.ui.Component;
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
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.*;
import java.util.concurrent.CancellationException;
import java.util.stream.Collectors;

import static com.hybris.backoffice.widgets.notificationarea.event.NotificationEvent.Level.FAILURE;
import static com.hybris.backoffice.widgets.notificationarea.event.NotificationEvent.Level.SUCCESS;
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

    private OrderModel orderModel;
    private final List<String> cancelReasons = new ArrayList<>();
    private transient Map<AbstractOrderEntryModel, Long> orderCancellableEntries;
    private transient Set<BlOrderEntryToCancelDto> orderEntriesToCancel;
    private List<BlOrderEntryToCancelDto> cancelAndRefundEntries;
    private List<BlOrderEntryToCancelDto> refundEntries;

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
    private transient ReturnService returnService;
    @Resource
    private BraintreeBackofficeVoidFacade braintreeBackofficeOrderFacade;
    @Resource
    private BlCustomCancelRefundService blCustomCancelRefundService;
    @Autowired
    BraintreeRefundService braintreeRefundService;

    @Resource
    private BlPaymentService blPaymentService;

    @Resource
    private BrainTreeTransactionService brainTreeTransactionService;

    /**
     * Init cancellation order form.
     *
     * @param inputObject the input object
     */
    @SocketEvent(socketId = BlCustomCancelRefundConstants.INPUT_OBJECT)
    public void initCancellationOrderForm(final OrderModel inputObject) {
        modelService.refresh(inputObject);

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
        if (!validateOrderEnteredQuantityAmountReason()) {
            Messagebox.show(this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_CONFIRM_MSG),
                    this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_CONFIRM_TITLE) + StringUtils.SPACE
                            + this.getOrderModel().getCode(), new Button[]{Button.NO, Button.YES},
                    BlCustomCancelRefundConstants.OMS_WIDGET_CANCELORDER_CONFIRM_ICON, this::processCancelAndRefund);
        }
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
                final OrderCancelRequest orderCancelRequest = this.buildCancelRequest();
                if (CollectionUtils.isNotEmpty(this.cancelAndRefundEntries)) {
                    this.populateAndRefundOnCancelResult(this.getOrderCancelService().requestOrderCancel(orderCancelRequest,
                            this.getUserService().getCurrentUser()));
                }
            } catch (final CancellationException | IllegalArgumentException | OrderCancelException e) {
                BlLogger.logFormattedMessage(LOGGER, ERROR, StringUtils.EMPTY, "Error while cancelling the order!!" + e.getMessage());
                Messagebox.show("Error while cancelling the order!!" + e.getMessage(),
                        "Order cancel error!!", Messagebox.OK, Messagebox.ERROR);
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
                this.refundProcess();
                this.getNotificationService().notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST, SUCCESS,
                        this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_CONFIRM_SUCCESS));
                break;

            case DENIED:
                this.getNotificationService().notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST, FAILURE,
                        this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_CONFIRM_ERROR));
                BlLogger.logFormattedMessage(LOGGER, ERROR, StringUtils.EMPTY, this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_CONFIRM_ERROR));
                Messagebox.show(this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_CONFIRM_ERROR),
                        "Something went wrong!!", Messagebox.OK, Messagebox.ERROR);
        }
    }

    /**
     * process the refund
     */
    private void refundProcess() {
        if (BooleanUtils.isTrue(this.getOrderModel().getIsCaptured())) {
            final Optional<PaymentTransactionEntryModel> captureEntry = blCustomCancelRefundService
                    .getCapturedPaymentTransaction(this.getOrderModel());
            if (captureEntry.isPresent()) {
                this.doRefund(this.globalCancelEntriesSelection.isChecked(), captureEntry.get());
            } else {
                this.voidAuthorizedPaymentAndRefundGiftCard();
            }
        } else {
            this.voidAuthorizedPaymentAndRefundGiftCard();
        }
    }

    /**
     * This method will do void and calculate gift card amount
     */
    private void voidAuthorizedPaymentAndRefundGiftCard() {
        StringBuilder stringBuilder = new StringBuilder("Successfully cancelled");
        final Collection<PaymentTransactionEntryModel> allVoidTransactionModels = braintreeBackofficeOrderFacade
                .getVoidableTransactions(this.getOrderModel());
        final boolean[] status = {Boolean.FALSE};
        if (CollectionUtils.isNotEmpty(allVoidTransactionModels)) {
            executeVoidOnTransactions(stringBuilder, allVoidTransactionModels, status);
        }
        if (this.globalCancelEntriesSelection.isChecked()) {
            final double giftCardAmount = this.deductGiftCartAmount(BlInventoryScanLoggingConstants.ZERO);
            if (giftCardAmount > BlInventoryScanLoggingConstants.ZERO) {
                this.logAmountForGiftCardTransactions((double) BlInventoryScanLoggingConstants.ZERO, String.valueOf(
                        this.getOrderModel().getGiftCardAmount()));
                stringBuilder.append("!! Please create gift card with: " + this.getOrderModel().getGiftCardAmount());
            }
        } else {
            partiallyFullOrderRefund(stringBuilder);
        }
        BlLogger.logFormattedMessage(LOGGER, INFO, StringUtils.EMPTY, stringBuilder.toString());
        Messagebox.show(stringBuilder.toString(), "Success!!", Messagebox.OK, Messagebox.INFORMATION);
    }

    /**
     * refund for full order
     *
     * @param stringBuilder result
     */
    private void partiallyFullOrderRefund(final StringBuilder stringBuilder) {
        double totalAmountToRefund = BlInventoryScanLoggingConstants.ZERO;
        if (CollectionUtils.isNotEmpty(this.cancelAndRefundEntries)) {
            for (final BlOrderEntryToCancelDto orderEntryToCancelDto : this.cancelAndRefundEntries) {
                final AbstractOrderEntryModel orderEntryModel = orderEntryToCancelDto.getOrderEntry();
                final double totAmount = blCustomCancelRefundService.getTotalAmountPerEntry(Math.toIntExact(orderEntryToCancelDto
                    .getQuantityToCancel()), (Math.toIntExact(orderEntryToCancelDto.getQuantityAvailableToCancel())),
                    orderEntryModel.getBasePrice(), (orderEntryModel.getAvalaraLineTax() / (Math.toIntExact(orderEntryToCancelDto
                    .getQuantityAvailableToCancel()))), (Boolean.TRUE.equals(orderEntryModel.getGearGuardWaiverSelected())
                    ? orderEntryModel.getGearGuardWaiverPrice() : orderEntryModel.getGearGuardProFullWaiverPrice()));
                if (((double) orderEntryToCancelDto.getAmount()) <= totAmount) {
                    totalAmountToRefund = totalAmountToRefund + ((double) orderEntryToCancelDto.getAmount());
                }
            }
        }
        if (brainTreeTransactionService.createAuthorizationTransactionOfOrder(this.getOrderModel(), BigDecimal.valueOf(
                this.getOrderModel().getTotalPrice() - totalAmountToRefund), Boolean.TRUE, null)) {
            stringBuilder.append(" and captured payment with remaining amount!!");
        } else {
            stringBuilder.append(" but error occurred during captured payment with remaining amount!!");
        }
    }

    /**
     * track og gift card transactions
     *
     * @param zero value
     * @param s string
     */
    private void logAmountForGiftCardTransactions(final double zero, final String s) {
        this.getOrderModel().setGiftCardAvailableAmount(zero);
        final List<String> gcTransactions = new ArrayList<>();
        gcTransactions.add(s);
        this.getOrderModel().setGiftCardAmountTransactions(gcTransactions);
        modelService.save(this.getOrderModel());
    }

    /**
     * execute void
     *
     * @param stringBuilder string
     * @param allVoidTransactionModels entries
     * @param status result
     */
    private void executeVoidOnTransactions(final StringBuilder stringBuilder, final Collection<PaymentTransactionEntryModel> allVoidTransactionModels,
                                           final boolean[] status) {
        final Collection<PaymentTransactionEntryModel> voidTransactionModels = allVoidTransactionModels.stream()
                .filter(voidEntry -> (voidEntry.getAmount().doubleValue()) > BlInventoryScanLoggingConstants.ONE)
                .collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(voidTransactionModels)) {
            voidTransactionModels.forEach(voidTransaction -> {
                try {
                    braintreeBackofficeOrderFacade.executeVoid(voidTransaction);
                } catch (BraintreeErrorException e) {
                    status[0] = Boolean.TRUE;
                    BlLogger.logFormattedMessage(LOGGER, ERROR, StringUtils.EMPTY, "Error while void the transaction", e.getMessage());
                    Messagebox.show("Error while void the transaction: " + e.getMessage(), "Void transaction Error!!", Messagebox.OK,
                            Messagebox.ERROR);
                }
            });
            if (!status[0]) {
                stringBuilder.append(" and voided order");
            }
        }
    }

    /**
     * This method will start the refund process whether full refund or part refund
     *
     * @param fullRefund   boolean
     * @param captureEntry entry
     */
    private void doRefund(final boolean fullRefund, final PaymentTransactionEntryModel captureEntry) {
        if (Boolean.TRUE.equals(fullRefund)) {
            final double globalTax = this.globalTaxSelection.isChecked() ? this.getOrderModel().getTotalTax() :
                    BlInventoryScanLoggingConstants.ZERO;
            final double globalWaiver = this.globalWaiverSelection.isChecked() ? this.getOrderModel().getTotalDamageWaiverCost() :
                    BlInventoryScanLoggingConstants.ZERO;
            final double globalShipping = this.globalShippingSelection.isChecked() ? this.getOrderModel().getDeliveryCost() :
                    BlInventoryScanLoggingConstants.ZERO;
            final double totalRefundAmount = blCustomCancelRefundService.calculateAmountOnCheckboxStatusFull(this.getOrderModel()
                    .getSubtotal(), globalTax, globalWaiver, globalShipping, Double.parseDouble(this.globalTotalRefundAmount.getValue()));
            double totalOrderRefundedAmount = Double.parseDouble(this.totalRefundedAmount.getValue());
            if (totalOrderRefundedAmount == BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL) {
                doFullRefund(totalRefundAmount);
            } else {
                this.doFullRefundWithPart(captureEntry, totalRefundAmount, (totalRefundAmount + totalOrderRefundedAmount),
                        this.getOrderModel().getTotalPrice());
            }
        } else {
            this.partialRefund(this.cancelAndRefundEntries, captureEntry);
        }
    }

    /**
     * perform part refund
     *
     * @param captureEntry entry
     * @param totalRefundAmount amount
     * @param v status
     * @param totalPrice price
     */
    private void doFullRefundWithPart(final PaymentTransactionEntryModel captureEntry, final double totalRefundAmount,
                                      final double v, final Double totalPrice) {
        if ((v) <= totalPrice) {
            this.doPartRefund(totalRefundAmount, captureEntry);
        } else {
            Messagebox.show(this.getLabel(BlCustomCancelRefundConstants.INVALID_ORDER_AMOUNT),
                    this.getLabel(BlCustomCancelRefundConstants.EMPTY_AMOUNT_HEADER), Messagebox.OK, Messagebox.ERROR);
        }
    }

    /**
     * full refund
     *
     * @param totalRefundAmount amount
     */
    private void doFullRefund(final double totalRefundAmount) {
        if (totalRefundAmount <= this.getOrderModel().getTotalPrice()) {
            this.fullRefund(this.deductGiftCartAmount(totalRefundAmount));
            if(this.getOrderModel().isGiftCardOrder()) {
                logAmountForGiftCardTransactions(Double.parseDouble(String.valueOf(BlInventoryScanLoggingConstants.ZERO)),
                        String.valueOf(this.getOrderModel().getGiftCardAmount()));
            }
        } else {
            Messagebox.show(this.getLabel(BlCustomCancelRefundConstants.INVALID_ORDER_AMOUNT),
                    this.getLabel(BlCustomCancelRefundConstants.EMPTY_AMOUNT_HEADER), Messagebox.OK, Messagebox.ERROR);
        }
    }

    /**
     * full refund
     *
     * @param refundAmount amount
     */
    private void fullRefund(final double refundAmount) {
        try {
            final BrainTreeRefundTransactionRequest request = new BrainTreeRefundTransactionRequest(
                    transactionId.getValue());
            request.setAmount(BigDecimal.valueOf(refundAmount));
            request.setOrderId(this.getOrderModel().getCode());
            request.setTransactionId(transactionId.getValue());
            final BrainTreeRefundTransactionResult result =
                    brainTreePaymentService.refundTransaction(request);
            if (result.isSuccess()) {
                final Map<AbstractOrderEntryModel, Long> returnableEntries = returnService.getAllReturnableEntries(this.getOrderModel());
                if (MapUtils.isNotEmpty(returnableEntries)) {
                    final ReturnRequestModel returnRequestModel = returnService.createReturnRequest(this.getOrderModel());
                    returnableEntries.forEach((orderEntry, qty) -> {
                        final RefundEntryModel refundEntry = returnService.createRefund(returnRequestModel, orderEntry,
                                "Refund Notes while full refund", qty, ReturnAction.IMMEDIATE, RefundReason.WRONGDESCRIPTION);
                        refundEntry.setAmount(BigDecimal.valueOf(orderEntry.getTotalPrice()));
                        modelService.save(refundEntry);
                        returnRequestModel.setSubtotal(
                                returnRequestModel.getReturnEntries().stream().filter(entry -> entry instanceof RefundEntryModel)
                                        .map(refund -> ((RefundEntryModel) refund).getAmount()).reduce(BigDecimal.ZERO, BigDecimal::add));
                        modelService.save(request);
                    });
                }
                BlLogger.logMessage(LOGGER, Level.DEBUG, "Refund Txn has been initiated successfully.");
                Messagebox.show("Order cancelled and Refund Amount has been initiated");
            }
        } catch (final AdapterException e) {
            BlLogger.logMessage(LOGGER, Level.DEBUG, "Failed to initiate refund.");
            Messagebox.show("Order cancelled and Refund Amount has been initiated");
        }
    }

    /**
     * This method will do partial refund by taking how much amount need to be refunded
     *
     * @param orderEntryToCancelDtos pojo
     * @param captureEntry           entry
     */
    private void partialRefund(final Collection<BlOrderEntryToCancelDto> orderEntryToCancelDtos,
                               final PaymentTransactionEntryModel captureEntry) {
        if (CollectionUtils.isNotEmpty(orderEntryToCancelDtos)) {
            for (final BlOrderEntryToCancelDto orderEntryToCancelDto : orderEntryToCancelDtos) {
                final AbstractOrderEntryModel orderEntryModel = orderEntryToCancelDto.getOrderEntry();
                final double csAmount = (double) orderEntryToCancelDto.getAmount();
                final double totAmount = blCustomCancelRefundService.getTotalAmountPerEntry(Math.toIntExact(orderEntryToCancelDto
                                .getQuantityToCancel()), (Math.toIntExact(orderEntryToCancelDto.getQuantityAvailableToCancel())),
                        orderEntryModel.getBasePrice(), (orderEntryModel.getAvalaraLineTax() / (Math.toIntExact(orderEntryToCancelDto
                                .getQuantityAvailableToCancel()))), (Boolean.TRUE.equals(orderEntryModel.getGearGuardWaiverSelected())
                                ? orderEntryModel.getGearGuardWaiverPrice() : orderEntryModel.getGearGuardProFullWaiverPrice()));
                this.doFullRefundWithPart(captureEntry, csAmount, csAmount, totAmount);
            }
        }
    }

    /**
     * This method will do partial refund by taking how much amount need to be refunded
     *
     * @param captureEntry model
     * @param totalAmt     refund amount
     */
    private void doPartRefund(final double totalAmt, final PaymentTransactionEntryModel captureEntry) {
        if (totalAmt > BlInventoryScanLoggingConstants.ZERO) {
            StringBuilder stringSuccess = new StringBuilder("Successfully cancelled");
            final double refundedAmount = Double.parseDouble(this.totalRefundedAmount.getValue());
            final double otherPayment = this.getOrderModel().getTotalPrice() - (this.getOrderModel().isGiftCardOrder()
                    ? this.getOrderModel().getGiftCardAmount() : BlInventoryScanLoggingConstants.ZERO);
            if (refundedAmount < otherPayment && (totalAmt + refundedAmount) > otherPayment) {
                partRefundAndLogResponse((totalAmt - ((otherPayment - refundedAmount) - totalAmt)), captureEntry, stringSuccess);
            } else if(refundedAmount > otherPayment){
                    partRefundAndLogResponse(totalAmt, captureEntry, stringSuccess);
            } else {
                this.logAmountForGiftCardTransactions((this.getOrderModel().getGiftCardAmount() - totalAmt), String.valueOf(totalAmt));
            }
        }
    }

    /**
     * log response
     *
     * @param totalAmt amount
     * @param captureEntry entry
     * @param stringSuccess success
     */
    private void partRefundAndLogResponse(final double totalAmt, final PaymentTransactionEntryModel captureEntry,
                                          final StringBuilder stringSuccess) {
        try {
            if (this.partRefund(totalAmt, captureEntry)) {
                stringSuccess.append(" and refunded!! Please create gift card with amount: " + totalAmt);
            }
        } catch (final BraintreeErrorException e) {
            stringSuccess.append(" but failed to refund!! Please create gift card with amount: " + totalAmt);
        }
        BlLogger.logFormattedMessage(LOGGER, INFO, StringUtils.EMPTY, stringSuccess.toString());
        Messagebox.show(stringSuccess.toString(), "Success!!", Messagebox.OK, Messagebox.INFORMATION);
    }

    /**
     * This method will refund amount in part
     *
     * @param totalAmt     amount
     * @param captureEntry entry
     * @return result
     * @throws BraintreeErrorException if failed
     */
    private boolean partRefund(final double totalAmt, final PaymentTransactionEntryModel captureEntry) throws BraintreeErrorException {
        final BrainTreeResponseResultData refundResult = braintreeBackofficePartialRefundFacade.
                partialRefundTransaction(this.getOrderModel(), captureEntry, BigDecimal.valueOf(totalAmt)
                        .setScale(BlInventoryScanLoggingConstants.TWO, RoundingMode.HALF_EVEN));
        return refundResult.isSuccess();
    }

    /**
     * This method will return amount to create gift card.
     *
     * @param refundAmount amount
     * @return amount
     */
    private double deductGiftCartAmount(final double refundAmount) {
        return this.getOrderModel().isGiftCardOrder() ? (refundAmount - this.getOrderModel().getGiftCardAmount()) : refundAmount;
    }

    /**
     * Build cancel request order cancel request.
     *
     * @return the order cancel request
     */
    private OrderCancelRequest buildCancelRequest() {
        if (this.getOrderModel() != null) {
            cancelAndRefundEntries = new ArrayList<>();
            refundEntries = new ArrayList<>();
            final List<OrderCancelEntry> orderCancelEntries = new ArrayList<>();
            this.getOrderEntriesGridRows().stream().filter(entryRow -> ((Checkbox) entryRow.getFirstChild()).isChecked()).forEach(
                    entry -> {
                        final BlOrderEntryToCancelDto orderEntry = ((Row) entry).getValue();
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
     * Create order cancel entry.
     *
     * @param orderCancelEntries the order cancel entries
     * @param entry              the entry
     */
    private void createOrderCancelEntry(final List<OrderCancelEntry> orderCancelEntries, final Object entry) {
        final BlOrderEntryToCancelDto orderEntryToCancel = (BlOrderEntryToCancelDto) entry;
        final OrderCancelEntry orderCancelEntry = new OrderCancelEntry(
                orderEntryToCancel.getOrderEntry(),
                orderEntryToCancel.getQuantityToCancel(), orderEntryToCancel.getCancelOrderEntryComment(),
                orderEntryToCancel.getSelectedReason());
        orderCancelEntries.add(orderCancelEntry);
    }

    /**
     * This method will validate input events of cancel and refund popup
     */
    private boolean validateOrderEnteredQuantityAmountReason() {
        if (this.globalCancelEntriesSelection.isChecked()) {
            this.validateGlobalSelection();
        } else {
            final Optional<Component> checkedEntry = this.getOrderEntriesGridRows().stream().filter(row -> Boolean.TRUE.equals(((Checkbox) row.getChildren().iterator()
                    .next()).isChecked())).findFirst();
            if (!checkedEntry.isPresent()) {
                Messagebox.show(this.getLabel(BlCustomCancelRefundConstants.CANCEL_CONFIRM_MISSING_SELECT_LINE),
                        this.getLabel(BlCustomCancelRefundConstants.CANCEL_CONFIRM_MISSING_SELECT_LINE_SELECTION), Messagebox.OK, Messagebox.ERROR);
                return Boolean.TRUE;
            }
            this.validateEntries();
        }
        return Boolean.FALSE;
    }

    /**
     * validate entries
     *
     * @return status
     */
    private boolean validateEntries() {
        for (final Component row : this.getOrderEntriesGridRows()) {
            if (((Checkbox) row.getChildren().iterator().next()).isChecked()) {
                final int cancelQty = Integer.parseInt(String.valueOf(((InputElement) row.getChildren().get(BlloggingConstants.TEN)).getRawValue()));
                final int cancellableQty = Integer.parseInt(String.valueOf(((InputElement) row.getChildren().get(BlloggingConstants.NINE)).getRawValue()));
                if (cancelQty == BlCustomCancelRefundConstants.ZERO && cancellableQty != BlCustomCancelRefundConstants.ZERO) {
                    Messagebox.show(this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_MISSING_QUANTITY),
                            this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_MISSING_QUANTITY_HEADER), Messagebox.OK, Messagebox.ERROR);
                    return Boolean.TRUE;
                } else if (cancelQty > cancellableQty) {
                    Messagebox.show(this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_MISSING_QUANTITY_HIGHER),
                            this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_MISSING_QUANTITY_HEADER), Messagebox.OK, Messagebox.ERROR);
                    return Boolean.TRUE;
                }

                if (this.getValidateRefundAmountMessage(row, cancelQty, cancellableQty)) {
                    return Boolean.TRUE;
                }

                if (((Combobox) row.getChildren().get(BlloggingConstants.TWELVE)).getSelectedIndex() == -BlInventoryScanLoggingConstants.ONE &&
                        cancellableQty != BlCustomCancelRefundConstants.ZERO) {
                    Messagebox.show(this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_ERROR_REASON),
                            this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_ERROR_REASON_HEADER), Messagebox.OK, Messagebox.ERROR);
                    return Boolean.TRUE;
                }
            }
        }
        return Boolean.FALSE;
    }

    /**
     * validate order fields
     *
     * @return status
     */
    private boolean validateGlobalSelection() {
        final double amount = Double.parseDouble(this.globalTotalRefundAmount.getValue());
        final double orderTotal = this.getOrderModel().getTotalPrice();
        final double refundedAmount = Double.parseDouble(this.totalRefundedAmount.getValue());
        if ((amount + refundedAmount) > orderTotal) {
            Messagebox.show(this.getLabel(BlCustomCancelRefundConstants.INVALID_ORDER_AMOUNT),
                    this.getLabel(BlCustomCancelRefundConstants.EMPTY_AMOUNT_HEADER), Messagebox.OK, Messagebox.ERROR);
            return Boolean.TRUE;
        }
        if (this.globalCancelReasons.getSelectedIndex() == -BlInventoryScanLoggingConstants.ONE) {
            Messagebox.show(this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_ERROR_REASON),
                    this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_ERROR_REASON_HEADER), Messagebox.OK, Messagebox.ERROR);
            return Boolean.TRUE;
        }
        if(BigDecimal.valueOf(amount).scale() > BlInventoryScanLoggingConstants.TWO) {
            Messagebox.show("Invalid entered amount!! amount should be up to two decimal digits only!!",
                    this.getLabel(BlCustomCancelRefundConstants.EMPTY_AMOUNT_HEADER), Messagebox.OK, Messagebox.ERROR);
            return Boolean.TRUE;
        }
        return Boolean.FALSE;
    }

    /**
     * Gets validate order message.
     *
     * @return the validate order message
     */
    private boolean getValidateRefundAmountMessage(final Component row, final int cancelQty, final int cancellableQty) {
        final double amount = Double.parseDouble(String.valueOf(((InputElement) row.getChildren().get(BlloggingConstants.ELEVEN)).getRawValue()));
        final double amountAlreadyRefunded = Double.parseDouble(String.valueOf(((InputElement) row.getChildren().get(BlloggingConstants.EIGHT)).getRawValue()));

        final double totalProductPrice = this.getTotalProductPriceForCancelQuantity(row, cancelQty, cancellableQty);
        if (amount <= BlCustomCancelRefundConstants.ZERO) {
            Messagebox.show(this.getLabel(BlCustomCancelRefundConstants.ZERO_ORDER_AMOUNT),
                    this.getLabel(BlCustomCancelRefundConstants.EMPTY_AMOUNT_HEADER), Messagebox.OK, Messagebox.ERROR);
            return Boolean.TRUE;
        } else if (amount > totalProductPrice || (amount + amountAlreadyRefunded) > totalProductPrice ||
                (amount + BigDecimal.valueOf(Double.parseDouble(this.totalRefundedAmount.getValue())).setScale(
                        BlInventoryScanLoggingConstants.TWO, RoundingMode.HALF_EVEN).doubleValue()) > this.getOrderModel().getTotalPrice()) {
            Messagebox.show(this.getLabel(BlCustomCancelRefundConstants.INVALID_ORDER_AMOUNT),
                    this.getLabel(BlCustomCancelRefundConstants.EMPTY_AMOUNT_HEADER), Messagebox.OK, Messagebox.ERROR);
            return Boolean.TRUE;
        }
        return Boolean.FALSE;
    }

    /**
     * This method will calculate total price for line item based on enter cancel quantity
     *
     * @param row            line
     * @param cancelQty      quantity
     * @param cancellableQty quantity
     * @return final price
     */
    private double getTotalProductPriceForCancelQuantity(final Component row, int cancelQty, int cancellableQty) {
        final Checkbox tax = ((Checkbox) row.getChildren().get(BlloggingConstants.SIX));
        final Checkbox waiver = ((Checkbox) row.getChildren().get(BlloggingConstants.SEVEN));

        return blCustomCancelRefundService.getTotalAmountPerEntry(cancelQty, cancellableQty, Double.parseDouble(
                String.valueOf(((InputElement) row.getChildren().get(BlloggingConstants.FOUR)).getRawValue())),
                (Double.parseDouble(tax.getLabel()) / cancellableQty), Double.parseDouble(waiver.getLabel()));
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
                    myComponent.addEventListener(BlCustomCancelRefundConstants.ON_CHANGING, event -> {
                        this.autoSelect(event);
                        ((BlOrderEntryToCancelDto) ((Row) event.getTarget().getParent()).getValue())
                                .setQuantityToCancel(Long.valueOf(((InputEvent) event).getValue()));
                        this.populateEntryLevelAmount((Row) event.getTarget().getParent());
                    });
                } else if (myComponent instanceof Textbox) {
                    myComponent.addEventListener(BlCustomCancelRefundConstants.ON_CHANGING, event -> {
                        this.autoSelect(event);
                        ((BlOrderEntryToCancelDto) ((Row) event.getTarget().getParent()).getValue())
                                .setCancelOrderEntryComment(((InputEvent) event).getValue());
                    });
                } else if (myComponent instanceof Doublebox) {
                    myComponent.addEventListener(BlCustomCancelRefundConstants.ON_CHANGE, event -> {
                        this.autoSelect(event);
                        ((BlOrderEntryToCancelDto) ((Row) event.getTarget().getParent()).getValue())
                                .setAmount(Long.valueOf(((InputEvent) event).getValue()));
                    });
                }
            }
        }

        this.globalCancelReasons.addEventListener(BlCustomCancelRefundConstants.ON_SELECT, this::handleGlobalCancelReason);
        this.globalCancelComment.addEventListener(BlCustomCancelRefundConstants.ON_CHANGING, this::handleGlobalCancelComment);
        this.globalCancelEntriesSelection.addEventListener(BlCustomCancelRefundConstants.ON_CHECK, event ->
                this.selectAllEntries());
        this.globalShippingSelection.addEventListener(BlCustomCancelRefundConstants.ON_CHECK, this::calculateOrderRefundAmount);
        this.globalTaxSelection.addEventListener(BlCustomCancelRefundConstants.ON_CHECK, this::calculateOrderRefundAmount);
        this.globalWaiverSelection.addEventListener(BlCustomCancelRefundConstants.ON_CHECK, this::calculateOrderRefundAmount);
    }

    /**
     * this method will calculate amount for order level
     *
     * @param event event
     */
    private void calculateOrderRefundAmount(final Event event) {
        double orderAmount = this.getOrderModel().getSubtotal();

        final boolean shipping = this.globalShippingSelection.isChecked();
        final boolean tax = this.globalTaxSelection.isChecked();
        final boolean waiver = this.globalWaiverSelection.isChecked();
        if (shipping) {
            orderAmount += this.getOrderModel().getDeliveryCost();
        }

        if (tax) {
            orderAmount += this.getOrderModel().getTotalTax();
        }

        if (waiver) {
            orderAmount += this.getOrderModel().getTotalDamageWaiverCost();
        }

        this.globalTotalRefundAmount.setValue(String.valueOf(BigDecimal.valueOf(orderAmount).setScale(
                BlInventoryScanLoggingConstants.TWO, RoundingMode.HALF_EVEN).doubleValue()));
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
        this.populateEntryLevelAmount(row);
    }

    /**
     * this method will populate amount on entry level
     *
     * @param row row
     */
    private void populateEntryLevelAmount(final Row row) {
        final BlOrderEntryToCancelDto myEntry = row.getValue();
        if (myEntry.getQuantityToCancel() > BlInventoryScanLoggingConstants.ZERO && myEntry.getQuantityToCancel() <=
                myEntry.getQuantityAvailableToCancel()) {
            final Checkbox tax = (Checkbox) row.getChildren().get(BlInventoryScanLoggingConstants.SIX);
            final Checkbox waiver = (Checkbox) row.getChildren().get(BlInventoryScanLoggingConstants.SEVEN);
            final double waiverAmount = (myEntry.getOrderEntry().getGearGuardWaiverSelected()
                    ? myEntry.getOrderEntry().getGearGuardWaiverPrice() : myEntry.getOrderEntry().getGearGuardProFullWaiverPrice());
            double refundAmount = myEntry.getOrderEntry().getBasePrice() + (tax.isChecked() ? myEntry.getOrderEntry().getAvalaraLineTax()
                    : BlInventoryScanLoggingConstants.ZERO) + (waiver.isChecked() ? waiverAmount : BlInventoryScanLoggingConstants.ZERO);
            final double finalAmount = BigDecimal.valueOf(refundAmount * myEntry.getQuantityToCancel()).setScale(BlInventoryScanLoggingConstants.TWO,
                    RoundingMode.HALF_EVEN).doubleValue();
            myEntry.setAmount(finalAmount);
            ((Doublebox) row.getChildren().get(BlInventoryScanLoggingConstants.ELEVEN)).setValue(finalAmount);
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
                    this.orderEntriesToCancel.add(new BlOrderEntryToCancelDto(entry, this.cancelReasons, cancellableQty,
                            this.determineDeliveryMode(entry), 0L, false, false,
                            (long) blCustomCancelRefundService.getTotalRefundedAmountOnOrderEntry(blCustomCancelRefundService
                                    .getAllRefundEntriesForOrderEntry(String.valueOf(entry.getEntryNumber()), this.orderModel.getCode(),
                                            Boolean.TRUE)))));
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
        this.globalTotalRefundAmount.setValue(String.valueOf(order.getSubtotal()));
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
