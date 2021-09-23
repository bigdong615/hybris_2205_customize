package com.bl.backoffice.widget.controller.order;

import static org.apache.log4j.Level.DEBUG;

import de.hybris.platform.basecommerce.enums.CancelReason;
import de.hybris.platform.basecommerce.enums.RefundReason;
import de.hybris.platform.basecommerce.enums.ReturnAction;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.enumeration.EnumerationService;
import de.hybris.platform.order.CalculationService;
import de.hybris.platform.order.exceptions.CalculationException;
import de.hybris.platform.ordercancel.OrderCancelEntry;
import de.hybris.platform.ordercancel.OrderCancelException;
import de.hybris.platform.ordercancel.OrderCancelRequest;
import de.hybris.platform.ordercancel.OrderCancelService;
import de.hybris.platform.ordercancel.model.OrderCancelRecordEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.payment.AdapterException;
import de.hybris.platform.payment.enums.PaymentTransactionType;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.returns.ReturnService;
import de.hybris.platform.returns.model.RefundEntryModel;
import de.hybris.platform.returns.model.ReturnRequestModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import javax.annotation.Resource;

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
import org.zkoss.zul.Checkbox;
import org.zkoss.zul.Combobox;
import org.zkoss.zul.Comboitem;
import org.zkoss.zul.Doublebox;
import org.zkoss.zul.Grid;
import org.zkoss.zul.Intbox;
import org.zkoss.zul.ListModelArray;
import org.zkoss.zul.ListModelList;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Messagebox.Button;
import org.zkoss.zul.Row;
import org.zkoss.zul.Textbox;
import org.zkoss.zul.impl.InputElement;

import com.bl.backoffice.consignment.service.BlConsignmentService;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.constants.BlloggingConstants;
import com.bl.core.payment.service.BlPaymentService;
import com.bl.core.services.cancelandrefund.service.BlCustomCancelRefundService;
import com.bl.core.stock.BlStockLevelDao;
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
    private Map<AbstractOrderEntryModel, Long> orderCancellableEntries;
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
    @WireVariable
    private transient CalculationService calculationService;

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
    
    @Resource(name = "blStockLevelDao")
  	private BlStockLevelDao blStockLevelDao;
    
 	@Resource(name = "defaultBlConsignmentService")
 	private BlConsignmentService defaultBlConsignmentService;

    /**
     * Init cancellation order form.
     *
     * @param inputObject the input object
     */
    @SocketEvent(socketId = BlCustomCancelRefundConstants.INPUT_OBJECT)
    public void initCancellationOrderForm(final OrderModel inputObject) {
        modelService.refresh(inputObject);
        if(inputObject.getOriginalOrderTotalAmount() == null || inputObject.getOriginalOrderTotalAmount() ==
                BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL) {
            if(inputObject.getGrandTotal() > BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL) {
                inputObject.setOriginalOrderTotalAmount(inputObject.getGrandTotal());
            } else {
                inputObject.setOriginalOrderTotalAmount(inputObject.getTotalPrice());
            }
            modelService.save(inputObject);
            modelService.refresh(inputObject);
        }

        this.setOrderModel(inputObject);
        this.setAmountInTextBox(this.getOrderModel());

        this.getEnumerationService().getEnumerationValues(CancelReason.class).forEach(reason ->
                this.cancelReasons.add(this.getEnumerationService().getEnumerationName(reason, this.getLocale())));
        this.globalCancelReasons.setModel(new ListModelArray<>(this.cancelReasons));

        this.orderEntriesToCancel = new HashSet<>();
        this.orderCancellableEntries = this.getOrderCancelService().getAllCancelableEntries(this.getOrderModel(),
             this.getUserService().getCurrentUser());

        if (!this.orderCancellableEntries.isEmpty()) {
            this.orderCancellableEntries.forEach((entry, cancellableQty) ->
                    this.orderEntriesToCancel.add(new BlOrderEntryToCancelDto(entry, this.cancelReasons, cancellableQty,
                            this.determineDeliveryMode(entry), 0L, false, false,
                            (long) blCustomCancelRefundService.getTotalRefundedAmountOnOrderEntry(blCustomCancelRefundService
                                    .getAllRefundEntriesForOrderEntry(String.valueOf(entry.getEntryNumber()), this.orderModel.getCode(),
                                            Boolean.TRUE)))));
        }

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
        if (Boolean.FALSE.equals(validateOrderEnteredQuantityAmountReason())) {
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
        if (Button.YES.event.equals(obj.getName())) {
            BlLogger.logFormattedMessage(LOGGER, DEBUG, StringUtils.EMPTY, BlCustomCancelRefundConstants.CANCELLING_THE_ORDER_FOR_CODE,
                    this.getOrderModel().getCode());
            if (this.buildCancelRequest() != null) {
                doCallToRefundProcess();
                try {
                    calculationService.recalculate(this.getOrderModel());
                } catch (final CalculationException e) {
                    BlLogger.logFormattedMessage(LOGGER, DEBUG, StringUtils.EMPTY, BlCustomCancelRefundConstants.CART_RECALCULATION_ERROR,
                            this.getOrderModel().getCode(), e.getMessage());
                }
            }

            final OrderModel order = this.getModelService().get(this.getOrderModel().getPk());
            order.getEntries().forEach(entry -> this.getCockpitEventQueue()
                    .publishEvent(new DefaultCockpitEvent(BlCustomCancelRefundConstants.OBJECTS_UPDATED, entry, (Object) null)));
            this.sendOutput(BlCustomCancelRefundConstants.CONFIRM_CANCELLATION, BlCustomCancelRefundConstants.COMPLETED);
        }
    }

    /**
     * This method will process the refund flow if capture or void flow if authorized
     */
    private void doCallToRefundProcess() {
        final Optional<PaymentTransactionEntryModel> captureEntry = blCustomCancelRefundService.getCapturedPaymentTransaction(
                this.getOrderModel());
        if(captureEntry.isPresent()) {
            this.refundProcess(captureEntry.get());
        } else {
            this.refundProcess(null);
        }
    }
    
    /**
     * process the refund
     *
     * @param captureEntry capture transaction details
     */
    private void refundProcess(final PaymentTransactionEntryModel captureEntry) {
        if (BooleanUtils.isTrue(this.getOrderModel().getIsCaptured()) && null != captureEntry) {
            this.doRefund(this.globalCancelEntriesSelection.isChecked(), captureEntry);
        } else {
            Map<String, String> responseMap = new HashMap<>();
            final Collection<PaymentTransactionEntryModel> allVoidTransactionModels = braintreeBackofficeOrderFacade
                    .getVoidableTransactions(this.getOrderModel());
            if (CollectionUtils.isNotEmpty(allVoidTransactionModels)) {
                responseMap = this.executeVoidOnTransactions(allVoidTransactionModels.stream().filter(voidEntry ->
                        (voidEntry.getAmount().doubleValue()) > BlInventoryScanLoggingConstants.ONE).collect(Collectors.toList()));
            }
            if(Boolean.TRUE.equals(responseMap.containsKey(BlCustomCancelRefundConstants.FAILED))) {
                BlLogger.logFormattedMessage(LOGGER, DEBUG, StringUtils.EMPTY, BlCustomCancelRefundConstants.FAILED_TO_CANCEL_DUE_TO_PAYMENT_GATEWAY_ERROR,
                this.getOrderModel().getCode());
                Messagebox.show(BlCustomCancelRefundConstants.FAILED_TO_CANCEL_DUE_TO_PAYMENT_GATEWAY_ERROR_MSG, BlCustomCancelRefundConstants.FAILURE,
                        Messagebox.OK, Messagebox.ERROR);
            } else {
                this.voidAuthorizedPaymentAndRefundGiftCard();
            }
        }
    }

    /**
     * execute void
     *
     * @param allVoidTransactionModels entries
     * @return responseMap
     */
    private Map<String, String> executeVoidOnTransactions(final Collection<PaymentTransactionEntryModel> allVoidTransactionModels) {
        final Map<String, String> responseMap = new HashMap<>();
        if (CollectionUtils.isNotEmpty(allVoidTransactionModels)) {
            allVoidTransactionModels.forEach(voidTransaction -> {
                try {
                    braintreeBackofficeOrderFacade.executeVoid(voidTransaction);
                } catch (final BraintreeErrorException e) {
                    responseMap.put(BlCustomCancelRefundConstants.FAILED, e.getMessage());
                }
            });
        }
        return responseMap;
    }

    /**
     * This method will do void and calculate gift card amount
     *
     */
    private void voidAuthorizedPaymentAndRefundGiftCard() {
        StringBuilder resultBuilder = new StringBuilder(BlCustomCancelRefundConstants.SUCCESSFULLY_CANCELLED);
        if (this.globalCancelEntriesSelection.isChecked()) {
            if(null == this.cancelOrder()) {
                BlLogger.logFormattedMessage(LOGGER, DEBUG, StringUtils.EMPTY, BlCustomCancelRefundConstants.FAILED_TO_CANCEL_ORDER_PLEASE_TRY_AGAIN_LATER,
                this.getOrderModel().getCode());
                Messagebox.show(BlCustomCancelRefundConstants.FAILED_TO_CANCEL_ORDER_PLEASE_TRY_AGAIN_LATER_MSG, BlCustomCancelRefundConstants.FAILURE,
                        Messagebox.OK, Messagebox.ERROR);
            } else {
                this.cancelFUllOrderByLoggingGiftCardTransactions(resultBuilder);
            }
        } else {
            if (this.partiallyFullOrderRefund() && null != this.cancelOrder()) {
                resultBuilder.append(BlCustomCancelRefundConstants.AND_CAPTURED_PAYMENT_WITH_REMAINING_AMOUNT);
                BlLogger.logFormattedMessage(LOGGER, DEBUG, StringUtils.EMPTY, resultBuilder.toString(), this.getOrderModel().getCode());
                Messagebox.show(BlCustomCancelRefundConstants.SUCCESS_CANCEL_REFUND, BlCustomCancelRefundConstants.SUCCESS,
                        Messagebox.OK, Messagebox.INFORMATION);
            } else {
                BlLogger.logFormattedMessage(LOGGER, DEBUG, StringUtils.EMPTY, BlCustomCancelRefundConstants.
                        FAILED_TO_CANCEL_ORDER_AS_ERROR_OCCURRED_DURING_AUTHORIZATION, this.getOrderModel().getCode());
                Messagebox.show(BlCustomCancelRefundConstants.FAILED_TO_CANCEL_ORDER_AS_ERROR_OCCURRED_DURING_AUTHORIZATION_MSG,
                        BlCustomCancelRefundConstants.FAILURE, Messagebox.OK, Messagebox.ERROR);
            }
        }

    }

    /**
     * cancel full order with void any existing auth entry and log gift card transactions if any
     *
     * @param resultBuilder message string
     */
    private void cancelFUllOrderByLoggingGiftCardTransactions(StringBuilder resultBuilder) {
        if(this.getOrderModel().getGiftCardAmount() > BlInventoryScanLoggingConstants.ZERO &&
            this.getGiftCardAmount() > BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL) {
            this.logAmountForGiftCardTransactions(BlInventoryScanLoggingConstants.ZERO);
            resultBuilder.append(BlCustomCancelRefundConstants.PLEASE_CREATE_GIFT_CARD_WITH + (this.getGiftCardAmount()));
        }

        BlLogger.logFormattedMessage(LOGGER, DEBUG, StringUtils.EMPTY, resultBuilder.toString() + BlCustomCancelRefundConstants.FOR_ORDER,
                this.getOrderModel().getCode());
        Messagebox.show(resultBuilder.toString(), BlCustomCancelRefundConstants.SUCCESS, Messagebox.OK, Messagebox.INFORMATION);
    }

    /**
     * refund for full order which is not captured
     *
     * @return new auth and capture status
     */
    private boolean partiallyFullOrderRefund() {
        double totalAmountToRefund = BlInventoryScanLoggingConstants.ZERO;
        if(Boolean.FALSE.equals(this.isAllEntriesChecked(this.cancelAndRefundEntries))) {
            for (final BlOrderEntryToCancelDto orderEntryToCancelDto : this.cancelAndRefundEntries) {
                final AbstractOrderEntryModel orderEntryModel = orderEntryToCancelDto.getOrderEntry();
                final double totAmount = blCustomCancelRefundService.getTotalAmountPerEntry(Math.toIntExact(orderEntryToCancelDto
                    .getQuantityToCancel()), (Math.toIntExact(orderEntryToCancelDto.getQuantityAvailableToCancel())),
                    orderEntryModel.getBasePrice(), (orderEntryModel.getAvalaraLineTax() / (Math.toIntExact(orderEntryToCancelDto
                    .getQuantityAvailableToCancel()))), (Boolean.TRUE.equals(orderEntryModel.getGearGuardWaiverSelected())
                    ? orderEntryModel.getGearGuardWaiverPrice() : (Boolean.TRUE.equals(orderEntryModel.getGearGuardProFullWaiverSelected())
                    ? orderEntryModel.getGearGuardProFullWaiverPrice() : BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL)));
                if (orderEntryToCancelDto.getAmount() <= totAmount) {
                    totalAmountToRefund = totalAmountToRefund + orderEntryToCancelDto.getAmount();
                }
            }
            BlLogger.logFormattedMessage(LOGGER, DEBUG, StringUtils.EMPTY, BlCustomCancelRefundConstants.TOTAL_REFUND_AMOUNT_FOR_FULL_ORDER_REFUND_AFTER_PART_REFUND,
                    totalAmountToRefund, this.getOrderModel().getCode());

            return this.partiallyFullOrderRefundGCScenario(totalAmountToRefund);
        } else {
            final double refundedAmount = Double.parseDouble(this.totalRefundedAmount.getValue());
            final double otherPayment = this.getOrderModel().getOriginalOrderTotalAmount() - this.getGiftCardAmount();
            if ((totalAmountToRefund + refundedAmount) > otherPayment) {
                this.logAmountForGiftCardTransactions(BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL);
            }
        }
        return Boolean.TRUE;
    }

    /**
     * gc scenario
     *
     * @param totalAmountToRefund amount
     */
    private boolean partiallyFullOrderRefundGCScenario(final double totalAmountToRefund) {
        final double refundedAmount = Double.parseDouble(this.totalRefundedAmount.getValue());
        final double otherPayment = this.getOrderModel().getOriginalOrderTotalAmount() - (this.getGiftCardAmount());
        if (refundedAmount < otherPayment) {
            if ((totalAmountToRefund + refundedAmount) > otherPayment) {
                final double refundAmount = (totalAmountToRefund - (otherPayment - refundedAmount));
                this.logAmountForGiftCardTransactions(this.getGiftCardAmount() - (totalAmountToRefund - refundAmount));
            } else {
                return brainTreeTransactionService.createAuthorizationTransactionOfOrder(this.getOrderModel(), BigDecimal.valueOf(
                        otherPayment - (totalAmountToRefund + refundedAmount)).setScale(BlInventoryScanLoggingConstants.TWO,
                        RoundingMode.HALF_EVEN), Boolean.FALSE, null);
            }
        } else {
            this.logAmountForGiftCardTransactions(this.getGiftCardAmount() - totalAmountToRefund);
        }
        return Boolean.TRUE;
    }

    /**
     * method will check if full order entries to checked for cancellation or not
     *
     * @param allSelectedCancelEntries list
     * @return true is not checked
     */
    private boolean isAllEntriesChecked(final Collection<BlOrderEntryToCancelDto> allSelectedCancelEntries) {
        if(allSelectedCancelEntries.size() == this.getOrderModel().getEntries().size()) {
            for (final BlOrderEntryToCancelDto orderEntryToCancelDto : allSelectedCancelEntries) {
                if (orderEntryToCancelDto.getQuantityAvailableToCancel() != orderEntryToCancelDto.getQuantityToCancel()) {
                    return Boolean.FALSE;
                }
            }
            return Boolean.TRUE;
        } else {
            return Boolean.FALSE;
        }
    }

    /**
     * track og gift card transactions
     *
     * @param amountAvailable value
     */
    private void logAmountForGiftCardTransactions(final double amountAvailable) {
        final double finalAvailableAmount = BigDecimal.valueOf((amountAvailable < BlInventoryScanLoggingConstants.ZERO)
            ? -amountAvailable : amountAvailable).setScale(BlInventoryScanLoggingConstants.TWO, RoundingMode.HALF_EVEN).doubleValue();
        final List<String> gcTransactions = new ArrayList<>();
        if(this.getOrderModel().getGiftCardAvailableAmount() > BlInventoryScanLoggingConstants.ZERO) {
            gcTransactions.add(String.valueOf(BigDecimal.valueOf(this.getOrderModel().getGiftCardAvailableAmount() - finalAvailableAmount
            < BlInventoryScanLoggingConstants.ZERO ? -(this.getOrderModel().getGiftCardAvailableAmount() - finalAvailableAmount)
            : (this.getOrderModel().getGiftCardAvailableAmount() - finalAvailableAmount)).setScale(BlInventoryScanLoggingConstants.TWO,
            RoundingMode.HALF_EVEN).doubleValue()));
        } else {
            gcTransactions.add(String.valueOf(this.getOrderModel().getGiftCardAmount() - finalAvailableAmount));
        }
        this.getOrderModel().setGiftCardAvailableAmount(finalAvailableAmount);
        BlLogger.logFormattedMessage(LOGGER, Level.DEBUG, StringUtils.EMPTY, BlCustomCancelRefundConstants.GC_AVAILABLE_AMT,
                finalAvailableAmount, this.getOrderModel().getCode());
        modelService.save(this.getOrderModel());
        modelService.refresh(this.getOrderModel());
    }

    /**
     * This method will start the refund process whether full refund or part refund
     *
     * @param fullRefund   boolean
     * @param captureEntry entry
     */
    private void doRefund(final boolean fullRefund, final PaymentTransactionEntryModel captureEntry) {
        if (Boolean.TRUE.equals(fullRefund)) {
            final double totalAmt = this.getTotalRefundAmount();
            final double refundedAmount = Double.parseDouble(this.totalRefundedAmount.getValue());
            final double otherPayment = this.getOrderModel().getOriginalOrderTotalAmount() - this.getGiftCardAmount();
            if (refundedAmount < otherPayment) {
                this.doFullRefundCalculations(captureEntry, totalAmt, refundedAmount, otherPayment);
            } else {
                this.logAmountForGiftCardTransactions(this.getGiftCardAmount() - totalAmt);

                BlLogger.logFormattedMessage(LOGGER, Level.DEBUG, BlCustomCancelRefundConstants.CANCEL_AND_REFUND_TXN_HAS_BEEN_INITIATED_SUCCESSFULLY,
                        this.getOrderModel().getCode());
                Messagebox.show(BlCustomCancelRefundConstants.ORDER_CANCELLED_AND_REFUND_AMOUNT_HAS_BEEN_INITIATED_SUCCESSFULLY,
                        BlCustomCancelRefundConstants.SUCCESS, Messagebox.OK, Messagebox.INFORMATION);
            }
        } else {
            this.partialRefund(this.cancelAndRefundEntries, captureEntry);
        }
    }

    /**
     * calculations for full refund
     *
     * @param captureEntry entry
     * @param totalAmt amt
     * @param refundedAmount amt
     * @param otherPayment amt
     */
    private void doFullRefundCalculations(final PaymentTransactionEntryModel captureEntry, final double totalAmt,
                                          final double refundedAmount, final double otherPayment) {
        if ((totalAmt + refundedAmount) > otherPayment) {
            final double refundAmount = (totalAmt - (otherPayment - refundedAmount));
            BlLogger.logFormattedMessage(LOGGER, DEBUG, StringUtils.EMPTY, BlCustomCancelRefundConstants.FULL_ORDER_REFUND_AMOUNT_FOR_ORDER,
                    refundAmount, this.getOrderModel().getCode());
            this.doFullRefund(refundAmount, captureEntry);
            if(this.getOrderModel().getGiftCardAmount() > BlInventoryScanLoggingConstants.ZERO) {
                this.logAmountForGiftCardTransactions(this.getGiftCardAmount() - (totalAmt - refundAmount));
            }
        } else {
            this.doFullRefund(totalAmt, captureEntry);
        }
    }

    /**
     * full refund
     *
     * @param totalRefundAmount amount
     */
    private void doFullRefund(final double totalRefundAmount, final PaymentTransactionEntryModel captureEntry) {
        if (totalRefundAmount <= this.getOrderModel().getOriginalOrderTotalAmount()) {
            try {
                final BrainTreeRefundTransactionRequest request = new BrainTreeRefundTransactionRequest(transactionId.getValue());
                request.setAmount(BigDecimal.valueOf(this.deductGiftCartAmount(totalRefundAmount) < BlCustomCancelRefundConstants.ZERO
                        ? -this.deductGiftCartAmount(totalRefundAmount) : this.deductGiftCartAmount(totalRefundAmount))
                        .setScale(BlInventoryScanLoggingConstants.TWO, RoundingMode.HALF_EVEN));
                request.setOrderId(this.getOrderModel().getCode());
                request.setTransactionId(captureEntry.getRequestId());
                this.refund(brainTreePaymentService.refundTransaction(request));
            }  catch (final AdapterException e) {
                BlLogger.logMessage(LOGGER, Level.DEBUG, BlCustomCancelRefundConstants.ORDER_CAN_NOT_BE_CANCEL_AS_FAILED_TO_INITIATE_REFUND,
                        this.getOrderModel().getCode());
                Messagebox.show(BlCustomCancelRefundConstants.ORDER_CAN_NOT_BE_CANCEL_AS_FAILED_TO_INITIATE_REFUND_MSG, BlCustomCancelRefundConstants.FAILURE,
                        Messagebox.OK, Messagebox.ERROR);

            }
        } else {
            Messagebox.show(this.getLabel(BlCustomCancelRefundConstants.INVALID_ORDER_AMOUNT),
                    this.getLabel(BlCustomCancelRefundConstants.EMPTY_AMOUNT_HEADER), Messagebox.OK, Messagebox.ERROR);
        }
    }

    /**
     * Method will execute refund result
     * @param result refundResult
     */
    private void refund(final BrainTreeRefundTransactionResult result) {
        if (result.isSuccess() && this.cancelOrder() != null) {
            this.fullOrderCancelAndLogReturnEntries();
            if(CollectionUtils.isNotEmpty(this.getOrderModel().getPaymentTransactions())) {
                blCustomCancelRefundService.createRefundTransaction(this.getOrderModel().getPaymentTransactions()
                        .get(BlCustomCancelRefundConstants.ZERO), result, PaymentTransactionType.REFUND_STANDALONE, this.getOrderModel());
            }
            BlLogger.logMessage(LOGGER, Level.DEBUG, BlCustomCancelRefundConstants.CANCEL_AND_REFUND_TXN_HAS_BEEN_INITIATED_SUCCESSFULLY,
                    this.getOrderModel().getCode());
            Messagebox.show(BlCustomCancelRefundConstants.ORDER_CANCELLED_AND_REFUND_AMOUNT_HAS_BEEN_INITIATED_SUCCESSFULLY,
                    BlCustomCancelRefundConstants.SUCCESS, Messagebox.OK, Messagebox.INFORMATION);
        } else {
            BlLogger.logMessage(LOGGER, Level.DEBUG, BlCustomCancelRefundConstants.ORDER_CAN_NOT_BE_CANCEL_AS_FAILED_TO_INITIATE_REFUND,
                    this.getOrderModel().getCode());
            Messagebox.show(BlCustomCancelRefundConstants.ORDER_CAN_NOT_BE_CANCEL_AS_FAILED_TO_INITIATE_REFUND_MSG, BlCustomCancelRefundConstants.FAILURE,
                    Messagebox.OK, Messagebox.ERROR);
        }
    }

    /**
     * full cancel and refund logs
     *
     */
    private void fullOrderCancelAndLogReturnEntries() {
        final Map<AbstractOrderEntryModel, Long> returnableEntries = returnService.getAllReturnableEntries(this.getOrderModel());
        if (MapUtils.isNotEmpty(returnableEntries)) {
            final ReturnRequestModel returnRequestModel = returnService.createReturnRequest(this.getOrderModel());
            returnableEntries.forEach((orderEntry, qty) -> {
                final RefundEntryModel refundEntry = returnService.createRefund(returnRequestModel, orderEntry,
                        BlCustomCancelRefundConstants.REFUND_NOTES_WHILE_FULL_REFUND, qty, ReturnAction.IMMEDIATE, RefundReason.WRONGDESCRIPTION);
                refundEntry.setAmount(BigDecimal.valueOf(orderEntry.getTotalPrice()));
                modelService.save(refundEntry);
                returnRequestModel.setSubtotal(returnRequestModel.getReturnEntries().stream().filter(entry ->
                        entry instanceof RefundEntryModel).map(refund -> ((RefundEntryModel) refund).getAmount())
                        .reduce(BigDecimal.ZERO, BigDecimal::add));
                BlLogger.logMessage(LOGGER, Level.DEBUG, BlCustomCancelRefundConstants.CREATE_RETURN_REQUEST_AND_REFUND_ENTRY,
                        this.getOrderModel().getCode());

            });
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
                final double totAmount = blCustomCancelRefundService.getTotalAmountPerEntry(Math.toIntExact(orderEntryToCancelDto
                        .getQuantityToCancel()), (Math.toIntExact(orderEntryToCancelDto.getQuantityAvailableToCancel())),
                        orderEntryModel.getBasePrice(), (orderEntryModel.getAvalaraLineTax() / (Math.toIntExact(orderEntryToCancelDto
                        .getQuantityAvailableToCancel()))), (Boolean.TRUE.equals(orderEntryModel.getGearGuardWaiverSelected())
                        ? orderEntryModel.getGearGuardWaiverPrice() : (Boolean.TRUE.equals(orderEntryModel.getGearGuardProFullWaiverSelected())
                        ? orderEntryModel.getGearGuardProFullWaiverPrice() : BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL)));
                this.doPartRefundCalculation(Math.min(orderEntryToCancelDto.getAmount(), totAmount), captureEntry, orderEntryModel);
            }
        }
    }

    /**
     * This method will do partial refund by taking how much amount need to be refunded
     *
     * @param captureEntry model
     * @param totalAmt     refund amount
     */
    private void doPartRefundCalculation(final double totalAmt, final PaymentTransactionEntryModel captureEntry,
                              final AbstractOrderEntryModel orderEntryModel) {
        final double refundedAmount = Double.parseDouble(this.totalRefundedAmount.getValue());
        final double otherPayment = this.getOrderModel().getOriginalOrderTotalAmount() - this.getGiftCardAmount();
        if (refundedAmount < otherPayment) {
            if ((totalAmt + refundedAmount) > otherPayment) {
                final double refundAmount = (totalAmt - (otherPayment - refundedAmount));
                BlLogger.logFormattedMessage(LOGGER, DEBUG, StringUtils.EMPTY, BlCustomCancelRefundConstants.PART_TOTAL_REFUND_AMOUNT,
                        refundAmount, this.getOrderModel().getCode());
                this.partRefundAndLogResponse(refundAmount, captureEntry, orderEntryModel);
                if(this.getOrderModel().getGiftCardAmount() > BlInventoryScanLoggingConstants.ZERO) {
                    this.logAmountForGiftCardTransactions(this.getGiftCardAmount() - (totalAmt - refundAmount));
                }
            } else {
                this.partRefundAndLogResponse(totalAmt, captureEntry, orderEntryModel);
            }
        } else {
            this.logAmountForGiftCardTransactions(this.getGiftCardAmount() - totalAmt);

            BlLogger.logFormattedMessage(LOGGER, Level.DEBUG, BlCustomCancelRefundConstants.CANCEL_AND_REFUND_TXN_HAS_BEEN_INITIATED_SUCCESSFULLY,
                    this.getOrderModel().getCode());
            Messagebox.show(BlCustomCancelRefundConstants.ORDER_CANCELLED_AND_REFUND_AMOUNT_HAS_BEEN_INITIATED_SUCCESSFULLY,
                    BlCustomCancelRefundConstants.SUCCESS, Messagebox.OK, Messagebox.INFORMATION);
        }
    }

    /**
     * cancel order/entry and return result
     *
     * @return true if cancellation success
     */
    private OrderCancelRecordEntryModel cancelOrder() {
   		try
   		{
   			final OrderCancelRecordEntryModel orderCancelRecordEntryModel = this.getOrderCancelService()
   					.requestOrderCancel(this.buildCancelRequest(), this.getUserService().getCurrentUser());
   			final Set<ConsignmentModel> consignments = this.getOrderModel().getConsignments();
   			if (orderCancelRecordEntryModel != null
   					&& (orderCancelRecordEntryModel.getCancelResult().getCode().equals(BlCustomCancelRefundConstants.FULL)))
   			{
   				getDefaultBlConsignmentService().updateStockForCancelledOrder(consignments);

   				return orderCancelRecordEntryModel;
   			}
   			if (orderCancelRecordEntryModel != null
   					&& orderCancelRecordEntryModel.getCancelResult().getCode().equals(BlCustomCancelRefundConstants.PARTIAL))
   			{
   				getDefaultBlConsignmentService().updateStockForPartialCancelledOrder(consignments, this.cancelAndRefundEntries);
   				return orderCancelRecordEntryModel;
   			}
   		}
   		catch (final OrderCancelException e)
   		{
   			BlLogger.logFormattedMessage(LOGGER, DEBUG, StringUtils.EMPTY,
   					BlCustomCancelRefundConstants.CANCELORDER_CONFIRM_ERROR + StringUtils.SPACE + this.getOrderModel().getCode());
   			Messagebox.show(BlCustomCancelRefundConstants.CANCELORDER_CONFIRM_ERROR, BlCustomCancelRefundConstants.FAILURE,
   					Messagebox.OK, Messagebox.ERROR);
   		}
   		return null;
    }
    

 	/**
     * refund and log response
     *
     *  @param totalAmt amount
     * @param captureEntry entry
     * @param orderEntry orderEntry
     */
    private void partRefundAndLogResponse(final double totalAmt, final PaymentTransactionEntryModel captureEntry,
                                          final AbstractOrderEntryModel orderEntry) {
        try {
            final BrainTreeResponseResultData refundResult = braintreeBackofficePartialRefundFacade.partialRefundTransaction(
                    this.getOrderModel(), captureEntry, BigDecimal.valueOf((totalAmt < BlInventoryScanLoggingConstants.ZERO) ? -totalAmt
                            : totalAmt).setScale(BlInventoryScanLoggingConstants.TWO, RoundingMode.HALF_EVEN));
            if (refundResult.isSuccess() && this.cancelOrder() != null) {
                StringBuilder stringSuccess = new StringBuilder(BlCustomCancelRefundConstants.SUCCESSFULLY_CANCELLED);
                //this.partialCancelAndRefundEntryLog(orderEntry);
                stringSuccess.append(BlCustomCancelRefundConstants.AND_REFUNDED);
                BlLogger.logFormattedMessage(LOGGER, DEBUG, StringUtils.EMPTY, stringSuccess.toString(), this.getOrderModel().getCode());
                Messagebox.show(BlCustomCancelRefundConstants.SUCCESS_CANCEL_REFUND, BlCustomCancelRefundConstants.SUCCESS,
                        Messagebox.OK, Messagebox.INFORMATION);
            }
        } catch (final BraintreeErrorException e) {
            BlLogger.logFormattedMessage(LOGGER, DEBUG, StringUtils.EMPTY, BlCustomCancelRefundConstants.ORDER_CAN_NOT_BE_CANCEL_AS_FAILED_TO_REFUND,
                    this.getOrderModel().getCode());
            Messagebox.show(BlCustomCancelRefundConstants.ORDER_CAN_NOT_BE_CANCEL_AS_FAILED_TO_REFUND_MSG, BlCustomCancelRefundConstants.FAILURE,
                    Messagebox.OK, Messagebox.ERROR);
        }
    }

    /**
     * create refund entry and return request
     *
     * @param orderEntry entry
     */
    private void partialCancelAndRefundEntryLog(final AbstractOrderEntryModel orderEntry) {
        //TODO: This is returning false everytime
        final ReturnRequestModel returnRequestModel = returnService.createReturnRequest(this.getOrderModel());
        final RefundEntryModel refundEntry = returnService.createRefund(returnRequestModel, orderEntry,
                BlCustomCancelRefundConstants.REFUND_NOTES_WHILE_FULL_REFUND, orderEntry.getQuantity(), ReturnAction.IMMEDIATE, RefundReason.WRONGDESCRIPTION);
        refundEntry.setAmount(BigDecimal.valueOf(orderEntry.getTotalPrice()));
        modelService.save(refundEntry);
        returnRequestModel.setSubtotal(returnRequestModel.getReturnEntries().stream().filter(entry ->
                entry instanceof RefundEntryModel).map(refund -> ((RefundEntryModel) refund).getAmount())
                .reduce(BigDecimal.ZERO, BigDecimal::add));
    }

    /**
     * This method will return amount to create gift card.
     *
     * @param refundAmount amount
     * @return amount
     */
    private double deductGiftCartAmount(final double refundAmount) {
        final double amount = this.getOrderModel().getGiftCardAmount() > BlInventoryScanLoggingConstants.ZERO ?
                (refundAmount - this.getGiftCardAmount()) : refundAmount;
        return amount < BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL ? -amount : amount;
    }

    /**
     * this will return gift card amount
     * @return amount
     */
    private double getGiftCardAmount() {
        double gcAmount ;
        if(this.getOrderModel().getGiftCardAvailableAmount() == null) {
            gcAmount = this.getOrderModel().getGiftCardAmount();
        } else {
            gcAmount = (this.getOrderModel().getGiftCardAvailableAmount() > BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL ?
                    this.getOrderModel().getGiftCardAvailableAmount() : this.getOrderModel().getGiftCardAmount());
        }
        return gcAmount;
    }

    /**
     * calculate total refund amount
     *
     * @return
     */
    private double getTotalRefundAmount() {
        final double globalTax = this.globalTaxSelection.isChecked() ? this.getOrderModel().getTotalTax() :
                BlInventoryScanLoggingConstants.ZERO;
        final double globalWaiver = this.globalWaiverSelection.isChecked() ? this.getOrderModel().getTotalDamageWaiverCost() :
                BlInventoryScanLoggingConstants.ZERO;
        final double globalShipping = this.globalShippingSelection.isChecked() ? this.getOrderModel().getDeliveryCost() :
                BlInventoryScanLoggingConstants.ZERO;
        return blCustomCancelRefundService.calculateAmountOnCheckboxStatusFull(this.getOrderModel()
                .getSubtotal(), globalTax, globalWaiver, globalShipping, Double.parseDouble(this.globalTotalRefundAmount.getValue()));
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
            if(Boolean.TRUE.equals(this.validateGlobalSelection())) {
                return Boolean.TRUE;
            }
        } else {
            final Optional<Component> checkedEntry = this.getOrderEntriesGridRows().stream().filter(row -> Boolean.TRUE
                    .equals(((Checkbox) row.getChildren().iterator().next()).isChecked())).findFirst();
            if (!checkedEntry.isPresent()) {
                Messagebox.show(this.getLabel(BlCustomCancelRefundConstants.CANCEL_CONFIRM_MISSING_SELECT_LINE),
                        this.getLabel(BlCustomCancelRefundConstants.CANCEL_CONFIRM_MISSING_SELECT_LINE_SELECTION),
                        Messagebox.OK, Messagebox.ERROR);
                return Boolean.TRUE;
            }
            if(Boolean.TRUE.equals(this.validateEntries())) {
                return Boolean.TRUE;
            }
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
        final double orderTotal = this.getOrderModel().getOriginalOrderTotalAmount();
        //final double refundedAmount = Double.parseDouble(this.totalRefundedAmount.getValue());
        if (amount > orderTotal) {
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
            Messagebox.show(BlCustomCancelRefundConstants.INVALID_ENTERED_AMOUNT_AMOUNT_SHOULD_BE_UP_TO_TWO_DECIMAL_DIGITS_ONLY,
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
        //final double amountAlreadyRefunded = Double.parseDouble(String.valueOf(((InputElement) row.getChildren().get(BlloggingConstants.EIGHT)).getRawValue()));

        final double totalProductPrice = this.getTotalProductPriceForCancelQuantity(row, cancelQty, cancellableQty);
        if (amount <= BlCustomCancelRefundConstants.ZERO) {
            Messagebox.show(this.getLabel(BlCustomCancelRefundConstants.ZERO_ORDER_AMOUNT),
                    this.getLabel(BlCustomCancelRefundConstants.EMPTY_AMOUNT_HEADER), Messagebox.OK, Messagebox.ERROR);
            return Boolean.TRUE;
        } else if (amount > totalProductPrice || (amount + BigDecimal.valueOf(Double.parseDouble(this.totalRefundedAmount.getValue()))
                .setScale(BlInventoryScanLoggingConstants.TWO, RoundingMode.HALF_EVEN).doubleValue()) > this.getOrderModel().getOriginalOrderTotalAmount()) {
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
                        this.inputBoxCustomization(event);
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
     * Input Box populate Order Entry Cost
     * @param event event
     */
    private void inputBoxCustomization(final Event event) {
        if(StringUtils.isNotEmpty(((InputEvent) event).getValue())) {
            ((BlOrderEntryToCancelDto) ((Row) event.getTarget().getParent()).getValue())
                    .setQuantityToCancel(Long.valueOf(((InputEvent) event).getValue()));
            this.populateEntryLevelAmount((Row) event.getTarget().getParent());
        }
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
            final double taxAmount = (tax.isChecked()) ? ((myEntry.getOrderEntry().getAvalaraLineTax()
                    /myEntry.getQuantityAvailableToCancel()) * myEntry.getQuantityToCancel()) : BlInventoryScanLoggingConstants.ZERO;
            final Checkbox waiver = (Checkbox) row.getChildren().get(BlInventoryScanLoggingConstants.SEVEN);
            final double waiverAmount = (myEntry.getOrderEntry().getGearGuardWaiverSelected()
                    ? myEntry.getOrderEntry().getGearGuardWaiverPrice() : (myEntry.getOrderEntry().getGearGuardProFullWaiverSelected() ?
                    myEntry.getOrderEntry().getGearGuardProFullWaiverPrice() : BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL));
            double refundAmount = myEntry.getOrderEntry().getBasePrice() + taxAmount + (waiver.isChecked() ? waiverAmount
                    : BlInventoryScanLoggingConstants.ZERO);
            final double finalAmount = BigDecimal.valueOf(refundAmount * myEntry.getQuantityToCancel()).setScale(BlInventoryScanLoggingConstants.TWO,
                    RoundingMode.HALF_EVEN).doubleValue();
            myEntry.setAmount(finalAmount);
            ((Doublebox) row.getChildren().get(BlInventoryScanLoggingConstants.ELEVEN)).setValue(finalAmount);
        } else {
            ((Doublebox) row.getChildren().get(BlInventoryScanLoggingConstants.ELEVEN)).setValue(BlCustomCancelRefundConstants.ZERO);
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
        this.totalAmount.setValue(formatAmount(order.getOriginalOrderTotalAmount()));
        this.transactionId.setValue(CollectionUtils.isEmpty(order.getPaymentTransactions()) ? StringUtils.EMPTY
                : (order.getPaymentTransactions().get(BlCustomCancelRefundConstants.ZERO) == null ? StringUtils.EMPTY
                : order.getPaymentTransactions().get(BlCustomCancelRefundConstants.ZERO).getRequestId()));
        final double amt = blCustomCancelRefundService.getTotalRefundedAmountOnOrder(order);
        BlLogger.logFormattedMessage(LOGGER, Level.DEBUG, StringUtils.EMPTY, BlCustomCancelRefundConstants.TOT_REFUND_AMT,
                amt, this.getOrderModel().getCode());
        this.totalRefundedAmount.setValue(String.valueOf(amt));
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

	/**
	 * @return the defaultBlConsignmentService
	 */
	public BlConsignmentService getDefaultBlConsignmentService()
	{
		return defaultBlConsignmentService;
	}

	/**
	 * @param defaultBlConsignmentService the defaultBlConsignmentService to set
	 */
	public void setDefaultBlConsignmentService(BlConsignmentService defaultBlConsignmentService)
	{
		this.defaultBlConsignmentService = defaultBlConsignmentService;
	}
}
