package com.bl.backoffice.widget.controller.order;

import static org.apache.log4j.Level.DEBUG;
import static org.apache.log4j.Level.ERROR;

import com.google.common.util.concurrent.AtomicDouble;
import de.hybris.platform.basecommerce.enums.RefundReason;
import de.hybris.platform.basecommerce.enums.ReturnAction;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.enumeration.EnumerationService;
import de.hybris.platform.order.CalculationService;
import de.hybris.platform.ordercancel.OrderCancelEntry;
import de.hybris.platform.ordercancel.OrderCancelService;
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
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import javax.annotation.Resource;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.zkoss.util.Locales;
import org.zkoss.zk.ui.Component;
import org.zkoss.zk.ui.WrongValueException;
import org.zkoss.zk.ui.event.Event;
import org.zkoss.zk.ui.event.InputEvent;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zk.ui.select.annotation.WireVariable;
import org.zkoss.zul.Checkbox;
import org.zkoss.zul.Combobox;
import org.zkoss.zul.Doublebox;
import org.zkoss.zul.Grid;
import org.zkoss.zul.Intbox;
import org.zkoss.zul.ListModelList;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Messagebox.Button;
import org.zkoss.zul.Row;
import org.zkoss.zul.Textbox;
import org.zkoss.zul.impl.InputElement;

import com.bl.backoffice.consignment.service.BlConsignmentService;
import com.bl.constants.BlDeliveryModeLoggingConstants;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.constants.BlloggingConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.esp.service.impl.DefaultBlESPEventService;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.payment.service.BlPaymentService;
import com.bl.core.services.cancelandrefund.service.BlCustomCancelRefundService;
import com.bl.core.services.customer.impl.DefaultBlUserService;
import com.bl.core.services.order.BlOrderService;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import com.braintree.command.request.BrainTreeRefundTransactionRequest;
import com.braintree.command.result.BrainTreeRefundTransactionResult;
import com.braintree.exceptions.BraintreeErrorException;
import com.braintree.facade.backoffice.BraintreeBackofficePartialRefundFacade;
import com.braintree.facade.backoffice.BraintreeBackofficeVoidFacade;
import com.braintree.hybris.data.BrainTreeResponseResultData;
import com.braintree.method.BrainTreePaymentService;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.order.refund.BraintreeRefundService;
import com.braintree.transaction.service.BrainTreeTransactionService;
import com.braintreegateway.Result;
import com.braintreegateway.Transaction;
import com.google.common.collect.Lists;
import com.hybris.backoffice.i18n.BackofficeLocaleService;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.core.events.CockpitEventQueue;
import com.hybris.cockpitng.core.events.impl.DefaultCockpitEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import com.hybris.cockpitng.util.notifications.NotificationService;


/**
 * ##################### Bl-986, Bl-987, Bl-988 ################### This controller is used for cancelling the order by
 * CS agent and refund the amount if payment has been captured.
 *
 * @author Namrata Lohar
 */
public class BlCustomCancelOrderController extends DefaultWidgetController
{
    private static final Logger LOGGER = Logger.getLogger(BlCustomCancelOrderController.class);
    public static final String REFUND = "REFUND";
    protected static final String OUT_CONFIRM = "confirmcancellation";
    protected static final String COMPLETE = "completed";

    @Resource(name = "blOrderService")
    DefaultBlESPEventService blEspEventService;

    private OrderModel orderModel;
    private Map<AbstractOrderEntryModel, Long> orderRefundableEntries;
    private transient Set<BlOrderEntryToCancelDto> orderEntriesToRefund;
    private transient List<BlOrderEntryToCancelDto> refundEntries;
    private transient List<OrderCancelEntry> orderCancelEntries;

    @Wire
    private Textbox customerName; //Information row customerName
    @Wire
    private Textbox globalCancelComment; //Information row globalCancelComment
    @Wire
    private Textbox transactionId; //Information row transactionId
    @Wire
    private Textbox totalLineItemPrice; //Information row totalLineItemPrice
    @Wire
    private Textbox totalShippingCost; //Information row totalShippingCost
    @Wire
    private Textbox totalDamageWaiverCost; //Information row totalDamageWaiverCost
    @Wire
    private Textbox totalTax; //Information row totalTax
    @Wire
    private Textbox totalRefundedAmount; //Information row totalRefundedAmount
    @Wire
    private Textbox totalAmount; //Information row totalAmount
    @Wire
    private Grid orderEntries;
    @Wire
    private Checkbox globalCancelEntriesSelection; //global cancel entry checkbox
    @Wire
    private Checkbox globalShippingSelection; //global shipping checkbox
    @Wire
    private Checkbox globalTaxSelection; //global tax checkbox
    @Wire
    private Checkbox globalWaiverSelection; //global waiver checkbox
    @Wire
    private Textbox globalTotalRefundAmount; //global total refund amount

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

    @Resource(name = "defaultBlUserService")
    private DefaultBlUserService defaultBlUserService;

    @Resource(name = "blOrderService")
    private BlOrderService blOrderService;

    final Map<Integer, Long> integerLongMap  = new HashMap<>();
    final AtomicDouble totalDamageWaiverRefunded   = new AtomicDouble(0.0);

    /**
     * Init cancellation order form.
     *
     * @param inputObject
     *           the input object
     */
    @SocketEvent(socketId = BlCustomCancelRefundConstants.INPUT_OBJECT)
    public void initCancellationOrderForm(final OrderModel inputObject)
    {
        getModelService().refresh(inputObject);
        orderCancelEntries = new ArrayList<>();
        if (inputObject.getOriginalOrderTotalAmount() == BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL)
        {
            if (inputObject.getGrandTotal() > BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL)
            {
                inputObject.setOriginalOrderTotalAmount(inputObject.getGrandTotal());
            }
            else
            {
                inputObject.setOriginalOrderTotalAmount(inputObject.getTotalPrice());
            }
            getModelService().save(inputObject);
            getModelService().refresh(inputObject);
        }
        this.initializePopupRequiredFields(inputObject);
        this.getOrderEntries().setModel(new ListModelList<>(this.orderEntriesToRefund));
        this.getOrderEntries().renderAll();
        this.addListeners();
    }

    /**
     * Reset button event of popup
     */
    @ViewEvent(componentID = BlCustomCancelRefundConstants.UNDO_CANCELLATION, eventName = BlCustomCancelRefundConstants.ON_CLICK)
    public void reset()
    {
        this.globalCancelComment.setValue(StringUtils.EMPTY);
        this.initCancellationOrderForm(this.getOrderModel());
    }

    /**
     * This method is used to close the cancel and refund Popup
     */
    @ViewEvent(componentID = "closePopup", eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
    public void cancelPopup()
    {
        this.sendOutput(OUT_CONFIRM, COMPLETE);
    }

    /**
     * Confirm cancellation.
     */
    @ViewEvent(componentID = BlCustomCancelRefundConstants.CONFIRM_CANCELLATION, eventName = BlCustomCancelRefundConstants.ON_CLICK)
    public void confirmCancellation()
    {
        if (Boolean.FALSE.equals(validateOrderEnteredQuantityAmountReason()))
        {
            Messagebox.show(this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_CONFIRM_MSG),
                    this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_CONFIRM_TITLE) + StringUtils.SPACE
                            + this.getOrderModel().getCode(),
                    new Button[]
                            { Button.NO, Button.YES }, BlCustomCancelRefundConstants.OMS_WIDGET_CANCELORDER_CONFIRM_ICON,
                    this::processCancelAndRefund);
        }
    }

    /**
     * This method will process the confirmation with Yes/No event and perform cancellation and refund on order entries!!
     *
     * @param obj
     *           event
     */
    private void processCancelAndRefund(final Event obj)
    {
        if (Button.YES.event.equals(obj.getName()))
        {
            this.logCancelRefundLogger(BlCustomCancelRefundConstants.REFUNDING_THE_ORDER_FOR_CODE, this.getOrderModel().getCode());
            if (this.buildRefundRequest())
            {
                this.setOrderCancelEntries(Lists.newArrayList());
                this.doCallToRefundProcess();
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
    private void doCallToRefundProcess()
    {
        final Optional<PaymentTransactionEntryModel> captureEntry = blCustomCancelRefundService
                .getCapturedPaymentTransaction(this.getOrderModel());
        if (captureEntry.isPresent() && BooleanUtils.isTrue(this.getOrderModel().getIsCaptured()))
        {
            this.initiateRefund(this.globalCancelEntriesSelection.isChecked(), captureEntry.get());
        }
        else
        {
            final Map<String, String> responseMap = this.getVoidResultStringMap();
            if (Boolean.TRUE.equals(responseMap.containsKey(BlCustomCancelRefundConstants.FAILED)))
            {
                this.logCancelRefundLogger(BlCustomCancelRefundConstants.FAILED_TO_CANCEL_DUE_TO_PAYMENT_GATEWAY_ERROR,
                        this.getOrderModel().getCode());
                this.failureMessageBox(BlCustomCancelRefundConstants.FAILED_TO_CANCEL_DUE_TO_PAYMENT_GATEWAY_ERROR_MSG);
            }
            else
            {
                this.voidAuthorizedPaymentAndRefundGiftCard();
            }
        }
    }

    /**
     * execute void
     *
     * @param allVoidTransactionModels
     *           entries
     * @return responseMap
     */
    private Map<String, String> executeVoidOnTransactions(final Collection<PaymentTransactionEntryModel> allVoidTransactionModels)
    {
        final Map<String, String> responseMap = new HashMap<>();
        if (CollectionUtils.isNotEmpty(allVoidTransactionModels))
        {
            allVoidTransactionModels.forEach(voidTransaction -> {
                final String[] expireTime = voidTransaction.getTransactionStatusDetails()
                        .split(BlCustomCancelRefundConstants.SPLIT_PATTERN);
                if (expireTime.length == BlCoreConstants.TWO_DAYS)
                {
                    this.checkTransaction(responseMap, voidTransaction, expireTime);
                }
            });
        }
        return responseMap;
    }

    /**
     * execute void checking expiry date
     *
     * @param responseMap
     *           map
     * @param voidTransaction
     *           transaction
     * @param expireTime
     *           time
     */
    private void checkTransaction(final Map<String, String> responseMap, final PaymentTransactionEntryModel voidTransaction,
                                  final String[] expireTime)
    {
        try
        {
            if (Boolean.TRUE.equals(new Date().before(new SimpleDateFormat(BlCustomCancelRefundConstants.PARSE_PATTERN)
                    .parse(expireTime[BlCoreConstants.ONE_DAY].trim()))))
            {
                braintreeBackofficeOrderFacade.executeVoid(voidTransaction);
            }
        }
        catch (final BraintreeErrorException e)
        {
            responseMap.put(BlCustomCancelRefundConstants.FAILED, e.getMessage());
        }
        catch (final ParseException e)
        {
            BlLogger.logFormatMessageInfo(LOGGER, ERROR, BlCustomCancelRefundConstants.PARSING_EXCEPTION,
                    expireTime[BlCoreConstants.ONE_DAY].trim());
        }
    }

    /**
     * This method will do void and calculate gift card amount
     */
    private void voidAuthorizedPaymentAndRefundGiftCard()
    {
        final double totalAmountToRefund = this.getTwoDecimalDoubleValue(this.getTotalAmountToRefund());
        this.logCancelRefundLogger(BlCustomCancelRefundConstants.TOTAL_REFUND_AMOUNT_FOR_FULL_ORDER_REFUND_AFTER_PART_REFUND,
                totalAmountToRefund, this.getOrderModel().getCode());
        if (this.globalCancelEntriesSelection.isChecked())
        {
            this.cancelFUllOrderByLoggingGiftCardTransactions(
                    new StringBuilder(BlCustomCancelRefundConstants.SUCCESSFULLY_CANCELLED), totalAmountToRefund);
           // resetDateOfSaleAttributeOnSerial();
        }
        else
        {
            if (Boolean.FALSE.equals(this.isAllEntriesChecked(this.refundEntries)))
            {
                this.partiallyFullOrderRefundGCScenario(totalAmountToRefund);
               // resetDateOfSaleAttributeOnSerial();
            }
            else
            {
                this.refundWithGCIfAny(totalAmountToRefund);
               // resetDateOfSaleAttributeOnSerial();
            }
        }
    }

    /**
     * This method will
     *
     * @param totalAmountToRefund
     *           amt
     */
    private void refundWithGCIfAny(final double totalAmountToRefund)
    {
        if (this.getRemainingGiftCardAmount() > BlCustomCancelRefundConstants.ZERO)
        {
            final double refundedAmount = Double.parseDouble(this.totalRefundedAmount.getValue());
            final double otherPayment = this.getTwoDecimalDoubleValue(
                    this.getOrderModel().getOriginalOrderTotalAmount() - this.getOrderModel().getGiftCardAmount());
            if ((totalAmountToRefund + refundedAmount) > otherPayment)
            {
                final double refundAmount = this
                        .getTwoDecimalDoubleValue(otherPayment > BlCustomCancelRefundConstants.ZERO ? (otherPayment - refundedAmount)
                                : BlCustomCancelRefundConstants.ZERO);
                this.setRefundAmountOnOrder(this.getTwoDecimalDoubleValue(refundAmount));
                this.logAmountForGiftCardTransactions(totalAmountToRefund - refundAmount);
                this.setRefundDetailsOnNonCapturedOrder(
                        (this.globalShippingSelection.isChecked() ? this.getOrderModel().getDeliveryCost()
                                : BlInventoryScanLoggingConstants.ZERO),
                        refundAmount, this.getTwoDecimalDoubleValue((totalAmountToRefund - refundAmount)));
                final StringBuilder resultBuilder = new StringBuilder(BlCustomCancelRefundConstants.SUCCESSFULLY_CANCELLED);
                resultBuilder.append(BlCustomCancelRefundConstants.PLEASE_CREATE_GIFT_CARD_WITH)
                        .append(this.getTwoDecimalDoubleValue(totalAmountToRefund - refundAmount));
                // trigger Esp Refund event for GC
                final AbstractOrderModel order = this.getOrderModel();
                if (order instanceof OrderModel && getDefaultBlUserService().isCsUser())
                {
                    try
                    {
                        final double amount = totalAmountToRefund - refundAmount;
                        BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, "Refund Amount : {}", amount);
                        getBlEspEventService().sendOrderRefundEvent((OrderModel) order, amount, BlCustomCancelRefundConstants.GIFTCARD,
                                getOrderCancelEntries());
                    }
                    catch (final Exception e)
                    {
                        BlLogger.logMessage(LOGGER, Level.ERROR, LogErrorCodeEnum.ESP_EVENT_API_FAILED_ERROR.getCode(),
                                BlCustomCancelRefundConstants.REFUND_EVENT_API_CALL_FAILED, e);
                    }
                }
                this.logCancelRefundLogger(resultBuilder.toString(), this.getOrderModel().getCode());
                this.successMessageBox(String.valueOf(resultBuilder));
            }
        }
        else
        {
            this.logCancelRefundLogger(BlCustomCancelRefundConstants.SUCCESSFULLY_CANCELLED_AND_INITIATED_REFUND_FOR_ORDER,
                    this.getOrderModel().getCode());
            this.successMessageBox(BlCustomCancelRefundConstants.SUCCESS_CANCEL_REFUND);
        }
    }

    /**
     * cancel full order with void any existing auth entry and log gift card transactions if any
     *
     * @param resultBuilder
     *           message string
     * @param totalAmountToRefund
     *           amt
     */
    private void cancelFUllOrderByLoggingGiftCardTransactions(final StringBuilder resultBuilder, final double totalAmountToRefund)
    {
        if (this.getRemainingGiftCardAmount() > BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL)
        {
            final double refundedAmount = Double.parseDouble(this.totalRefundedAmount.getValue());
            final double otherPayment = this.getTwoDecimalDoubleValue(
                    this.getOrderModel().getOriginalOrderTotalAmount() - this.getOrderModel().getGiftCardAmount());
            if (refundedAmount < otherPayment)
            {
                if ((totalAmountToRefund + refundedAmount) > otherPayment)
                {
                    final double refundAmount = this
                            .getTwoDecimalDoubleValue(otherPayment > BlCustomCancelRefundConstants.ZERO ? (otherPayment - refundedAmount)
                                    : BlCustomCancelRefundConstants.ZERO);
                    this.setRefundAmountOnOrder(this.getTwoDecimalDoubleValue(refundAmount));
                    this.logAmountForGiftCardTransactions(totalAmountToRefund - refundAmount);
                    this.setRefundDetailsOnNonCapturedOrder(
                            (this.globalShippingSelection.isChecked() ? this.getOrderModel().getDeliveryCost()
                                    : BlInventoryScanLoggingConstants.ZERO),
                            refundAmount, this.getTwoDecimalDoubleValue((totalAmountToRefund - refundAmount)));
                    // trigger Esp Refund event for GC


                    setRefundedQuantityToOrderEntry();
                    final AbstractOrderModel order = this.getOrderModel();
                    if (order instanceof OrderModel && getDefaultBlUserService().isCsUser())
                    {
                        try
                        {
                            final double amount = totalAmountToRefund - refundAmount;
                            BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, "Refund Amount : {}", amount);
                            getBlEspEventService().sendOrderRefundEvent((OrderModel) order, amount,
                                    BlCustomCancelRefundConstants.GIFTCARD, getOrderCancelEntries());
                        }
                        catch (final Exception e)
                        {
                            BlLogger.logMessage(LOGGER, Level.ERROR, LogErrorCodeEnum.ESP_EVENT_API_FAILED_ERROR.getCode(),
                                    BlCustomCancelRefundConstants.REFUND_EVENT_API_CALL_FAILED, e);
                        }
                    }
                    resultBuilder.append(BlCustomCancelRefundConstants.PLEASE_CREATE_GIFT_CARD_WITH)
                            .append(this.getTwoDecimalDoubleValue(totalAmountToRefund - refundAmount));
                }
            }
            else
            {
                this.logAmountForGiftCardTransactions(totalAmountToRefund);
                this.setRefundDetailsOnOrder((this.globalShippingSelection.isChecked() ? this.getOrderModel().getDeliveryCost()
                        : BlInventoryScanLoggingConstants.ZERO), this.getTwoDecimalDoubleValue(totalAmountToRefund));
                resultBuilder.append(BlCustomCancelRefundConstants.PLEASE_CREATE_GIFT_CARD_WITH).append(totalAmountToRefund);
                setRefundedQuantityToOrderEntry();
                // trigger Esp Refund event for GC
                final AbstractOrderModel order = this.getOrderModel();
                if (order instanceof OrderModel && getDefaultBlUserService().isCsUser())
                {
                    try
                    {
                        BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, "Refund Amount : {}", totalAmountToRefund);
                        getBlEspEventService().sendOrderRefundEvent((OrderModel) order, totalAmountToRefund,
                                BlCustomCancelRefundConstants.GIFTCARD, getOrderCancelEntries());
                        this.setOrderCancelEntries(null);
                    }
                    catch (final Exception e)
                    {
                        BlLogger.logMessage(LOGGER, Level.ERROR, LogErrorCodeEnum.ESP_EVENT_API_FAILED_ERROR.getCode(),
                                BlCustomCancelRefundConstants.REFUND_EVENT_API_CALL_FAILED, e);
                    }
                }
            }
        }
        this.logCancelRefundLogger((resultBuilder.toString() + BlCustomCancelRefundConstants.FOR_ORDER),
                this.getOrderModel().getCode());
        this.successMessageBox(resultBuilder.toString());
    }

    /**
     * this method will return total amount to refund in case of partial
     *
     * @return amount
     */
    private double getTotalAmountToRefund()
    {
        if (this.globalCancelEntriesSelection.isChecked())
        {
            final double tax = this.globalTaxSelection.isChecked() ? this.getOrderModel().getTotalTax()
                    : BlInventoryScanLoggingConstants.ZERO;
            final double waiver = this.globalWaiverSelection.isChecked() ? this.getOrderModel().getTotalDamageWaiverCost()
                    : BlInventoryScanLoggingConstants.ZERO;
            return this.globalShippingSelection.isChecked()
                    ? (this.getOrderModel().getSubtotal() + this.getOrderModel().getDeliveryCost() + tax + waiver)
                    : this.getTotalAmountToRefundForFullOrder();
        }
        else
        {
            return getTotalAmountToRefundForFull();
        }
    }

    /**
     * this method will return total amount to refund in case of partial
     *
     * @return amt
     */
    private double getTotalAmountToRefundForFull()
    {
        double totalAmountToRefund = BlInventoryScanLoggingConstants.ZERO;
        for (final BlOrderEntryToCancelDto orderEntryToCancelDto : this.refundEntries)
        {
            final AbstractOrderEntryModel orderEntryModel = orderEntryToCancelDto.getOrderEntry();
            final double gearGuardProFullWaiverSelected = Boolean.TRUE.equals(orderEntryModel.getGearGuardProFullWaiverSelected())
                    ? orderEntryModel.getGearGuardProFullWaiverPrice()
                    : BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL;
            final double totAmount = blCustomCancelRefundService.getTotalAmountPerEntry(
                    Math.toIntExact(orderEntryToCancelDto.getQuantityToCancel()),
                    (Math.toIntExact(orderEntryToCancelDto.getQuantityAvailableToCancel())), orderEntryModel.getBasePrice(),
                    (orderEntryModel.getAvalaraLineTax() / (Math.toIntExact(orderEntryToCancelDto.getQuantityAvailableToCancel()))),
                    Boolean.TRUE.equals(orderEntryModel.getGearGuardWaiverSelected()) ? orderEntryModel.getGearGuardWaiverPrice()
                            : gearGuardProFullWaiverSelected);
            totalAmountToRefund += Math.min(totAmount, orderEntryToCancelDto.getAmount());
        }
        return totalAmountToRefund;
    }

    /**
     * This method will calculate amt for full order cancel and refund
     *
     * @return amt
     */
    private double getTotalAmountToRefundForFullOrder()
    {
        double orderAmount = BlInventoryScanLoggingConstants.ZERO;
        double tax;
        double waiver = BlInventoryScanLoggingConstants.ZERO;
        double amt = BlInventoryScanLoggingConstants.ZERO;
        for (final BlOrderEntryToCancelDto orderEntryToCancelDto : this.refundEntries)
        {
            final AbstractOrderEntryModel orderEntryModel = orderEntryToCancelDto.getOrderEntry();
            orderAmount += orderEntryModel.getBasePrice() * orderEntryToCancelDto.getQuantityAvailableToCancel();
            tax = this.globalTaxSelection.isChecked() ? orderEntryModel.getAvalaraLineTax() : BlInventoryScanLoggingConstants.ZERO;
            if (this.globalWaiverSelection.isChecked())
            {
                final double gearGuardProFullWaiverSelected = Boolean.TRUE.equals(orderEntryModel.getGearGuardProFullWaiverSelected())
                        ? orderEntryModel.getGearGuardProFullWaiverPrice()
                        : BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL;
                waiver = (Boolean.TRUE.equals(orderEntryModel.getGearGuardWaiverSelected())
                        ? orderEntryModel.getGearGuardWaiverPrice()
                        : gearGuardProFullWaiverSelected) * orderEntryToCancelDto.getQuantityAvailableToCancel();
            }
            amt += orderAmount + tax + waiver;
        }
        return Math.min(Double.parseDouble(this.globalTotalRefundAmount.getValue()), amt);
    }

    /**
     * gc scenario
     *
     * @param totalAmountToRefund
     *           amount
     */
    private boolean partiallyFullOrderRefundGCScenario(final double totalAmountToRefund)
    {
        final double refundedAmount = Double.parseDouble(this.totalRefundedAmount.getValue());
        final double otherPayment = this.getTwoDecimalDoubleValue(
                this.getOrderModel().getOriginalOrderTotalAmount() - this.getOrderModel().getGiftCardAmount());
        if (refundedAmount < otherPayment)
        {
            if ((totalAmountToRefund + refundedAmount) > otherPayment)
            {
                this.partialCancelWithGC(totalAmountToRefund, refundedAmount, otherPayment);
            }
            else
            {
                this.authorizeAndCancelAfterVoid(totalAmountToRefund, refundedAmount, otherPayment);
            }
        }
        else
        {
            this.logAmountForGiftCardTransactions(totalAmountToRefund);
            this.setRefundDetailsOnOrder((this.globalShippingSelection.isChecked() ? this.getOrderModel().getDeliveryCost()
                    : BlInventoryScanLoggingConstants.ZERO), this.getTwoDecimalDoubleValue(totalAmountToRefund));
            this.logCancelRefundLogger(BlCustomCancelRefundConstants.SUCCESS_CANCEL_REFUND_WITH_GC, this.getOrderModel().getCode(),
                    totalAmountToRefund);
            // trigger Esp Refund event for GC

            try {
                orderModel.getEntries().forEach(abstractOrderEntryModel -> {
                    if(MapUtils.isNotEmpty(integerLongMap) && integerLongMap.containsKey(abstractOrderEntryModel.getEntryNumber())) {
                        final OrderEntryModel orderEntryModel = (OrderEntryModel)abstractOrderEntryModel;

                        final Long qtyToRefund = orderModel.getStatus().getCode().equalsIgnoreCase(OrderStatus.CANCELLED.getCode())
                                ? orderEntryModel.getCancelledQuantity() : orderEntryModel.getQuantity();
                        orderEntryModel.setRefundedQuantity(integerLongMap.get(abstractOrderEntryModel.getEntryNumber()) + (null == orderEntryModel.getRefundedQuantity() ? 0L : orderEntryModel.getRefundedQuantity()));
                        if(orderEntryModel.getRefundedQuantity() == qtyToRefund ) {
                            orderEntryModel.setFullyRefunded(Boolean.TRUE);
                        }
                        modelService.save(orderEntryModel);
                        modelService.refresh(orderEntryModel);
                    }
                });
            }
            catch (Exception e) {
                BlLogger.logMessage(LOGGER , Level.ERROR  , "Error while setting the refundable quantity" , e);
            }


            final AbstractOrderModel order = this.getOrderModel();
            if (order instanceof OrderModel && getDefaultBlUserService().isCsUser())
            {
                try
                {
                    BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, "Refund Amount : {}", totalAmountToRefund);
                    getBlEspEventService().sendOrderRefundEvent((OrderModel) order, totalAmountToRefund,
                            BlCustomCancelRefundConstants.GIFTCARD, getOrderCancelEntries());
                }
                catch (final Exception e)
                {
                    BlLogger.logMessage(LOGGER, Level.ERROR, LogErrorCodeEnum.ESP_EVENT_API_FAILED_ERROR.getCode(),
                            BlCustomCancelRefundConstants.REFUND_EVENT_API_CALL_FAILED, e);
                }
            }
            this.successMessageBox(BlCustomCancelRefundConstants.SUCCESSFULLY_CANCELLED
                    + BlCustomCancelRefundConstants.PLEASE_CREATE_GIFT_CARD_WITH + this.getTwoDecimalDoubleValue(totalAmountToRefund));
        }
        return Boolean.TRUE;
    }

    /**
     * This method will do cancel with GC
     *
     * @param totalAmountToRefund
     *           amt
     * @param refundedAmount
     *           amt
     * @param otherPayment
     *           amt
     */
    private void partialCancelWithGC(final double totalAmountToRefund, final double refundedAmount, final double otherPayment)
    {
        final double refundAmount = this.getTwoDecimalDoubleValue(otherPayment - refundedAmount);
        this.setRefundAmountOnOrder(this.getTwoDecimalDoubleValue(refundAmount));
        this.logAmountForGiftCardTransactions(totalAmountToRefund - refundAmount);
        this.setRefundDetailsOnNonCapturedOrder(
                (this.globalShippingSelection.isChecked() ? this.getOrderModel().getDeliveryCost()
                        : BlInventoryScanLoggingConstants.ZERO),
                refundAmount, this.getTwoDecimalDoubleValue((totalAmountToRefund - refundAmount)));
        this.logCancelRefundLogger(BlCustomCancelRefundConstants.SUCCESS_CANCEL_REFUND_WITH_GC, this.getOrderModel().getCode(),
                totalAmountToRefund - refundAmount);

        try {
            orderModel.getEntries().forEach(abstractOrderEntryModel -> {
                if(MapUtils.isNotEmpty(integerLongMap) && integerLongMap.containsKey(abstractOrderEntryModel.getEntryNumber())) {
                    final OrderEntryModel orderEntryModel = (OrderEntryModel)abstractOrderEntryModel;

                    final Long qtyToRefund = orderModel.getStatus().getCode().equalsIgnoreCase(OrderStatus.CANCELLED.getCode())
                            ? orderEntryModel.getCancelledQuantity() : orderEntryModel.getQuantity();
                    orderEntryModel.setRefundedQuantity(integerLongMap.get(abstractOrderEntryModel.getEntryNumber()) + (null == orderEntryModel.getRefundedQuantity() ? 0L : orderEntryModel.getRefundedQuantity()));
                    if(orderEntryModel.getRefundedQuantity() == qtyToRefund ) {
                        orderEntryModel.setFullyRefunded(Boolean.TRUE);
                    }
                    modelService.save(orderEntryModel);
                    modelService.refresh(orderEntryModel);
                }
            });
        }
        catch (Exception e) {
            BlLogger.logMessage(LOGGER , Level.ERROR  , "Error while setting the refundable quantity" , e);
        }


        // trigger Esp Refund event for GC
        final AbstractOrderModel order = this.getOrderModel();
        if (order instanceof OrderModel && getDefaultBlUserService().isCsUser())
        {
            try
            {
                final double amount = totalAmountToRefund - refundAmount;
                BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, "Refund Amount : {}", amount);
                getBlEspEventService().sendOrderRefundEvent((OrderModel) order, amount, BlCustomCancelRefundConstants.GIFTCARD,
                        getOrderCancelEntries());
            }
            catch (final Exception e)
            {
                BlLogger.logMessage(LOGGER, Level.ERROR, LogErrorCodeEnum.ESP_EVENT_API_FAILED_ERROR.getCode(),
                        BlCustomCancelRefundConstants.REFUND_EVENT_API_CALL_FAILED, e);
            }
        }
        this.successMessageBox(
                BlCustomCancelRefundConstants.SUCCESS_CANCEL_REFUND + BlCustomCancelRefundConstants.PLEASE_CREATE_GIFT_CARD_WITH
                        + this.getTwoDecimalDoubleValue(totalAmountToRefund - refundAmount));
    }

    /**
     * This method will do cancel with auth scenario
     *
     * @param totalAmountToRefund
     *           amt
     * @param refundedAmount
     *           amt
     * @param otherPayment
     *           amt
     */
    private void authorizeAndCancelAfterVoid(final double totalAmountToRefund, final double refundedAmount,
                                             final double otherPayment)
    {
        if (brainTreeTransactionService.createAuthorizationTransactionOfOrder(this.getOrderModel(),
                BigDecimal.valueOf(otherPayment - (totalAmountToRefund + refundedAmount))
                        .setScale(BlInventoryScanLoggingConstants.TWO, RoundingMode.HALF_EVEN),
                Boolean.FALSE, null))
        {
            this.setRefundAmountOnOrder(this.getTwoDecimalDoubleValue(totalAmountToRefund));
            this.logCancelRefundLogger(BlCustomCancelRefundConstants.SUCCESSFULLY_CANCELLED
                    + BlCustomCancelRefundConstants.AND_CAPTURED_PAYMENT_WITH_REMAINING_AMOUNT, this.getOrderModel().getCode());
            this.successMessageBox(BlCustomCancelRefundConstants.SUCCESS_CANCEL_REFUND);
        }
        else
        {
            this.logCancelRefundLogger(BlCustomCancelRefundConstants.FAILED_TO_CANCEL_ORDER_AS_ERROR_OCCURRED_DURING_AUTHORIZATION,
                    this.getOrderModel().getCode());
            failureMessageBox(BlCustomCancelRefundConstants.FAILED_TO_CANCEL_ORDER_AS_ERROR_OCCURRED_DURING_AUTHORIZATION_MSG);
        }
    }

    /**
     * method will check if full order entries to checked for cancellation or not
     *
     * @param allSelectedCancelEntries
     *           list
     * @return true is not checked
     */
    private boolean isAllEntriesChecked(final Collection<BlOrderEntryToCancelDto> allSelectedCancelEntries)
    {
        if (allSelectedCancelEntries.size() == this.orderRefundableEntries.size())
        {
            for (final BlOrderEntryToCancelDto orderEntryToCancelDto : allSelectedCancelEntries)
            {
                if (orderEntryToCancelDto.getQuantityAvailableToCancel() != orderEntryToCancelDto.getQuantityToCancel())
                {
                    return Boolean.FALSE;
                }
            }
            return Boolean.TRUE;
        }
        else
        {
            return Boolean.FALSE;
        }
    }

    /**
     * track og gift card transactions
     *
     * @param amount
     *           value
     */
    private void logAmountForGiftCardTransactions(final double amount)
    {
        final OrderModel order = this.getOrderModel();
        final double finalAmount = this.getTwoDecimalDoubleValue(amount);
        final List<String> gcTransactions = new ArrayList<>(order.getGiftCardAmountTransactions());
        gcTransactions
                .add(new SimpleDateFormat(BlDeliveryModeLoggingConstants.RENTAL_DATE_PATTERN).format(Calendar.getInstance().getTime())
                        + BlCoreConstants.DELIMETER + String.valueOf(this.getTwoDecimalDoubleValue(finalAmount)));

        //order.setRefundedGiftCardAmount(Objects.nonNull(order.getRefundedGiftCardAmount()) ? order.getRefundedGiftCardAmount().doubleValue() + amount : amount);
        //order.setGiftCardAvailableAmount(this.getTwoDecimalDoubleValue(this.getGiftCardAmount() - finalAmount));
        order.setGiftCardAmountTransactions(gcTransactions);
        this.setRefundAmountOnOrder(finalAmount);
        this.logCancelRefundLogger(BlCustomCancelRefundConstants.GC_AVAILABLE_AMT, finalAmount, order.getCode());
        getModelService().save(order);
        getModelService().refresh(order);
    }

    /**
     * This method will return void result
     *
     * @return map
     */
    private Map<String, String> getVoidResultStringMap()
    {
        Map<String, String> responseMap = new HashMap<>();
        final Collection<PaymentTransactionEntryModel> allVoidTransactionModels = braintreeBackofficeOrderFacade
                .getVoidableTransactions(this.getOrderModel());
        if (CollectionUtils.isNotEmpty(allVoidTransactionModels))
        {
            responseMap = this.executeVoidOnTransactions(allVoidTransactionModels.stream()
                    .filter(voidEntry -> (voidEntry.getAmount().doubleValue()) > BlInventoryScanLoggingConstants.ONE)
                    .collect(Collectors.toList()));
        }
        return responseMap;
    }

    /**
     * This method will start the refund process whether full refund or part refund
     * @param fullRefund   boolean
     * @param captureEntry entry
     */
    private void initiateRefund(final boolean fullRefund, final PaymentTransactionEntryModel captureEntry) {
        if (Boolean.TRUE.equals(fullRefund)) {
            final double refundedAmount = Double.parseDouble(this.totalRefundedAmount.getValue());
            final double otherPayment = this.getTwoDecimalDoubleValue(this.getOrderModel().getOriginalOrderTotalAmount()
                    - this.getOrderModel().getGiftCardAmount());
            final double totalRefundAmount = this.getTwoDecimalDoubleValue(this.getTotalRefundAmount());
            if (refundedAmount < otherPayment) {
                this.doFullRefundCalculations(captureEntry, totalRefundAmount, refundedAmount, otherPayment);
            } else /*if(null != this.cancelOrder())*/ {
                this.logAmountForGiftCardTransactions(totalRefundAmount);
                this.setRefundDetailsOnOrder((this.globalShippingSelection.isChecked() ? this.getOrderModel().getDeliveryCost() :
                        BlInventoryScanLoggingConstants.ZERO), totalRefundAmount);
                setRefundedQuantityToOrderEntry();
                this.logCancelRefundLogger(BlCustomCancelRefundConstants.SUCCESS_CANCEL_REFUND_WITH_GC, this.getOrderModel().getCode(), totalRefundAmount);
                this.successMessageBox(BlCustomCancelRefundConstants.SUCCESSFULLY_CANCELLED +
                        BlCustomCancelRefundConstants.PLEASE_CREATE_GIFT_CARD_WITH_AMOUNT + this.getTwoDecimalDoubleValue(totalRefundAmount));
                // trigger Esp Refund event for GC
                final AbstractOrderModel order = captureEntry.getPaymentTransaction().getOrder();
                if(order instanceof OrderModel && getDefaultBlUserService().isCsUser()) {
                    try {
                        BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, "Refund Amount : {}",
                                this.getTotalRefundAmount());
                        getBlEspEventService().sendOrderRefundEvent((OrderModel) order,this.getTotalRefundAmount(),BlCustomCancelRefundConstants.GIFTCARD,getOrderCancelEntries());
                        this.setOrderCancelEntries(null);
                    }
                    catch (final Exception e){
                        BlLogger.logMessage(LOGGER, Level.ERROR, LogErrorCodeEnum.ESP_EVENT_API_FAILED_ERROR.getCode(),
                                BlCustomCancelRefundConstants.REFUND_EVENT_API_CALL_FAILED, e);
                    }
                }
            } /*else {
                this.logCancelRefundLogger(BlCustomCancelRefundConstants.FAILED_TO_CANCEL_ORDER_PLEASE_TRY_AGAIN_LATER, this.getOrderModel().getCode());
                this.failureMessageBox(BlCustomCancelRefundConstants.FAILED_TO_CANCEL_ORDER_PLEASE_TRY_AGAIN_LATER_MSG);
            }*/
        } else {
            this.partialRefund(this.refundEntries, captureEntry);
        }
    }



   /* private void initiateRefund(final boolean fullRefund, final PaymentTransactionEntryModel captureEntry)
    {
        if (BooleanUtils.isTrue(fullRefund)) // if order is selected for full refund
        {
            final double refundedAmountOnOrder = Double.parseDouble(this.totalRefundedAmount.getValue());
            if (isGiftCardAppliedOnOrder()) // in case of gift card is applied on order
            {
                // order total excluding applied gift card amount
                final double orderTotalSubtractingGCAmount = this.getTwoDecimalDoubleValue(
                        this.getOrderModel().getOriginalOrderTotalAmount() - this.getOrderModel().getGiftCardAmount());
                if (orderTotalSubtractingGCAmount == 0.0d) // incase if order is paid fully using gift card
                {
                    // if order is selected for full refund then calculate the remaining gift card amount to refund
                    final double gcAmountRemainingToRefund = this.getRemainingGiftCardAmount();
                    this.logAmountForGiftCardTransactions(gcAmountRemainingToRefund);
                }
                else
                {
                    double amountRemainingToRefundFromCCOrPaypal = 0.0d;
                    final double orderAmountExcludingGCAmount = this
                            .getTwoDecimalDoubleValue(this.getOrderModel().getOriginalOrderTotalAmount()
                                    - ObjectUtils.defaultIfNull(this.getOrderModel().getGiftCardAmount(), 0.0d));
                    if (refundedAmountOnOrder <= 0.0d)
                    {
                        amountRemainingToRefundFromCCOrPaypal = orderAmountExcludingGCAmount;
                        //doCall refund logic
                        if (true)//add success from braintree
                        {
                            final double gcAmountRemainingToRefund = this.getRemainingGiftCardAmount();
                            this.logAmountForGiftCardTransactions(gcAmountRemainingToRefund);
                        }
                        else
                        {
                            //add error
                        }
                    }
                    else if (refundedAmountOnOrder <= orderAmountExcludingGCAmount)
                    {
                        amountRemainingToRefundFromCCOrPaypal = orderAmountExcludingGCAmount - refundedAmountOnOrder;
                        //doCall refund logic
                        if (true)//add success from braintree
                        {
                            final double gcAmountRemainingToRefund = this.getRemainingGiftCardAmount();
                            this.logAmountForGiftCardTransactions(gcAmountRemainingToRefund);
                        }
                        else
                        {
                            //add error
                        }
                    }
                    else
                    {
                        final double gcAmountRemainingToRefund = this.getRemainingGiftCardAmount();
                        this.logAmountForGiftCardTransactions(gcAmountRemainingToRefund);
                    }
                }
            }
            else // if no gift card is applied on order
            {

            }

            final double orderAmountExcludingGCAmount = this
                    .getTwoDecimalDoubleValue(this.getOrderModel().getOriginalOrderTotalAmount()
                            - ObjectUtils.defaultIfNull(this.getOrderModel().getGiftCardAmount(), 0.0d));
            final double totalRefundAmount = this.getTwoDecimalDoubleValue(this.getTotalRefundAmount());
            if (refundedAmountOnOrder < orderAmountExcludingGCAmount)
            {
                this.doFullRefundCalculations(captureEntry, totalRefundAmount, refundedAmountOnOrder, orderAmountExcludingGCAmount);
            }
            else
            {
                this.logAmountForGiftCardTransactions(totalRefundAmount);
                this.setRefundDetailsOnOrder((this.globalShippingSelection.isChecked() ? this.getOrderModel().getDeliveryCost()
                        : BlInventoryScanLoggingConstants.ZERO), totalRefundAmount);
                this.logCancelRefundLogger(BlCustomCancelRefundConstants.SUCCESS_CANCEL_REFUND_WITH_GC,
                        this.getOrderModel().getCode(), totalRefundAmount);
                this.successMessageBox(BlCustomCancelRefundConstants.SUCCESSFULLY_CANCELLED
                        + BlCustomCancelRefundConstants.PLEASE_CREATE_GIFT_CARD_WITH_AMOUNT
                        + this.getTwoDecimalDoubleValue(totalRefundAmount));
                // trigger Esp Refund event for GC
                final AbstractOrderModel order = captureEntry.getPaymentTransaction().getOrder();
                if (order instanceof OrderModel && getDefaultBlUserService().isCsUser())
                {
                    try
                    {
                        BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, "Refund Amount : {}", this.getTotalRefundAmount());
                        getBlEspEventService().sendOrderRefundEvent((OrderModel) order, this.getTotalRefundAmount(),
                                BlCustomCancelRefundConstants.GIFTCARD, getOrderCancelEntries());
                        this.setOrderCancelEntries(null);
                    }
                    catch (final Exception e)
                    {
                        BlLogger.logMessage(LOGGER, Level.ERROR, LogErrorCodeEnum.ESP_EVENT_API_FAILED_ERROR.getCode(),
                                BlCustomCancelRefundConstants.REFUND_EVENT_API_CALL_FAILED, e);
                    }
                }
            }
        }
        else
        {
            this.partialRefund(this.refundEntries, captureEntry);
        }
    }
*/
    private boolean isGiftCardAppliedOnOrder()
    {
        final OrderModel orderModel = this.getOrderModel();
        return Objects.nonNull(orderModel) && Objects.nonNull(orderModel.getGiftCardAmount())
                && orderModel.getGiftCardAmount().compareTo(Double.valueOf(0.0d)) >= 1;
    }

    /**
     * calculations for full refund
     *
     * @param captureEntry
     *           entry
     * @param totalAmt
     *           amt
     * @param refundedAmount
     *           amt
     * @param orderAmountExcludingGCAmount
     *           amt
     */
    private void doFullRefundCalculations(final PaymentTransactionEntryModel captureEntry, final double totalAmountToRefund,
                                          final double refundedAmount, final double orderAmountExcludingGCAmount)
    {
        if ((totalAmountToRefund + refundedAmount) > orderAmountExcludingGCAmount)
        {
            final double refundAmount = this.getTwoDecimalDoubleValue(orderAmountExcludingGCAmount - refundedAmount);
            this.logCancelRefundLogger(BlCustomCancelRefundConstants.FULL_ORDER_REFUND_AMOUNT_FOR_ORDER, refundAmount,
                    this.getOrderModel().getCode());
            this.doFullRefund(refundAmount, captureEntry, (totalAmountToRefund - refundAmount));
        }
        else
        {
            this.doFullRefund(totalAmountToRefund, captureEntry, BlCustomCancelRefundConstants.ZERO);
        }
    }

    /**
     * full refund
     *
     * @param totalRefundAmount
     *           amount
     */
    private void doFullRefund(final double totalRefundAmount, final PaymentTransactionEntryModel captureEntry,
                              final double gcAmount)
    {
        if (totalRefundAmount <= this.getOrderModel().getOriginalOrderTotalAmount())
        {
            try
            {
                boolean refundSuccessful = false;
                final BigDecimal amount = (BigDecimal
                        .valueOf(this.deductGiftCartAmount(totalRefundAmount) < BlCustomCancelRefundConstants.ZERO
                                ? -this.deductGiftCartAmount(totalRefundAmount)
                                : this.deductGiftCartAmount(totalRefundAmount))).setScale(BlInventoryScanLoggingConstants.TWO,
                        RoundingMode.HALF_EVEN);
                if (captureEntry.getPaymentTransaction().isLegacyTransaction())
                {
                    final Result<Transaction> result = brainTreeTransactionService.issueBlindCredit(captureEntry, amount);
                    refundSuccessful = result.isSuccess();
                    this.refund(result.getTarget().getAmount(), gcAmount, refundSuccessful);
                }
                else
                {
                    final BrainTreeRefundTransactionRequest request = new BrainTreeRefundTransactionRequest(transactionId.getValue());
                    request.setAmount(amount.setScale(BlInventoryScanLoggingConstants.TWO, RoundingMode.HALF_EVEN));
                    request.setOrderId(this.getOrderModel().getCode());
                    request.setTransactionId(captureEntry.getRequestId());
                    final BrainTreeRefundTransactionResult result = brainTreePaymentService.refundTransaction(request);
                    createTransaction(result);
                    refundSuccessful = result.isSuccess();
                    this.refund(result.getAmount(), gcAmount, refundSuccessful);
                }
            }
            catch (final AdapterException e)
            {
                this.logCancelRefundLogger(BlCustomCancelRefundConstants.ORDER_CAN_NOT_BE_CANCEL_AS_FAILED_TO_INITIATE_REFUND,
                        this.getOrderModel().getCode());
                this.failureMessageBox(BlCustomCancelRefundConstants.ORDER_CAN_NOT_BE_CANCEL_AS_FAILED_TO_INITIATE_REFUND_MSG);
            }
        }
        else
        {
            this.errorMessageBox(this.getLabel(BlCustomCancelRefundConstants.INVALID_ORDER_AMOUNT),
                    this.getLabel(BlCustomCancelRefundConstants.EMPTY_AMOUNT_HEADER));
        }
    }

    /**
     * It creates the refund transaction entry and associates it with the corresponding transaction
     *
     * @param result
     */
    private void createTransaction(final BrainTreeRefundTransactionResult result)
    {
        if (result.isSuccess() && CollectionUtils.isNotEmpty(this.getOrderModel().getPaymentTransactions()))
        {
            this.blCustomCancelRefundService.createRefundTransaction(
                    this.getOrderModel().getPaymentTransactions().get(BlCustomCancelRefundConstants.ZERO), result,
                    PaymentTransactionType.REFUND_STANDALONE, this.getOrderModel());
        }
    }

    /**
     * Method will execute refund result
     *
     * @param amount
     */
    private void refund(final BigDecimal amount, final double gcAmount, final boolean refundSuccessful)
    {
        if (refundSuccessful)
        {
            this.cancelRefundProcess(amount, gcAmount); // Create map .
            //resetDateOfSaleAttributeOnSerial();
        }
        else
        {
            this.logCancelRefundLogger(BlCustomCancelRefundConstants.ORDER_CAN_NOT_BE_CANCEL_AS_FAILED_TO_INITIATE_REFUND,
                    this.getOrderModel().getCode());
            failureMessageBox(BlCustomCancelRefundConstants.ORDER_CAN_NOT_BE_CANCEL_AS_FAILED_TO_INITIATE_REFUND_MSG);
        }
    }

    /**
     * It resets the attribute dateOfSale for returned
     */
    private void resetDateOfSaleAttributeOnSerial()
    {
        this.getCancelAndRefundEntries().forEach(entry -> {
            if (getBlOrderService().isUsedOrderOnly(this.getOrderModel())
                    && entry.getOrderEntry().getProduct() instanceof BlSerialProductModel)
            {
                final BlSerialProductModel serialProductModel = (BlSerialProductModel) entry.getOrderEntry().getProduct();
                serialProductModel.setDateOfSale(null);
                this.getModelService().save(serialProductModel);
            }
        });
    }

    /**
     * This method will complete flow for cancel and refund
     *
     * @param amount
     *           res
     * @param gcAmount
     *           amt
     */
    private void cancelRefundProcess(final BigDecimal amount, final double gcAmount)
    {
        this.setRefundAmountOnOrder(this.getTwoDecimalDoubleValue(amount.doubleValue()));
        this.setRefundDetailsOnOrder((this.globalShippingSelection.isChecked() ? this.getOrderModel().getDeliveryCost()
                : BlInventoryScanLoggingConstants.ZERO), this.getTwoDecimalDoubleValue((amount.doubleValue() + gcAmount)));
        double grandSubTotal = 0.0d;
        final StringBuilder paymentType = new StringBuilder();
        String gcString = StringUtils.EMPTY;
        if (gcAmount > BlInventoryScanLoggingConstants.ZERO)
        {
            grandSubTotal = grandSubTotal + gcAmount;
            gcString = BlCoreConstants.GC_TYPE;
        }

        setRefundedQuantityToOrderEntry();


        // trigger Esp Refund event for  GC or cc/paypal
        if (this.getOrderModel().getPaymentInfo() instanceof BrainTreePaymentInfoModel && getDefaultBlUserService().isCsUser())
        {
            try
            {
                grandSubTotal = grandSubTotal + this.getTwoDecimalDoubleValue(amount.doubleValue());
                BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, "Refund Amount : {}", grandSubTotal);
                final BrainTreePaymentInfoModel brainTreePaymentInfoModel = (BrainTreePaymentInfoModel) orderModel.getPaymentInfo();
                final String paymentMethodType = StringUtils.equalsIgnoreCase(BlCoreConstants.PAY_PAL_PROVIDER,
                        brainTreePaymentInfoModel.getPaymentProvider()) ? BlCoreConstants.PAY_PAL
                        : ((BrainTreePaymentInfoModel) this.getOrderModel().getPaymentInfo()).getPaymentProvider();
                getBlEspEventService().sendOrderRefundEvent(this.getOrderModel(), grandSubTotal, paymentMethodType,
                        getOrderCancelEntries());
            }
            catch (final Exception e)
            {
                BlLogger.logMessage(LOGGER, Level.ERROR, LogErrorCodeEnum.ESP_EVENT_API_FAILED_ERROR.getCode(),
                        BlCustomCancelRefundConstants.REFUND_EVENT_API_CALL_FAILED, e);
            }
        }
        if (gcAmount > BlInventoryScanLoggingConstants.ZERO)
        {
            this.logAmountForGiftCardTransactions(gcAmount);
            this.logCancelRefundLogger(BlCustomCancelRefundConstants.SUCCESS_CANCEL_REFUND_WITH_GC, this.getOrderModel().getCode(),
                    gcAmount);
            this.successMessageBox(BlCustomCancelRefundConstants.SUCCESSFULLY_CANCELLED_AND_INITIATED_REFUND_FOR_ORDER
                    + this.getOrderModel().getCode() + BlCustomCancelRefundConstants.PLEASE_CREATE_GIFT_CARD_WITH_AMOUNT
                    + this.getTwoDecimalDoubleValue(gcAmount));
        }
        else
        {
            this.logCancelRefundLogger(BlCustomCancelRefundConstants.CANCEL_AND_REFUND_TXN_HAS_BEEN_INITIATED_SUCCESSFULLY,
                    this.getOrderModel().getCode());
            this.successMessageBox(BlCustomCancelRefundConstants.ORDER_CANCELLED_AND_REFUND_AMOUNT_HAS_BEEN_INITIATED_SUCCESSFULLY);
        }
    }

    /**
     * This method created to set the refunded quantity to entry model
     */
    private void setRefundedQuantityToOrderEntry() {
        try {
            final OrderModel orderModel = this.orderModel;
            orderModel.getEntries().forEach(abstractOrderEntryModel -> {
                final OrderEntryModel orderEntryModel = (OrderEntryModel) abstractOrderEntryModel;
                orderEntryModel.setFullyRefunded(Boolean.TRUE);
                final Long qtyToRefund = orderModel.getStatus().getCode().equalsIgnoreCase(OrderStatus.CANCELLED.getCode())
                        ? orderEntryModel.getCancelledQuantity() : orderEntryModel.getQuantity();
                final long remainingQty = qtyToRefund - (null == orderEntryModel.getRefundedQuantity() ? 0L : orderEntryModel.getRefundedQuantity());
                orderEntryModel.setRefundedQuantity(remainingQty + (null == orderEntryModel.getRefundedQuantity() ? 0L : orderEntryModel.getRefundedQuantity()));
                modelService.save(orderEntryModel);
                modelService.refresh(orderEntryModel);
            });
        }
        catch (Exception e) {
            BlLogger.logMessage(LOGGER , ERROR , "Error While set refund quantity to entry" , e);
        }

    }

    /**
     * This method return Message If Gc Applied
     *
     * @param object
     */
    String getMessageIfGcApplied(final String object)
    {
        if (StringUtils.isNotBlank(object))
        {
            final StringBuilder paymentType = new StringBuilder();
            return paymentType.append(StringUtils.SPACE).append("+").append(StringUtils.SPACE).append(object).toString();
        }
        return StringUtils.EMPTY;
    }

    /**
     * full cancel and refund logs
     */
    private void fullOrderCancelAndLogReturnEntries()
    {
        final Map<AbstractOrderEntryModel, Long> returnableEntries = returnService.getAllReturnableEntries(this.getOrderModel());
        if (MapUtils.isNotEmpty(returnableEntries))
        {
            final ReturnRequestModel returnRequestModel = returnService.createReturnRequest(this.getOrderModel());
            returnableEntries.forEach((orderEntry, qty) -> {
                final RefundEntryModel refundEntry = returnService.createRefund(returnRequestModel, orderEntry,
                        BlCustomCancelRefundConstants.REFUND_NOTES_WHILE_FULL_REFUND, qty, ReturnAction.IMMEDIATE,
                        RefundReason.WRONGDESCRIPTION);
                refundEntry.setAmount(BigDecimal.valueOf(orderEntry.getTotalPrice()));
                getModelService().save(refundEntry);
                returnRequestModel
                        .setSubtotal(returnRequestModel.getReturnEntries().stream().filter(entry -> entry instanceof RefundEntryModel)
                                .map(refund -> ((RefundEntryModel) refund).getAmount()).reduce(BigDecimal.ZERO, BigDecimal::add));
                this.logCancelRefundLogger(BlCustomCancelRefundConstants.CREATE_RETURN_REQUEST_AND_REFUND_ENTRY,
                        this.getOrderModel().getCode());
            });
        }
    }

    /**
     * This method will do partial refund by taking how much amount need to be refunded
     *
     * @param orderEntryToCancelDtos
     *           pojo
     * @param captureEntry
     *           entry
     */
    private void partialRefund(final Collection<BlOrderEntryToCancelDto> orderEntryToCancelDtos,
                               final PaymentTransactionEntryModel captureEntry)
    {
        if (CollectionUtils.isNotEmpty(orderEntryToCancelDtos))
        {
            double totAmount = BlCustomCancelRefundConstants.ZERO;
            double enteredAmount = BlCustomCancelRefundConstants.ZERO;
            for (final BlOrderEntryToCancelDto orderEntryToCancelDto : orderEntryToCancelDtos)
            {
                final AbstractOrderEntryModel orderEntryModel = orderEntryToCancelDto.getOrderEntry();
                integerLongMap.put(orderEntryModel.getEntryNumber() , orderEntryToCancelDto.getQuantityToCancel());
                final double waiver = getDamageWaiverPriceFromEntry(orderEntryModel);
                totalDamageWaiverRefunded.getAndAdd(waiver);
                totAmount += blCustomCancelRefundService.getTotalAmountPerEntry(
                        Math.toIntExact(orderEntryToCancelDto.getQuantityToCancel()),
                        (Math.toIntExact(orderEntryToCancelDto.getQuantityAvailableToCancel())), orderEntryModel.getBasePrice(),
                        (orderEntryModel.getAvalaraLineTax() / (Math.toIntExact(orderEntryToCancelDto.getQuantityAvailableToCancel()))),
                        waiver);
                enteredAmount += orderEntryToCancelDto.getAmount();
            }
            this.doPartRefundCalculation(Math.min(enteredAmount, totAmount), captureEntry);
        }
    }

    /**
     * This method is to get the damage waiver price  from order  entry model
     * @param abstractOrderEntryModel AbstractOrderEntryModel
     * @return values to set on request
     */
    protected Double getDamageWaiverPriceFromEntry(final AbstractOrderEntryModel abstractOrderEntryModel) {
        final AtomicDouble damageWaiverPrice = new AtomicDouble(0.0);
        if(org.apache.commons.lang3.BooleanUtils.isTrue(abstractOrderEntryModel.getGearGuardWaiverSelected())) {
            if(abstractOrderEntryModel.getGearGuardWaiverPrice()!=null) {
                damageWaiverPrice.set(abstractOrderEntryModel.getGearGuardWaiverPrice());
            }
        }
        else if(org.apache.commons.lang3.BooleanUtils.isTrue(abstractOrderEntryModel.getGearGuardProFullWaiverSelected())){
            if(abstractOrderEntryModel.getGearGuardWaiverPrice()!=null) {
                damageWaiverPrice.set(abstractOrderEntryModel.getGearGuardWaiverPrice());
            }
        }
        else if(org.apache.commons.lang3.BooleanUtils.isTrue(abstractOrderEntryModel.getNoDamageWaiverSelected())){
            damageWaiverPrice.set(0.0);
        }
        return damageWaiverPrice.get();
    }




    /**
     * This method will do partial refund by taking how much amount need to be refunded
     *
     * @param captureEntry
     *           model
     * @param totalAmt
     *           refund amount
     */
    private void doPartRefundCalculation(final double totalAmt, final PaymentTransactionEntryModel captureEntry)
    {
        final double refundedAmount = Double.parseDouble(this.totalRefundedAmount.getValue());
        final double otherPayment = this.getTwoDecimalDoubleValue(
                this.getOrderModel().getOriginalOrderTotalAmount() - this.getOrderModel().getGiftCardAmount());
        if (refundedAmount < otherPayment)
        {
            if ((totalAmt + refundedAmount) > otherPayment)
            {
                final double refundAmount = this.getTwoDecimalDoubleValue(otherPayment - refundedAmount);
                this.logCancelRefundLogger(BlCustomCancelRefundConstants.PART_TOTAL_REFUND_AMOUNT, refundAmount,
                        this.getOrderModel().getCode());
                this.partRefundAndLogResponse(refundAmount, captureEntry, (totalAmt - refundAmount));
            }
            else
            {
                this.partRefundAndLogResponse(totalAmt, captureEntry, BlCustomCancelRefundConstants.ZERO);
            }
        }
        else
        {
            this.logAmountForGiftCardTransactions(totalAmt);
            this.setRefundDetailsOnOrder((this.globalShippingSelection.isChecked() ? this.getOrderModel().getDeliveryCost()
                    : BlInventoryScanLoggingConstants.ZERO), this.getTwoDecimalDoubleValue((totalAmt)));
            try {
                orderModel.getEntries().forEach(abstractOrderEntryModel -> {
                    if(MapUtils.isNotEmpty(integerLongMap) && integerLongMap.containsKey(abstractOrderEntryModel.getEntryNumber())) {
                        final OrderEntryModel orderEntryModel = (OrderEntryModel)abstractOrderEntryModel;

                        final Long qtyToRefund = orderModel.getStatus().getCode().equalsIgnoreCase(OrderStatus.CANCELLED.getCode())
                                ? orderEntryModel.getCancelledQuantity() : orderEntryModel.getQuantity();
                        orderEntryModel.setRefundedQuantity(integerLongMap.get(abstractOrderEntryModel.getEntryNumber()) + (null == orderEntryModel.getRefundedQuantity() ? 0L : orderEntryModel.getRefundedQuantity()));
                        if(orderEntryModel.getRefundedQuantity() == qtyToRefund ) {
                            orderEntryModel.setFullyRefunded(Boolean.TRUE);
                        }
                        modelService.save(orderEntryModel);
                        modelService.refresh(orderEntryModel);
                    }
                });
            }
            catch (Exception e) {
                BlLogger.logMessage(LOGGER , Level.ERROR  , "Error while setting the refundable quantity" , e);
            }

            this.logCancelRefundLogger(BlCustomCancelRefundConstants.SUCCESS_CANCEL_REFUND_WITH_GC, this.getOrderModel().getCode(),
                    totalAmt);
            this.successMessageBox(BlCustomCancelRefundConstants.SUCCESSFULLY_CANCELLED_AND_INITIATED_REFUND_FOR_ORDER
                    + this.getOrderModel().getCode() + BlCustomCancelRefundConstants.PLEASE_CREATE_GIFT_CARD_WITH_AMOUNT
                    + this.getTwoDecimalDoubleValue(totalAmt));
            // trigger Esp Refund event for GC
            final AbstractOrderModel order = captureEntry.getPaymentTransaction().getOrder();
            if (order instanceof OrderModel && getDefaultBlUserService().isCsUser())
            {
                try
                {
                    BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, "Refund Amount : {}", totalAmt);
                    getBlEspEventService().sendOrderRefundEvent((OrderModel) order, totalAmt, BlCustomCancelRefundConstants.GIFTCARD,
                            getOrderCancelEntries());
                }
                catch (final Exception e)
                {
                    BlLogger.logMessage(LOGGER, Level.ERROR, LogErrorCodeEnum.ESP_EVENT_API_FAILED_ERROR.getCode(),
                            BlCustomCancelRefundConstants.REFUND_EVENT_API_CALL_FAILED, e);
                }
            }
        }
    }

    /**
     * refund and log response
     *
     * @param totalAmt
     *           amount
     * @param captureEntry
     *           entry
     */
    private void partRefundAndLogResponse(final double totalAmt, final PaymentTransactionEntryModel captureEntry,
                                          final double gcAmount)
    {
        try
        {
            boolean refundSuccessful = false;
            final BigDecimal amount = BigDecimal.valueOf((totalAmt < BlInventoryScanLoggingConstants.ZERO) ? -totalAmt : totalAmt)
                    .setScale(BlInventoryScanLoggingConstants.TWO, RoundingMode.HALF_EVEN);
            if (captureEntry.getPaymentTransaction().isLegacyTransaction())
            {
                final Result<Transaction> result = brainTreeTransactionService.issueBlindCredit(captureEntry, amount);
                refundSuccessful = result.isSuccess();
            }
            else
            {
                final BrainTreeResponseResultData refundResult = braintreeBackofficePartialRefundFacade
                        .partialRefundTransaction(this.getOrderModel(), captureEntry, amount);
                refundSuccessful = refundResult.isSuccess();
            }
            if (refundSuccessful)
            {
                this.cancelPartialRefund(totalAmt, gcAmount);
                //resetDateOfSaleAttributeOnSerial();
            }
            else
            {
                this.logCancelRefundLogger(BlCustomCancelRefundConstants.ORDER_CAN_NOT_BE_CANCEL_AS_FAILED_TO_REFUND,
                        this.getOrderModel().getCode());
                this.failureMessageBox(BlCustomCancelRefundConstants.ORDER_CAN_NOT_BE_CANCEL_AS_FAILED_TO_REFUND_MSG);
            }
        }
        catch (final BraintreeErrorException e)
        {
            this.logCancelRefundLogger(BlCustomCancelRefundConstants.ORDER_CAN_NOT_BE_CANCEL_AS_FAILED_TO_REFUND,
                    this.getOrderModel().getCode());
            this.failureMessageBox(BlCustomCancelRefundConstants.ORDER_CAN_NOT_BE_CANCEL_AS_FAILED_TO_REFUND_MSG);
        }
    }

    /**
     * This method will cancel and complete refund for partial
     *
     * @param totalAmt
     *           amt
     * @param gcAmount
     *           amt
     */
    private void cancelPartialRefund(final double totalAmt, final double gcAmount)
    {

        final StringBuilder stringSuccess = new StringBuilder(BlCustomCancelRefundConstants.SUCCESS_CANCEL_REFUND);
        double grandSubTotal = 0.0d;
        final StringBuilder paymentType = new StringBuilder();
        String gcString = StringUtils.EMPTY;
        this.setRefundAmountOnOrder(this.getTwoDecimalDoubleValue(totalAmt));
        this.setRefundDetailsOnOrder((this.globalShippingSelection.isChecked() ? this.getOrderModel().getDeliveryCost()
                : BlInventoryScanLoggingConstants.ZERO), this.getTwoDecimalDoubleValue((totalAmt + gcAmount)));
        if (gcAmount > BlInventoryScanLoggingConstants.ZERO)
        {
            grandSubTotal = grandSubTotal + gcAmount;
            gcString = BlCoreConstants.GC_TYPE;
            this.logAmountForGiftCardTransactions(gcAmount);
            stringSuccess.append(BlCustomCancelRefundConstants.PLEASE_CREATE_GIFT_CARD_WITH)
                    .append(this.getTwoDecimalDoubleValue(gcAmount));
            this.successMessageBox(stringSuccess.toString());
        }
        this.logCancelRefundLogger(BlCustomCancelRefundConstants.SUCCESSFULLY_CANCELLED_AND_INITIATED_REFUND_FOR_ORDER,
                this.getOrderModel().getCode());
        this.successMessageBox(stringSuccess.toString());
        grandSubTotal = grandSubTotal + totalAmt;

        try {
            orderModel.getEntries().forEach(abstractOrderEntryModel -> {
                if(MapUtils.isNotEmpty(integerLongMap) && integerLongMap.containsKey(abstractOrderEntryModel.getEntryNumber())) {
                    final OrderEntryModel orderEntryModel = (OrderEntryModel)abstractOrderEntryModel;
                     Long qtyToRefund = orderModel.getStatus().getCode().equalsIgnoreCase(OrderStatus.CANCELLED.getCode())
                            ? orderEntryModel.getCancelledQuantity() : orderEntryModel.getQuantity();
                    orderEntryModel.setRefundedQuantity(integerLongMap.get(abstractOrderEntryModel.getEntryNumber()) + (null == orderEntryModel.getRefundedQuantity() ? 0L : orderEntryModel.getRefundedQuantity()));
                    if(orderEntryModel.getRefundedQuantity() == qtyToRefund ) {
                        orderEntryModel.setFullyRefunded(Boolean.TRUE);
                    }
                    modelService.save(orderEntryModel);
                    modelService.refresh(orderEntryModel);
                }
            });
        }
        catch (Exception e) {
            BlLogger.logMessage(LOGGER , Level.ERROR  , "Error while setting the refundable quantity" , e);
        }

        // trigger Esp Refund event for  GC or cc/paypal
        if (this.getOrderModel().getPaymentInfo() instanceof BrainTreePaymentInfoModel && getDefaultBlUserService().isCsUser())
        {
            try
            {
                BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, "Refund Amount : {}", grandSubTotal);
                final BrainTreePaymentInfoModel brainTreePaymentInfoModel = (BrainTreePaymentInfoModel) orderModel.getPaymentInfo();
                /*
                 * final String paymentMethodType=
                 * StringUtils.equalsIgnoreCase(BlCoreConstants.PAY_PAL_PROVIDER,brainTreePaymentInfoModel.
                 * getPaymentProvider()) ? BlCoreConstants.PAY_PAL
                 * :paymentType.append(((BrainTreePaymentInfoModel)this.getOrderModel().getPaymentInfo()).
                 * getPaymentProvider()).append(getMessageIfGcApplied(gcString)).toString();
                 */
                final String paymentMethodType = StringUtils.equalsIgnoreCase(BlCoreConstants.PAY_PAL_PROVIDER,
                        brainTreePaymentInfoModel.getPaymentProvider()) ? BlCoreConstants.PAY_PAL
                        : ((BrainTreePaymentInfoModel) this.getOrderModel().getPaymentInfo()).getPaymentProvider();
                getBlEspEventService().sendOrderRefundEvent(this.getOrderModel(), grandSubTotal, paymentMethodType,
                        getOrderCancelEntries());
            }
            catch (final Exception e)
            {
                BlLogger.logMessage(LOGGER, Level.ERROR, LogErrorCodeEnum.ESP_EVENT_API_FAILED_ERROR.getCode(),
                        BlCustomCancelRefundConstants.REFUND_EVENT_API_CALL_FAILED, e);
            }

        }
    }

    /**
     * create refund entry and return request
     *
     * @param orderEntry
     *           entry
     */
    private void partialCancelAndRefundEntryLog(final AbstractOrderEntryModel orderEntry)
    {
        final ReturnRequestModel returnRequestModel = returnService.createReturnRequest(this.getOrderModel());
        final RefundEntryModel refundEntry = returnService.createRefund(returnRequestModel, orderEntry,
                BlCustomCancelRefundConstants.REFUND_NOTES_WHILE_FULL_REFUND, orderEntry.getQuantity(), ReturnAction.IMMEDIATE,
                RefundReason.WRONGDESCRIPTION);
        refundEntry.setAmount(BigDecimal.valueOf(orderEntry.getTotalPrice()));
        getModelService().save(refundEntry);
        getModelService().refresh(refundEntry);
        returnRequestModel
                .setSubtotal(returnRequestModel.getReturnEntries().stream().filter(entry -> entry instanceof RefundEntryModel)
                        .map(refund -> ((RefundEntryModel) refund).getAmount()).reduce(BigDecimal.ZERO, BigDecimal::add));
    }

    /**
     * This method will return amount to create gift card.
     *
     * @param refundAmount
     *           amount
     * @return amount
     */
    private double deductGiftCartAmount(final double refundAmount)
    {
        final double amount = this.getOrderModel().getGiftCardAmount() > BlInventoryScanLoggingConstants.ZERO
                ? (refundAmount - this.getRemainingGiftCardAmount())
                : refundAmount;
        return amount < BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL ? -amount : amount;
    }

    /**
     * this will return gift card amount
     *
     * @return amount
     */
    private double getRemainingGiftCardAmount()
    {
        double gcAmount;
        final OrderModel orderModel = this.getOrderModel();
        //		return Objects.nonNull(orderModel.getRefundedGiftCardAmount())
        //				&& orderModel.getRefundedGiftCardAmount().compareTo(Double.valueOf(0.0d)) >= 1
        //						? orderModel.getGiftCardAmount().doubleValue() - orderModel.getRefundedGiftCardAmount().doubleValue()
        //						: orderModel.getGiftCardAmount().doubleValue();
        if (this.getOrderModel().getGiftCardAvailableAmount() == null)
        {
            gcAmount = this.getOrderModel().getGiftCardAmount();
        }
        else
        {
            gcAmount = (this.getOrderModel().getGiftCardAvailableAmount() > BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL
                    ? this.getOrderModel().getGiftCardAvailableAmount()
                    : this.getOrderModel().getGiftCardAmount());
        }
        return gcAmount;
    }

    /**
     * calculate total refund amount
     *
     * @return amt
     */
    private double getTotalRefundAmount()
    {
        final double orderAmount = BlCustomCancelRefundConstants.ZERO;
        final double globalTax = BlInventoryScanLoggingConstants.ZERO;
        final double globalWaiver = BlInventoryScanLoggingConstants.ZERO;
        final double globalShipping = BlInventoryScanLoggingConstants.ZERO;
        if (Double.parseDouble(this.totalRefundedAmount.getValue()) <= BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL)
        {
            return blCustomCancelRefundService.calculateAmountOnCheckboxStatusFull(this.getOrderModel().getSubtotal(),
                    (this.globalTaxSelection.isChecked() ? this.getOrderModel().getTotalTax() : BlInventoryScanLoggingConstants.ZERO),
                    (this.globalWaiverSelection.isChecked() ? this.getOrderModel().getTotalDamageWaiverCost()
                            : BlInventoryScanLoggingConstants.ZERO),
                    (this.globalShippingSelection.isChecked() ? this.getOrderModel().getDeliveryCost()
                            : BlInventoryScanLoggingConstants.ZERO),
                    Double.parseDouble(this.globalTotalRefundAmount.getValue()));
        }
        else
        {
            return this.calculateAmount(orderAmount, globalTax, globalWaiver, globalShipping);
        }
    }

    /**
     * calculate total refund amount
     *
     * @param shipping
     *           cost
     * @param ccAmt
     *           amt
     * @param gcAmt
     *           amt
     */
    private void setRefundDetailsOnNonCapturedOrder(final double shipping, final double ccAmt, final double gcAmt)
    {
        final OrderModel order = this.getOrderModel();
        final double taxValue = this.getTaxAmount(BlCustomCancelRefundConstants.ZERO);
        final double totTax = this.getTwoDecimalDoubleValue(shipping + taxValue);
        if (totTax <= ccAmt)
        {
            order.setRefundShippingTotalAmount(
                    order.getRefundShippingTotalAmount() == null ? BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL
                            : this.getTwoDecimalDoubleValue(
                            (order.getRefundShippingTotalAmount() + BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL)));
            order.setRefundTaxTotalAmount(order.getRefundTaxTotalAmount() == null ? BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL
                    : this.getTwoDecimalDoubleValue(
                    (order.getRefundTaxTotalAmount() + BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL)));
        }
        else
        {
            order.setRefundShippingTotalAmount(order.getRefundShippingTotalAmount() == null ? shipping
                    : this.getTwoDecimalDoubleValue((order.getRefundShippingTotalAmount() + shipping)));
            BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, "Adding Shipping Amount : {}", shipping);
            order.setRefundTaxTotalAmount(order.getRefundTaxTotalAmount() == null ? taxValue
                    : this.getTwoDecimalDoubleValue((order.getRefundTaxTotalAmount() + taxValue)));
            BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, "Adding Tax Amount : {}", taxValue);
        }
        order.setRefundTotalAmount(
                order.getRefundTotalAmount() == null ? gcAmt : this.getTwoDecimalDoubleValue((order.getRefundTotalAmount() + gcAmt)));
        BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, "Adding Refund Amount : {}", gcAmt);
        getModelService().save(order);
        getModelService().refresh(order);
    }

    /**
     * calculate total refund amount
     *
     * @param shipping
     *           cost
     * @param amt
     *           amt
     */
    private void setRefundDetailsOnOrder(final double shipping, final double amt)
    {
        final OrderModel order = this.getOrderModel();
        final double taxValue = this.getTaxAmount(BlCustomCancelRefundConstants.ZERO);
        order.setRefundTotalAmount(
                order.getRefundTotalAmount() == null ? amt : this.getTwoDecimalDoubleValue((order.getRefundTotalAmount() + amt)));
        BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, "Adding Refund Amount : {}", amt);
        order.setRefundShippingTotalAmount(order.getRefundShippingTotalAmount() == null ? shipping
                : this.getTwoDecimalDoubleValue((order.getRefundShippingTotalAmount() + shipping)));
        BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, "Adding Shipping Amount : {}", shipping);
        order.setRefundTaxTotalAmount(order.getRefundTaxTotalAmount() == null ? taxValue
                : this.getTwoDecimalDoubleValue((order.getRefundTaxTotalAmount() + taxValue)));
        BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, "Adding Tax Amount : {}", taxValue);
        final Double totalDamageWaiverRefundedAmt = this.globalCancelEntriesSelection.isChecked() && this.globalWaiverSelection.isChecked()
                ? order.getTotalDamageWaiverCost() : this.totalDamageWaiverRefunded.get();
        order.setRefundTotalDamageWaiverAmount(Objects.nonNull(order.getRefundTotalDamageWaiverAmount())
                ? order.getRefundTotalDamageWaiverAmount() + totalDamageWaiverRefundedAmt : totalDamageWaiverRefundedAmt);
        BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, "Adding Total Damage Waiver Amount Refunded : {}", totalDamageWaiverRefundedAmt);
        this.totalDamageWaiverRefunded.set(0.0);
        getModelService().save(order);
        getModelService().refresh(order);
    }

    /**
     * This method will calculate tax amount
     *
     * @param taxValue
     *           tax
     * @return double
     */
    private double getTaxAmount(double taxValue)
    {
        if (Boolean.TRUE.equals(this.globalCancelEntriesSelection.isChecked()))
        {
            if (Double.parseDouble(this.totalRefundedAmount.getValue()) <= BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL)
            {
                taxValue = this.globalTaxSelection.isChecked() ? this.getOrderModel().getTotalTax()
                        : BlInventoryScanLoggingConstants.ZERO;
            }
            else
            {
                for (final BlOrderEntryToCancelDto orderEntryToCancelDto : this.refundEntries)
                {
                    taxValue += orderEntryToCancelDto.isTax() ? orderEntryToCancelDto.getOrderEntry().getAvalaraLineTax()
                            : BlCustomCancelRefundConstants.ZERO;
                }
            }
        }
        else
        {
            for (final BlOrderEntryToCancelDto orderEntryToCancelDto : this.refundEntries)
            {
                taxValue += orderEntryToCancelDto.isTax()
                        ? ((orderEntryToCancelDto.getOrderEntry().getAvalaraLineTax()
                        / Math.toIntExact(orderEntryToCancelDto.getQuantityAvailableToCancel()))
                        * orderEntryToCancelDto.getQuantityToCancel())
                        : BlCustomCancelRefundConstants.ZERO;
            }
        }
        return taxValue;
    }

    /**
     * This method will calculate amount
     *
     * @param orderAmount
     *           amt
     * @param globalTax
     *           tax
     * @param globalWaiver
     *           waiver
     * @param globalShipping
     *           shipping
     * @return amt
     */
    private double calculateAmount(double orderAmount, double globalTax, double globalWaiver, final double globalShipping)
    {
        for (final Map.Entry<AbstractOrderEntryModel, Long> entry : this.orderRefundableEntries.entrySet())
        {
            final OrderEntryModel orderEntry = (OrderEntryModel) entry.getKey();
            orderAmount += orderEntry.getBasePrice() * entry.getValue();
            if (this.globalTaxSelection.isChecked())
            {
                globalTax += (orderEntry.getAvalaraLineTax() / orderEntry.getQuantity()) * entry.getValue();
            }
            if (this.globalWaiverSelection.isChecked())
            {
                final double waiver = Boolean.TRUE.equals(orderEntry.getGearGuardProFullWaiverSelected())
                        ? orderEntry.getGearGuardProFullWaiverPrice()
                        : BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL;
                globalWaiver += (Boolean.TRUE.equals(orderEntry.getGearGuardWaiverSelected()) ? orderEntry.getGearGuardWaiverPrice()
                        : waiver) * entry.getValue();
            }
        }
        return blCustomCancelRefundService.calculateAmountOnCheckboxStatusFull(orderAmount, globalTax, globalWaiver, globalShipping,
                Double.parseDouble(this.globalTotalRefundAmount.getValue()));
    }

    /**
     * This method will set refund amount on order
     *
     * @param totalAmountToRefund
     *           amount
     */
    private void setRefundAmountOnOrder(final double totalAmountToRefund)
    {
        final OrderModel order = this.getOrderModel();
        order.setTotalRefundedAmount(order.getTotalRefundedAmount() == null ? totalAmountToRefund
                : (this.getTwoDecimalDoubleValue(order.getTotalRefundedAmount() + totalAmountToRefund)));
        getModelService().save(order);
        getModelService().refresh(order);
    }

    /**
     * this method will give two decimal value
     *
     * @param amount
     *           amount
     * @return double
     */
    private double getTwoDecimalDoubleValue(final double amount)
    {
        return BigDecimal.valueOf((amount < BlCustomCancelRefundConstants.ZERO) ? -amount : amount)
                .setScale(BlInventoryScanLoggingConstants.TWO, RoundingMode.HALF_EVEN).doubleValue();
    }

    /**
     * Build cancel request order cancel request.
     *
     * @return the order cancel request
     */
    private boolean buildRefundRequest()
    {
        if (this.getOrderModel() != null)
        {
            refundEntries = new ArrayList<>();
            if (this.globalCancelEntriesSelection.isChecked())
            {
                this.getOrderEntriesGridRows()
                        .forEach(entry -> this.createOrderRefundEntryRecord(this.getOrderCancelEntries(), (Row) entry));
            }
            else
            {
                this.getOrderEntriesGridRows().stream().filter(entryRow -> ((Checkbox) entryRow.getFirstChild()).isChecked())
                        .forEach(entry -> this.createOrderRefundEntryRecord(this.getOrderCancelEntries(), (Row) entry));
            }
            return CollectionUtils.isNotEmpty(this.getOrderCancelEntries());
        }
        return false;
    }

    /**
     * This method will create OrderCancelEntry
     *
     * @param orderCancelEntries
     *           entries
     * @param entry
     *           entry
     */
    private void createOrderRefundEntryRecord(final List<OrderCancelEntry> orderCancelEntries, final Row entry)
    {
        final BlOrderEntryToCancelDto orderEntry = entry.getValue();
        if (orderEntry.getQuantityAvailableToCancel() > BlCustomCancelRefundConstants.ZERO)
        {
            refundEntries.add(orderEntry);
            this.createOrderRefundEntry(orderCancelEntries, entry.getValue());
        }
    }

    /**
     * Create order cancel entry.
     *
     * @param orderCancelEntries
     *           the order cancel entries
     * @param entry
     *           the entry
     */
    private void createOrderRefundEntry(final List<OrderCancelEntry> orderCancelEntries, final Object entry)
    {
        final BlOrderEntryToCancelDto orderEntryToCancel = (BlOrderEntryToCancelDto) entry;
        final OrderCancelEntry orderCancelEntry = new OrderCancelEntry(orderEntryToCancel.getOrderEntry(),
                orderEntryToCancel.getQuantityToCancel(), orderEntryToCancel.getCancelOrderEntryComment(),
                orderEntryToCancel.getSelectedReason());
        orderCancelEntries.add(orderCancelEntry);
        integerLongMap.put(orderEntryToCancel.getOrderEntry().getEntryNumber() , orderEntryToCancel.getQuantityToCancel());
    }

    /**
     * This method will validate input events of cancel and refund popup
     */
    private boolean validateOrderEnteredQuantityAmountReason()
    {
        if (this.globalCancelEntriesSelection.isChecked())
        {
            if (Boolean.TRUE.equals(this.validateGlobalSelection()))
            {
                return Boolean.TRUE;
            }
        }
        else
        {
            final Optional<Component> checkedEntry = this.getOrderEntriesGridRows().stream()
                    .filter(row -> Boolean.TRUE.equals(((Checkbox) row.getChildren().iterator().next()).isChecked())).findFirst();
            if (!checkedEntry.isPresent())
            {
                this.errorMessageBox(this.getLabel(BlCustomCancelRefundConstants.CANCEL_CONFIRM_MISSING_SELECT_LINE),
                        this.getLabel(BlCustomCancelRefundConstants.CANCEL_CONFIRM_MISSING_SELECT_LINE_SELECTION));
                return Boolean.TRUE;
            }
            if (Boolean.TRUE.equals(this.validateEntries()))
            {
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
    private boolean validateEntries()
    {
        for (final Component row : this.getOrderEntriesGridRows())
        {
            if (((Checkbox) row.getChildren().iterator().next()).isChecked())
            {
                final int cancelQty = Integer
                        .parseInt(String.valueOf(((InputElement) row.getChildren().get(BlloggingConstants.TEN)).getRawValue()));
                final int cancellableQty = Integer
                        .parseInt(String.valueOf(((InputElement) row.getChildren().get(BlloggingConstants.NINE)).getRawValue()));
                if (cancelQty == BlCustomCancelRefundConstants.ZERO && cancellableQty != BlCustomCancelRefundConstants.ZERO)
                {
                    this.errorMessageBox(this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_MISSING_QUANTITY),
                            this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_MISSING_QUANTITY_HEADER));
                    return Boolean.TRUE;
                }
                else if (cancelQty > cancellableQty)
                {
                    this.errorMessageBox(this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_MISSING_QUANTITY_HIGHER),
                            this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_MISSING_QUANTITY_HEADER));
                    return Boolean.TRUE;
                }

                if (this.getValidateRefundAmountMessage(row, cancelQty, cancellableQty))
                {
                    return Boolean.TRUE;
                }

                /*if (((Combobox) row.getChildren().get(BlloggingConstants.TWELVE))
                        .getSelectedIndex() == -BlInventoryScanLoggingConstants.ONE
                        && cancellableQty != BlCustomCancelRefundConstants.ZERO)
                {
                    this.errorMessageBox(this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_ERROR_REASON),
                            this.getLabel(BlCustomCancelRefundConstants.CANCELORDER_ERROR_REASON_HEADER));
                    return Boolean.TRUE;
                }*/
            }
        }
        return Boolean.FALSE;
    }

    /**
     * validate order fields
     *
     * @return status
     */
    private boolean validateGlobalSelection()
    {
        final double amount = Double.parseDouble(this.globalTotalRefundAmount.getValue());
        if ((amount + Double.parseDouble(this.totalRefundedAmount.getValue())) > this.getOrderModel().getOriginalOrderTotalAmount())
        {
            this.errorMessageBox(this.getLabel(BlCustomCancelRefundConstants.INVALID_ORDER_AMOUNT),
                    this.getLabel(BlCustomCancelRefundConstants.EMPTY_AMOUNT_HEADER));
            return Boolean.TRUE;
        }
        if (BigDecimal.valueOf(amount).scale() > BlInventoryScanLoggingConstants.TWO)
        {
            this.errorMessageBox(BlCustomCancelRefundConstants.INVALID_ENTERED_AMOUNT_AMOUNT_SHOULD_BE_UP_TO_TWO_DECIMAL_DIGITS_ONLY,
                    this.getLabel(BlCustomCancelRefundConstants.EMPTY_AMOUNT_HEADER));
            return Boolean.TRUE;
        }
        return Boolean.FALSE;
    }

    /**
     * Gets validate order message.
     *
     * @return the validate order message
     */
    private boolean getValidateRefundAmountMessage(final Component row, final int cancelQty, final int cancellableQty)
    {
        final double amount = Double
                .parseDouble(String.valueOf(((InputElement) row.getChildren().get(BlloggingConstants.ELEVEN)).getRawValue()));
        if (amount <= BlCustomCancelRefundConstants.ZERO)
        {
            this.errorMessageBox(this.getLabel(BlCustomCancelRefundConstants.ZERO_ORDER_AMOUNT),
                    this.getLabel(BlCustomCancelRefundConstants.EMPTY_AMOUNT_HEADER));
            return Boolean.TRUE;
        }
        else if (amount > (this.getTotalProductPriceForCancelQuantity(row, cancelQty, cancellableQty))
                || (amount + BigDecimal.valueOf(Double.parseDouble(this.totalRefundedAmount.getValue()))
                .setScale(BlInventoryScanLoggingConstants.TWO, RoundingMode.HALF_EVEN).doubleValue()) > this.getOrderModel()
                .getOriginalOrderTotalAmount())
        {
            this.errorMessageBox(this.getLabel(BlCustomCancelRefundConstants.INVALID_ORDER_AMOUNT),
                    this.getLabel(BlCustomCancelRefundConstants.EMPTY_AMOUNT_HEADER));
            return Boolean.TRUE;
        }
        return Boolean.FALSE;
    }

    /**
     * This method will calculate total price for line item based on enter cancel quantity
     *
     * @param row
     *           line
     * @param cancelQty
     *           quantity
     * @param cancellableQty
     *           quantity
     * @return final price
     */
    private double getTotalProductPriceForCancelQuantity(final Component row, final int cancelQty, final int cancellableQty)
    {
        final Checkbox tax = ((Checkbox) row.getChildren().get(BlloggingConstants.SIX));
        final Checkbox waiver = ((Checkbox) row.getChildren().get(BlloggingConstants.SEVEN));
        return blCustomCancelRefundService.getTotalAmountPerEntry(cancelQty, cancellableQty,
                Double.parseDouble(String.valueOf(((InputElement) row.getChildren().get(BlloggingConstants.FOUR)).getRawValue())),
                (Double.parseDouble(tax.getLabel()) / cancellableQty), Double.parseDouble(waiver.getLabel()));
    }

    /**
     * Add listeners.
     */
    private void addListeners()
    {
        final List<Component> rows = this.getOrderEntries().getRows().getChildren();
        for (final Component row : rows)
        {
            for (final Component myComponent : row.getChildren())
            {
                if (myComponent instanceof Checkbox)
                {
                    myComponent.addEventListener(BlCustomCancelRefundConstants.ON_CHECK,
                            event -> this.handleRow((Row) event.getTarget().getParent()));
                }
                else if (myComponent instanceof Intbox)
                {
                    myComponent.addEventListener(BlCustomCancelRefundConstants.ON_CHANGING, event -> {
                        this.autoSelect(event);
                        this.inputBoxCustomization(event);
                    });
                }
                else if (myComponent instanceof Textbox)
                {
                    myComponent.addEventListener(BlCustomCancelRefundConstants.ON_CHANGING, event -> {
                        this.autoSelect(event);
                        ((BlOrderEntryToCancelDto) ((Row) event.getTarget().getParent()).getValue())
                                .setCancelOrderEntryComment(((InputEvent) event).getValue());
                    });
                }
                else if (myComponent instanceof Doublebox)
                {
                    myComponent.addEventListener(BlCustomCancelRefundConstants.ON_CHANGE, event -> {
                        this.autoSelect(event);
                        ((BlOrderEntryToCancelDto) ((Row) event.getTarget().getParent()).getValue())
                                .setAmount(Double.parseDouble(((InputEvent) event).getValue()));
                    });
                }
            }
        }
        this.globalCancelComment.addEventListener(BlCustomCancelRefundConstants.ON_CHANGING, this::handleGlobalCancelComment); //TO-DO Needs to check the row number 13 to 12 if needed
        this.globalCancelEntriesSelection.addEventListener(BlCustomCancelRefundConstants.ON_CHECK,
                event -> this.selectAllEntries());
        this.globalShippingSelection.addEventListener(BlCustomCancelRefundConstants.ON_CHECK, this::calculateOrderRefundAmount);
        this.globalTaxSelection.addEventListener(BlCustomCancelRefundConstants.ON_CHECK, this::calculateOrderRefundAmount);
        this.globalWaiverSelection.addEventListener(BlCustomCancelRefundConstants.ON_CHECK, this::calculateOrderRefundAmount);
    }

    /**
     * Input Box populate Order Entry Cost
     *
     * @param event
     *           event
     */
    private void inputBoxCustomization(final Event event)
    {
        if (StringUtils.isNotEmpty(((InputEvent) event).getValue()))
        {
            ((BlOrderEntryToCancelDto) ((Row) event.getTarget().getParent()).getValue())
                    .setQuantityToCancel(Long.valueOf(((InputEvent) event).getValue()));
            this.populateEntryLevelAmount((Row) event.getTarget().getParent());
        }
    }

    /**
     * this method will calculate amount for order level
     *
     * @param event
     *           event
     */
    private void calculateOrderRefundAmount(final Event event)
    {
        double orderAmount = BlCustomCancelRefundConstants.ZERO;
        if (Double.parseDouble(this.totalRefundedAmount.getValue()) <= BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL)
        {
            orderAmount = this.getOrderModel().getSubtotal();
            if (this.globalCancelEntriesSelection.isChecked())
            {
                orderAmount += this.getOrderModel().getDeliveryCost();
            }
            if (this.globalTaxSelection.isChecked())
            {
                orderAmount += this.getOrderModel().getTotalTax();
            }
            if (this.globalWaiverSelection.isChecked())
            {
                orderAmount += this.getOrderModel().getTotalDamageWaiverCost();
            }
        }
        else
        {
            orderAmount = getOrderAmount(orderAmount);
        }
        this.globalTotalRefundAmount.setValue(String.valueOf(
                BigDecimal.valueOf(orderAmount).setScale(BlInventoryScanLoggingConstants.TWO, RoundingMode.HALF_EVEN).doubleValue()));
    }

    /**
     * This method will calculate order amount
     *
     * @param orderAmount
     *           amt
     * @return amt
     */
    private double getOrderAmount(double orderAmount)
    {
        double tax = BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL;
        double waiver = BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL;
        for (final Map.Entry<AbstractOrderEntryModel, Long> entry : this.orderRefundableEntries.entrySet())
        {
            final OrderEntryModel orderEntry = (OrderEntryModel) entry.getKey();
            orderAmount += orderEntry.getBasePrice() * entry.getValue();
            if (this.globalTaxSelection.isChecked())
            {
                tax += (orderEntry.getAvalaraLineTax() / orderEntry.getQuantity()) * entry.getValue();
            }
            if (this.globalWaiverSelection.isChecked())
            {
                final double totWaiver = Boolean.TRUE.equals(orderEntry.getGearGuardProFullWaiverSelected())
                        ? orderEntry.getGearGuardProFullWaiverPrice()
                        : BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL;
                waiver += (Boolean.TRUE.equals(orderEntry.getGearGuardWaiverSelected()) ? orderEntry.getGearGuardWaiverPrice()
                        : totWaiver) * entry.getValue();
            }
        }
        if (this.globalShippingSelection.isChecked())
        {
            orderAmount += this.getOrderModel().getDeliveryCost();
        }
        if (tax > BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL)
        {
            orderAmount += tax;
        }
        if (waiver > BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL)
        {
            orderAmount += waiver;
        }
        return orderAmount;
    }

    /**
     * This method will log message
     *
     * @param message
     *           msg
     */
    private void logCancelRefundLogger(final String message, final Object... args)
    {
        BlLogger.logFormatMessageInfo(LOGGER, DEBUG, message, args);
    }

    /**
     * This message will show failure message box
     *
     * @param message
     *           msg
     */
    private void failureMessageBox(final String message)
    {
        Messagebox.show(message, BlCustomCancelRefundConstants.FAILURE, Messagebox.OK, Messagebox.ERROR);
    }

    /**
     * This message will show failure message box
     *
     * @param m1
     *           msg
     * @param m2
     *           msg
     */
    private void errorMessageBox(final String m1, final String m2)
    {
        Messagebox.show(m1, m2, Messagebox.OK, Messagebox.ERROR);
    }

    /**
     * This message will show success message box
     *
     * @param message
     *           msg
     */
    private void successMessageBox(final String message)
    {
        Messagebox.show(message, BlCustomCancelRefundConstants.SUCCESS, Messagebox.OK, Messagebox.INFORMATION);
    }

    /**
     * Handle row.
     *
     * @param row
     *           the row
     */
    private void handleRow(final Row row)
    {
        try
        {
            final BlOrderEntryToCancelDto myEntry = row.getValue();
            if (!((Checkbox) row.getChildren().iterator().next()).isChecked())
            {
                this.applyToRow(BlCustomCancelRefundConstants.ZERO, BlloggingConstants.TEN, row);
                this.applyToRow(null, BlloggingConstants.TWELVE, row);
               // this.applyToRow(null, BlloggingConstants.THIRTEEN, row);
                myEntry.setQuantityToCancel(BlCustomCancelRefundConstants.ZERO_LONG);
                myEntry.setSelectedReason(null);
                myEntry.setCancelOrderEntryComment(null);
            }
            else
            {
                this.applyToRow(this.globalCancelComment.getValue(), BlloggingConstants.TWELVE, row);
                myEntry.setSelectedReason(null);
                myEntry.setCancelOrderEntryComment(this.globalCancelComment.getValue());
            }
            this.populateEntryLevelAmount(row);
        }
        catch (final WrongValueException wrongValueException)
        {
            BlLogger.logMessage(LOGGER, Level.ERROR, "Exception occurred while cancel and refund the order {}",
                    this.getOrderModel().getCode(), wrongValueException);
        }
    }

    /**
     * this method will populate amount on entry level
     *
     * @param row
     *           row
     */
    private void populateEntryLevelAmount(final Row row)
    {
        final BlOrderEntryToCancelDto myEntry = row.getValue();
        if (myEntry.getQuantityToCancel() > BlInventoryScanLoggingConstants.ZERO
                && myEntry.getQuantityToCancel() <= myEntry.getQuantityAvailableToCancel())
        {
            final OrderEntryModel orderEntryModel = (OrderEntryModel) myEntry.getOrderEntry();
            final Checkbox tax = (Checkbox) row.getChildren().get(BlInventoryScanLoggingConstants.SIX);
            final double taxAmount = (tax.isChecked())
                    ? (orderEntryModel.getAvalaraLineTax() / myEntry.getQuantityAvailableToCancel())
                    : BlInventoryScanLoggingConstants.ZERO;
            final Checkbox waiver = (Checkbox) row.getChildren().get(BlInventoryScanLoggingConstants.SEVEN);
            final double totWaiver = Boolean.TRUE.equals(orderEntryModel.getGearGuardProFullWaiverSelected())
                    ? orderEntryModel.getGearGuardProFullWaiverPrice()
                    : BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL;
            final double waiverAmount = Boolean.TRUE.equals(orderEntryModel.getGearGuardWaiverSelected())
                    ? orderEntryModel.getGearGuardWaiverPrice()
                    : totWaiver;
            final double refundAmount = orderEntryModel.getBasePrice() + taxAmount
                    + (waiver.isChecked() ? waiverAmount : BlInventoryScanLoggingConstants.ZERO);
            final double finalAmount = BigDecimal.valueOf(refundAmount * myEntry.getQuantityToCancel())
                    .setScale(BlInventoryScanLoggingConstants.TWO, RoundingMode.HALF_EVEN).doubleValue();
            myEntry.setAmount(finalAmount);
            myEntry.setTax(tax.isChecked());
            myEntry.setWaiver(waiver.isChecked());
            ((Doublebox) row.getChildren().get(BlInventoryScanLoggingConstants.ELEVEN)).setValue(finalAmount);
        }
        else
        {
            ((Doublebox) row.getChildren().get(BlInventoryScanLoggingConstants.ELEVEN)).setValue(BlCustomCancelRefundConstants.ZERO);
        }
    }

    /**
     * This method will initialize popup fields from order
     *
     * @param inputObject
     *           order
     */
    private void initializePopupRequiredFields(final OrderModel inputObject)
    {
        this.setOrderModel(inputObject);
        this.orderEntriesToRefund = new HashSet<>();
        if (CollectionUtils.isNotEmpty(this.orderModel.getEntries()))
        {
            this.orderRefundableEntries = this.orderModel.getEntries().stream().collect(Collectors.toMap(entryModel -> entryModel,
                    entryModel -> (getRefundableQuantity((OrderEntryModel) entryModel , inputObject)), (a, b) -> b));
        }
        if (!this.orderRefundableEntries.isEmpty())
        {
            this.orderRefundableEntries.forEach((entry,
                                                 refundableQty) -> this.orderEntriesToRefund.add(new BlOrderEntryToCancelDto(entry, Lists.newArrayList(),
                    refundableQty, this.determineDeliveryMode(entry), BlCustomCancelRefundConstants.ZERO_LONG, Boolean.FALSE,
                    Boolean.FALSE,
                    (long) blCustomCancelRefundService.getTotalRefundedAmountOnOrderEntry(
                            blCustomCancelRefundService.getAllRefundEntriesForOrderEntry(String.valueOf(entry.getEntryNumber()),
                                    this.orderModel.getCode(), Boolean.TRUE)))));
        }
        this.setFieldValuesInTextBox(this.getOrderModel());
    }

    /**
     * Determine delivery mode string.
     *
     * @param orderEntry
     *           the order entry
     * @return the string
     */
    private String determineDeliveryMode(final AbstractOrderEntryModel orderEntry)
    {
        String deliveryModeResult;
        if (orderEntry.getDeliveryMode() != null)
        {
            deliveryModeResult = orderEntry.getDeliveryMode().getName();
        }
        else if (orderEntry.getDeliveryPointOfService() != null)
        {
            deliveryModeResult = this.getLabel(BlCustomCancelRefundConstants.CANCEL_CONFIRM_PICKUP);
        }
        else
        {
            if (orderEntry.getOrder().getDeliveryMode() != null)
            {
                deliveryModeResult = orderEntry.getOrder().getDeliveryMode().getName() != null
                        ? orderEntry.getOrder().getDeliveryMode().getName()
                        : orderEntry.getOrder().getDeliveryMode().getCode();
            }
            else
            {
                deliveryModeResult = null;
            }
        }
        return deliveryModeResult;
    }

    /**
     * Sets amount and other field values in text boxes.
     */
    private void setFieldValuesInTextBox(final OrderModel order)
    {
        this.globalCancelEntriesSelection.setChecked(false);
        this.globalWaiverSelection.setChecked(false);
        this.globalTaxSelection.setChecked(false);
        this.globalShippingSelection.setChecked(false);
        this.getWidgetInstanceManager()
                .setTitle(this.getWidgetInstanceManager().getLabel(BlCustomCancelRefundConstants.CANCEL_CONFIRM_TITLE)
                        + StringUtils.SPACE + order.getCode());
        this.customerName.setValue(order.getUser().getDisplayName());
        this.totalLineItemPrice.setValue(formatAmount(order.getSubtotal()));
        this.totalTax.setValue(formatAmount(order.getTotalTax()));
        this.totalDamageWaiverCost.setValue(formatAmount(order.getTotalDamageWaiverCost()));
        this.totalShippingCost.setValue(formatAmount(order.getDeliveryCost()));
        this.totalAmount.setValue(formatAmount(order.getOriginalOrderTotalAmount()));
        final String transId = order.getPaymentTransactions().size() == BlCustomCancelRefundConstants.ZERO ? StringUtils.EMPTY
                : (order.getPaymentTransactions().get(BlCustomCancelRefundConstants.ZERO) == null ? StringUtils.EMPTY
                : order.getPaymentTransactions().get(BlCustomCancelRefundConstants.ZERO).getRequestId());
        this.transactionId.setValue(CollectionUtils.isEmpty(order.getPaymentTransactions()) ? StringUtils.EMPTY : transId);
        this.totalRefundedAmount.setValue(
                String.valueOf(this.getOrderModel().getTotalRefundedAmount() == null ? BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL
                        : this.getOrderModel().getTotalRefundedAmount()));

        if (this.getOrderModel().getTotalRefundedAmount() == null
                || this.getOrderModel().getTotalRefundedAmount() == BlCustomCancelRefundConstants.ZERO_DOUBLE_VAL)
        {
            this.globalTotalRefundAmount.setValue(String.valueOf(order.getSubtotal()));
            this.globalShippingSelection.setDisabled(Boolean.FALSE);
        }
        else
        {
            double orderAmount = BlCustomCancelRefundConstants.ZERO;
            for (final Map.Entry<AbstractOrderEntryModel, Long> entry : this.orderRefundableEntries.entrySet())
            {
                orderAmount += entry.getKey().getBasePrice() * entry.getValue();
            }
            this.globalTotalRefundAmount.setValue(String.valueOf(this.getTwoDecimalDoubleValue(orderAmount)));
            this.globalShippingSelection.setDisabled(Boolean.TRUE);
        }
    }

    /**
     * Format amount string.
     *
     * @param amount
     *           the amount
     * @return the string
     */
    private String formatAmount(final Double amount)
    {
        final DecimalFormat decimalFormat = (DecimalFormat) NumberFormat.getNumberInstance(Locales.getCurrent());
        decimalFormat.applyPattern(BlCustomCancelRefundConstants.ZERO_DOUBLE);
        return decimalFormat.format(amount);
    }

    /**
     * Handle global cancel comment.
     *
     * @param event
     *           the event
     */
    private void handleGlobalCancelComment(final Event event)
    {
        this.applyToGrid(((InputEvent) event).getValue(), BlloggingConstants.TWELVE);
        this.getOrderEntriesGridRows().stream().filter(entry -> ((Checkbox) entry.getChildren().iterator().next()).isChecked())
                .forEach(entry -> {
                    final BlOrderEntryToCancelDto myEntry = ((Row) entry).getValue();
                    myEntry.setCancelOrderEntryComment(((InputEvent) event).getValue());
                });
    }

    /**
     * Select all entries.
     */
    private void selectAllEntries()
    {
        double orderAmount = BlCustomCancelRefundConstants.ZERO;
        this.applyToGrid(Boolean.TRUE, BlCustomCancelRefundConstants.ZERO);
        for (final Component row : this.getOrderEntriesGridRows())
        {
            final Component firstComponent = row.getChildren().iterator().next();
            if (firstComponent instanceof Checkbox)
            {
                ((Checkbox) firstComponent).setChecked(this.globalCancelEntriesSelection.isChecked());
            }
            this.handleRow((Row) row);
            if (this.globalCancelEntriesSelection.isChecked())
            {
                final InputElement cancellableQty = (InputElement) row.getChildren().get(BlloggingConstants.NINE);
                this.applyToRow(Integer.parseInt(String.valueOf(cancellableQty.getRawValue())), BlloggingConstants.NINE, row);
            }
        }
        if (this.globalCancelEntriesSelection.isChecked())
        {
            this.orderEntriesToRefund
                    .forEach(entry -> entry.setQuantityToCancel(this.orderRefundableEntries.get(entry.getOrderEntry())));
            orderAmount = Double.valueOf(this.globalTotalRefundAmount.getValue()) + this.getOrderModel().getDeliveryCost();
        }
        if (!this.globalCancelEntriesSelection.isChecked())
        {
            orderAmount = Double.valueOf(this.globalTotalRefundAmount.getValue()) - this.getOrderModel().getDeliveryCost();
        }
        this.globalTotalRefundAmount.setValue(String.valueOf(
                BigDecimal.valueOf(orderAmount).setScale(BlInventoryScanLoggingConstants.TWO, RoundingMode.HALF_EVEN).doubleValue()));
    }

    /**
     * Apply to grid.
     *
     * @param data
     *           the data
     * @param childrenIndex
     *           the children index
     */
    private void applyToGrid(final Object data, final int childrenIndex)
    {
        this.getOrderEntriesGridRows().stream().filter(entry -> ((Checkbox) entry.getChildren().iterator().next()).isChecked())
                .forEach(entry -> this.applyToRow(data, childrenIndex, entry));
    }

    /**
     * Apply to row.
     *
     * @param data
     *           the data
     * @param childrenIndex
     *           the children index
     * @param row
     *           the row
     */
    private void applyToRow(final Object data, final int childrenIndex, final Component row)
    {
        int index = BlInventoryScanLoggingConstants.ZERO;
        for (final Component myComponent : row.getChildren())
        {
            if (index == childrenIndex)
            {
                setValueInRow(data, myComponent);
            }
            ++index;
        }
    }

    /**
     * Sets value in row.
     *
     * @param data
     *           the data
     * @param myComponent
     *           the my component
     */
    private void setValueInRow(final Object data, final Component myComponent)
    {
        if (myComponent instanceof Checkbox && data != null)
        {
            ((Checkbox) myComponent).setChecked((Boolean) data);
        }
        if (myComponent instanceof Combobox)
        {
            if (data == null)
            {
                ((Combobox) myComponent).setSelectedItem(null);
            }
            else
            {
                ((Combobox) myComponent).setSelectedIndex((Integer) data);
            }
        }
        else if (myComponent instanceof Intbox)
        {
            ((Intbox) myComponent).setValue((Integer) data);
        }
        else if (myComponent instanceof Textbox)
        {
            ((Textbox) myComponent).setValue((String) data);
        }
    }

    /**
     * Auto select.
     *
     * @param event
     *           the event
     */
    private void autoSelect(final Event event)
    {
        ((Checkbox) event.getTarget().getParent().getChildren().iterator().next()).setChecked(true);
    }

    /**
     * Gets order entries grid rows.
     *
     * @return the order entries grid rows
     */
    private List<Component> getOrderEntriesGridRows()
    {
        return this.getOrderEntries().getRows().getChildren();
    }

    /**
     * This method created to get the refundable quanty for order entry
     * @param entryModel  entryModel
     * @param orderModel orderModel
     * @return long
     */
    private Long getRefundableQuantity(final OrderEntryModel entryModel, final OrderModel orderModel) {
        if(StringUtils.equalsIgnoreCase(OrderStatus.CANCELLED.getCode() , orderModel.getStatus().getCode())) {
            return entryModel.getCancelledQuantity() - ObjectUtils.defaultIfNull(entryModel.getRefundedQuantity() , 0L);
        }
        return entryModel.getQuantityPending() - ObjectUtils.defaultIfNull(entryModel.getRefundedQuantity() , 0L);
    }

    private Locale getLocale()
    {
        return this.getCockpitLocaleService().getCurrentLocale();
    }

    private BackofficeLocaleService getCockpitLocaleService()
    {
        return this.cockpitLocaleService;
    }

    protected Grid getOrderEntries()
    {
        return this.orderEntries;
    }

    protected OrderModel getOrderModel()
    {
        return this.orderModel;
    }

    public void setOrderModel(final OrderModel orderModel)
    {
        this.orderModel = orderModel;
    }

    protected OrderCancelService getOrderCancelService()
    {
        return this.orderCancelService;
    }

    protected EnumerationService getEnumerationService()
    {
        return this.enumerationService;
    }

    protected ModelService getModelService()
    {
        return this.modelService;
    }

    protected CockpitEventQueue getCockpitEventQueue()
    {
        return this.cockpitEventQueue;
    }

    protected UserService getUserService()
    {
        return this.userService;
    }

    protected NotificationService getNotificationService()
    {
        return this.notificationService;
    }

    public List<BlOrderEntryToCancelDto> getCancelAndRefundEntries()
    {
        return refundEntries;
    }

    public void setCancelAndRefundEntries(final List<BlOrderEntryToCancelDto> cancelAndRefundEntries)
    {
        this.refundEntries = cancelAndRefundEntries;
    }

    /**
     * @return the defaultBlConsignmentService
     */
    public BlConsignmentService getDefaultBlConsignmentService()
    {
        return defaultBlConsignmentService;
    }

    /**
     * @param defaultBlConsignmentService
     *           the defaultBlConsignmentService to set
     */
    public void setDefaultBlConsignmentService(final BlConsignmentService defaultBlConsignmentService)
    {
        this.defaultBlConsignmentService = defaultBlConsignmentService;
    }

    public DefaultBlESPEventService getBlEspEventService()
    {
        return blEspEventService;
    }

    public void setBlEspEventService(final DefaultBlESPEventService blEspEventService)
    {
        this.blEspEventService = blEspEventService;
    }

    public List<OrderCancelEntry> getOrderCancelEntries()
    {
        return orderCancelEntries;
    }

    public void setOrderCancelEntries(final List<OrderCancelEntry> orderCancelEntries)
    {
        this.orderCancelEntries = orderCancelEntries;
    }

    public DefaultBlUserService getDefaultBlUserService()
    {
        return defaultBlUserService;
    }

    public void setDefaultBlUserService(final DefaultBlUserService defaultBlUserService)
    {
        this.defaultBlUserService = defaultBlUserService;
    }

    /**
     * @return the blOrderService
     */
    public BlOrderService getBlOrderService()
    {
        return blOrderService;
    }

    /**
     * @param blOrderService
     *           the blOrderService to set
     */
    public void setBlOrderService(final BlOrderService blOrderService)
    {
        this.blOrderService = blOrderService;
    }

}
