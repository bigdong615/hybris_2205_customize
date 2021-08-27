package com.bl.core.services.cancelandrefund.service.impl;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.services.cancelandrefund.dao.BlCustomCancelRefundDao;
import com.bl.core.services.cancelandrefund.service.BlCustomCancelRefundService;
import com.braintree.command.request.BrainTreeRefundTransactionRequest;
import com.braintree.command.result.BrainTreeRefundTransactionResult;
import com.braintree.method.BrainTreePaymentService;
import de.hybris.platform.basecommerce.enums.RefundReason;
import de.hybris.platform.basecommerce.enums.ReturnAction;
import de.hybris.platform.core.model.c2l.CurrencyModel;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.payment.PaymentService;
import de.hybris.platform.payment.enums.PaymentTransactionType;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.returns.ReturnService;
import de.hybris.platform.returns.model.RefundEntryModel;
import de.hybris.platform.returns.model.ReturnRequestModel;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Custom cancel and refund service implementer
 *
 * @author Namrata Lohar
 */
public class DefaultBlCustomCancelRefundService implements BlCustomCancelRefundService {

    private static final Logger LOG = Logger.getLogger(DefaultBlCustomCancelRefundService.class);

    private BlCustomCancelRefundDao blCustomCancelRefundDao;
    private BrainTreePaymentService brainTreePaymentService;
    private CommonI18NService commonI18NService;
    private PaymentService paymentService;
    private transient ModelService modelService;
    private ReturnService returnService;

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<RefundEntryModel> getAllRefundEntriesForOrderEntry(final String abstractOrderEntryCode, final String orderNumber,
                                                                         boolean originalInstance) {
        return getBlCustomCancelRefundDao().getAllRefundEntriesForOrderEntry(abstractOrderEntryCode, orderNumber, originalInstance);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Collection<RefundEntryModel>> getAllRefundEntriesForOrder(final OrderModel orderNumber, final boolean originalInstance) {
        final Map<String, Collection<RefundEntryModel>> collectionMap = new HashMap<>();
        if (CollectionUtils.isNotEmpty(orderNumber.getEntries())) {
            for (AbstractOrderEntryModel orderEntryModel : orderNumber.getEntries()) {
                final Collection<RefundEntryModel> refundEntryModels = this.getAllRefundEntriesForOrderEntry(
                        String.valueOf(orderEntryModel.getEntryNumber()), orderNumber.getCode(), originalInstance);
                if (CollectionUtils.isNotEmpty(refundEntryModels)) {
                    collectionMap.put((orderNumber.getCode() + BlCoreConstants.HYPHEN + orderEntryModel.getEntryNumber()), refundEntryModels);
                }
            }
        }
        return collectionMap;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BrainTreeRefundTransactionResult createBrainTreeRefundTransactionRequest(final String transactionId, final BigDecimal amount,
                                                                                    final String orderId) {
        final BrainTreeRefundTransactionRequest request = new BrainTreeRefundTransactionRequest(transactionId);
        request.setAmount(amount);
        request.setOrderId(orderId);
        request.setTransactionId(transactionId);
        return getBrainTreePaymentService().refundTransaction(request);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Optional<PaymentTransactionEntryModel> getCapturedPaymentTransaction(final OrderModel orderModel) {
        return orderModel.getPaymentTransactions().get(BlInventoryScanLoggingConstants.ZERO).getEntries().stream().filter(entry ->
                BlCoreConstants.ACCEPTED.equalsIgnoreCase(entry.getTransactionStatus()) && entry.getAmount().doubleValue() >
                        BlInventoryScanLoggingConstants.ONE && (PaymentTransactionType.CAPTURE.equals(entry.getType()))).findFirst();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double getTotalRefundedAmountOnOrderEntry(final Collection<RefundEntryModel> refundEntryModels) {
        double refundedAmount = BlInventoryScanLoggingConstants.ZERO;
        for (final RefundEntryModel refundEntryModel : refundEntryModels) {
            refundedAmount = refundedAmount + (refundEntryModel.getAmount() != null ? refundEntryModel.getAmount().doubleValue() :
                    BlInventoryScanLoggingConstants.ZERO);
        }
        return refundedAmount;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double getTotalRefundedAmountOnOrder(final OrderModel orderModel) {
        double refundedAmount = BlInventoryScanLoggingConstants.ZERO;
        for(final PaymentTransactionEntryModel transactionModel : orderModel.getPaymentTransactions().get(0).getEntries()) {
            if(BlCoreConstants.ACCEPTED.equalsIgnoreCase(transactionModel.getTransactionStatus()) && transactionModel.getType().getCode()
                .startsWith("REFUND")) {
                refundedAmount = refundedAmount + transactionModel.getAmount().doubleValue();
            }
        }
        return refundedAmount;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void createRefundTransaction(final PaymentTransactionModel transaction, final BrainTreeRefundTransactionResult result,
                                        final PaymentTransactionType transactionType) {
        final String newEntryCode = getPaymentService().getNewPaymentTransactionEntryCode(transaction, transactionType);
        final PaymentTransactionEntryModel entry = getModelService().create(PaymentTransactionEntryModel.class);
        entry.setType(transactionType);
        entry.setCode(newEntryCode);
        entry.setRequestId(result.getTransactionId());
        entry.setPaymentTransaction(transaction);
        entry.setCurrency(resolveCurrency(result.getCurrencyIsoCode()));
        entry.setAmount(formatAmount(result.getAmount()));
        entry.setTransactionStatus(result.getTransactionStatus().toString());
        entry.setTransactionStatusDetails(result.getTransactionStatusDetails().toString());
        entry.setTime(new Date());
        getModelService().saveAll(entry, transaction);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void createRefundAndPaymentEntry(final BrainTreeRefundTransactionResult result, final ReturnRequestModel returnRequestModel,
                                            final AbstractOrderEntryModel orderEntry, final Long qty, final Double totalPrice,
                                            final OrderModel orderModel, final PaymentTransactionType refundStandalone,
                                            final String notes) {
        final RefundEntryModel refundEntry = returnService.createRefund(returnRequestModel, orderEntry, notes, qty, ReturnAction.IMMEDIATE,
                RefundReason.WRONGDESCRIPTION);
        refundEntry.setAmount(BigDecimal.valueOf(totalPrice));
        modelService.save(refundEntry);
        returnRequestModel.setSubtotal(returnRequestModel.getReturnEntries().stream().filter(entry -> entry instanceof RefundEntryModel)
                .map(refund -> ((RefundEntryModel) refund).getAmount()).reduce(BigDecimal.ZERO, BigDecimal::add));
        modelService.save(returnRequestModel);
        this.createRefundTransaction(orderModel.getPaymentTransactions().get(BlInventoryScanLoggingConstants.ZERO), result, refundStandalone);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double calculateAmountOnCheckboxStatusFull(final boolean tax, final boolean waiver, final boolean shipping, final OrderModel order,
                                                      final double amount) {
        final double totalSelectionAmount = order.getSubtotal() + ((Boolean.TRUE.equals(shipping) ? order.getDeliveryCost() : BlInventoryScanLoggingConstants.ZERO)
                - (Boolean.TRUE.equals(tax) ? order.getTotalTax() : BlInventoryScanLoggingConstants.ZERO)
                - (Boolean.TRUE.equals(waiver) ? order.getTotalDamageWaiverCost() : BlInventoryScanLoggingConstants.ZERO));
        return totalSelectionAmount > amount ? totalSelectionAmount : amount;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double calculateAmountOnCheckboxStatusPartial(final OrderModel orderModel, final AbstractOrderEntryModel order,
                                                         final Map<String, Object> collectionMap, final Long qtyToCancel) {
        final Collection<RefundEntryModel> refundEntryModels = this.getAllRefundEntriesForOrderEntry(String.valueOf(order.getEntryNumber()),
                orderModel.getCode(), Boolean.TRUE);
        double totalOrderRefundedAmount = BlInventoryScanLoggingConstants.ZERO;
        if(CollectionUtils.isNotEmpty(refundEntryModels)) {
            totalOrderRefundedAmount = this.getTotalRefundedAmountOnOrderEntry(refundEntryModels);
        }
        final double entryTotal = order.getBasePrice() + order.getAvalaraLineTax() + (order.getGearGuardWaiverSelected()
                ? order.getGearGuardWaiverPrice() : order.getGearGuardProFullWaiverPrice());

        final double currentEntryTotal = (order.getBasePrice() * qtyToCancel) + (Boolean.TRUE.equals(collectionMap.get("Tax"))
                ? (order.getAvalaraLineTax() * qtyToCancel) : BlInventoryScanLoggingConstants.ZERO) + (Boolean.TRUE.equals(collectionMap.get("Waiver"))
                ? (order.getGearGuardWaiverPrice() * qtyToCancel) : BlInventoryScanLoggingConstants.ZERO);

        if(((Double) collectionMap.get("Amount")) + totalOrderRefundedAmount <= entryTotal) {
            return BigDecimal.valueOf(currentEntryTotal).setScale(BlInventoryScanLoggingConstants.TWO, RoundingMode.HALF_EVEN).doubleValue();
        } else {
            return BigDecimal.valueOf(BlInventoryScanLoggingConstants.ZERO).setScale(BlInventoryScanLoggingConstants.TWO, RoundingMode.HALF_EVEN).doubleValue();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Object> collectSelectionCheckboxAndCreateMap(final boolean tax, final boolean waiver, final Boolean shipping,
                                                                     final Double amount) {
        final Map<String, Object> selectionAttributeMap = new HashMap<>();
        selectionAttributeMap.put("Tax", tax);
        selectionAttributeMap.put("Waiver", waiver);
        if(shipping != null) {
            selectionAttributeMap.put("Shipping", shipping);
        }
        if(amount != null) {
            selectionAttributeMap.put("Amount", amount);
        }

        return selectionAttributeMap;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double getTotalAmountPerEntry(int cancelQty, final int cancellableQty, final double productPrice, final boolean tax,
                                         final double taxLabel, final boolean waiver, final double waiverLabel) {
        final double totalProductPrice = (productPrice * cancelQty) + ((taxLabel/cancellableQty) * cancelQty) + (waiverLabel);
        /*final double totalProductPrice = (productPrice * cancelQty) + (tax ? (perEntryTax * cancelQty) : BlInventoryScanLoggingConstants.ZERO)
                + (waiver ? waiverLabel : BlInventoryScanLoggingConstants.ZERO);*/
        return BigDecimal.valueOf(totalProductPrice).setScale(BlInventoryScanLoggingConstants.TWO, RoundingMode.HALF_EVEN).doubleValue();
    }

    protected CurrencyModel resolveCurrency(final String currencyIsoCode) {
        return getCommonI18NService().getCurrency(currencyIsoCode);
    }

    private BigDecimal formatAmount(final BigDecimal amount) {
        return amount.setScale(getCurrencyDigit(), RoundingMode.HALF_EVEN);
    }

    private int getCurrencyDigit() {
        final CurrencyModel currency = getCommonI18NService().getCurrentCurrency();
        if (currency != null) {
            final Integer digits = currency.getDigits();
            return digits != null ? digits : BlInventoryScanLoggingConstants.TWO;
        }
        return BlInventoryScanLoggingConstants.TWO;
    }

    public BlCustomCancelRefundDao getBlCustomCancelRefundDao() {
        return blCustomCancelRefundDao;
    }

    public void setBlCustomCancelRefundDao(BlCustomCancelRefundDao blCustomCancelRefundDao) {
        this.blCustomCancelRefundDao = blCustomCancelRefundDao;
    }

    public BrainTreePaymentService getBrainTreePaymentService() {
        return brainTreePaymentService;
    }

    public void setBrainTreePaymentService(BrainTreePaymentService brainTreePaymentService) {
        this.brainTreePaymentService = brainTreePaymentService;
    }

    public CommonI18NService getCommonI18NService() {
        return commonI18NService;
    }

    public void setCommonI18NService(CommonI18NService commonI18NService) {
        this.commonI18NService = commonI18NService;
    }

    public PaymentService getPaymentService() {
        return paymentService;
    }

    public void setPaymentService(PaymentService paymentService) {
        this.paymentService = paymentService;
    }

    public ModelService getModelService() {
        return modelService;
    }

    public void setModelService(ModelService modelService) {
        this.modelService = modelService;
    }

    public ReturnService getReturnService() {
        return returnService;
    }

    public void setReturnService(ReturnService returnService) {
        this.returnService = returnService;
    }
}
