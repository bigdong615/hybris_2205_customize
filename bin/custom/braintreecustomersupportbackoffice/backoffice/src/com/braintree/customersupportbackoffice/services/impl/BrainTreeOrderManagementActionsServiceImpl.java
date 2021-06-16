package com.braintree.customersupportbackoffice.services.impl;

import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.constants.BraintreeConstants;
import com.braintree.customersupportbackoffice.services.BrainTreeOrderManagementActionsService;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.order.capture.partial.services.BraintreePartialCaptureService;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.enums.PaymentTransactionType;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.returns.ReturnService;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;

import java.math.BigDecimal;
import java.util.List;

@Deprecated
public class BrainTreeOrderManagementActionsServiceImpl implements BrainTreeOrderManagementActionsService {
    private BrainTreeConfigService brainTreeConfigService;
    private BraintreePartialCaptureService braintreePartialCaptureService;
    private ReturnService returnService;

    @Deprecated
    @Override
    public boolean isMultipleCapturePossible(final OrderModel order) {
        if (order != null && order.getPaymentInfo() instanceof BrainTreePaymentInfoModel) {
            return isSuccessfulTransactionPresent(order, PaymentTransactionType.AUTHORIZATION);
        }
        return false;
    }

    @Deprecated
    @Override
    public boolean isPartialRefundPossible(final OrderModel order) {
        if (order == null){
            return false;
        }
        BigDecimal capturedAmount = BigDecimal.ZERO;
        for (PaymentTransactionModel paymentTransaction : order.getPaymentTransactions()) {
            for (PaymentTransactionEntryModel transactionEntry : paymentTransaction.getEntries()) {
                if ("SUCCESFULL".equals(transactionEntry.getTransactionStatusDetails()) &&
                        TransactionStatus.ACCEPTED.name().equals(transactionEntry.getTransactionStatus())) {
                    PaymentTransactionType transactionType =  transactionEntry.getType();
                    if (PaymentTransactionType.CAPTURE.equals(transactionType) || PaymentTransactionType.PARTIAL_CAPTURE.equals(transactionType)){
                        capturedAmount = capturedAmount.add(transactionEntry.getAmount());
                    } else if (PaymentTransactionType.REFUND_PARTIAL.equals(transactionType)){
                        capturedAmount = capturedAmount.subtract(transactionEntry.getAmount());
                    }
                }
            }
        }
        return capturedAmount.compareTo(BigDecimal.ZERO) > 0;
    }

    @Deprecated
    @Override
    public boolean isMultiCaptureEnabled()
    {
        return brainTreeConfigService.getMultiCaptureEnabled();
    }

    @Deprecated
    @Override
    public boolean isRefundPossible(OrderModel order)
    {
        if (order != null && !returnService.getAllReturnableEntries(order).isEmpty())
        {
            if (OrderStatus.COMPLETED.equals(order.getStatus()))
            {
                return true;
            }
            if (isMultiCaptureEnabled())
            {
                boolean isRefundPossible = false;
                isRefundPossible = !isPartialCaptureTakePlace(order);
                return isRefundPossible && StringUtils.isBlank(order.getVersionID()) && !isAvailableOrderAuthorization(order);
            }
            return StringUtils.isBlank(order.getVersionID()) && !isAvailableOrderAuthorization(order);
        }
        return false;
    }

//     @Override
//    public boolean isIntentOrder(OrderModel order) {
//        if (order != null && order.getPaymentInfo() != null && order.getPaymentInfo() instanceof BrainTreePaymentInfoModel)
//        {
//            String intent = ((BrainTreePaymentInfoModel) order.getPaymentInfo()).getPayPalIntent();
//            if (BraintreeConstants.PAYPAL_INTENT_ORDER.equalsIgnoreCase(intent))
//            {
//                return true;
//            }
//        }
//        return false;
//    }

    @Deprecated
    @Override
    public boolean isAvailableOrderAuthorization(OrderModel order) {
        if (order != null && order.getPaymentInfo() != null && order.getPaymentInfo() instanceof BrainTreePaymentInfoModel) {
            String intent = ((BrainTreePaymentInfoModel) order.getPaymentInfo()).getPayPalIntent();
            if (BraintreeConstants.PAYPAL_INTENT_ORDER.equalsIgnoreCase(intent)) {
                return true;
            }
        }
        return false;
    }

    private boolean isNotAuthorizedOrder(OrderModel order) {
        return (order.getStatus() == null || OrderStatus.CREATED.equals(order.getStatus()) || OrderStatus.CHECKED_VALID.equals(order.getStatus())) && (order.getPaymentTransactions() == null || order.getPaymentTransactions().isEmpty());
    }

    private boolean isPartialCaptureTakePlace(OrderModel order)
    {
        List<PaymentTransactionModel> paymentTransactions = order.getPaymentTransactions();

        if (CollectionUtils.isNotEmpty(paymentTransactions))
        {
            PaymentTransactionModel next = paymentTransactions.iterator().next();
            List<PaymentTransactionEntryModel> paymentTransactionEntries = next.getEntries();
            for (PaymentTransactionEntryModel paymentTransactionEntry : paymentTransactionEntries)
            {
                if (PaymentTransactionType.PARTIAL_CAPTURE.equals(paymentTransactionEntry.getType()))
                {

                    return true;
                }
            }
        }
        return false;
    }

    @Deprecated
    @Override
    public boolean isReplacePossible(OrderModel order) {
        return order != null && StringUtils.isBlank(order.getVersionID()) && !isAvailableOrderAuthorization(order);
    }

    private BigDecimal getAmountAvailableForMultiCapture(OrderModel orderModel)
    {
        if (orderModel != null)
        {
            return getBraintreePartialCaptureService().getPossibleAmountForCapture(orderModel);
        }
        return BigDecimal.ZERO;
    }

    public BrainTreeConfigService getBrainTreeConfigService()
    {
        return brainTreeConfigService;
    }

    public void setBrainTreeConfigService(BrainTreeConfigService brainTreeConfigService)
    {
        this.brainTreeConfigService = brainTreeConfigService;
    }

    public BraintreePartialCaptureService getBraintreePartialCaptureService() {
        return braintreePartialCaptureService;
    }

    public void setBraintreePartialCaptureService(BraintreePartialCaptureService braintreePartialCaptureService) {
        this.braintreePartialCaptureService = braintreePartialCaptureService;
    }

    public ReturnService getReturnService() {
        return returnService;
    }

    public void setReturnService(ReturnService returnService) {
        this.returnService = returnService;
    }

    private boolean isSuccessfulTransactionPresent(final OrderModel order, PaymentTransactionType transactionType) {
        for (PaymentTransactionModel paymentTransaction : order.getPaymentTransactions()) {
            for (PaymentTransactionEntryModel transactionEntry : paymentTransaction.getEntries()) {
                if (transactionType.equals(transactionEntry.getType()) &&
                        "SUCCESFULL".equals(transactionEntry.getTransactionStatusDetails()) &&
                        TransactionStatus.ACCEPTED.name().equals(transactionEntry.getTransactionStatus())) {
                    return true;
                }
            }
        }
        return false;
    }

}
