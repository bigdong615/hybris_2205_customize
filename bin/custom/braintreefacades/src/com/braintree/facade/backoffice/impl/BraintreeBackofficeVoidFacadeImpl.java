package com.braintree.facade.backoffice.impl;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;

import com.braintree.command.request.BrainTreeDeletePaymentMethodRequest;
import com.braintree.command.result.BrainTreePaymentMethodResult;
import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.hybris.data.BrainTreeResponseResultData;
import com.braintree.payment.info.service.PaymentInfoService;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.payment.dto.TransactionStatusDetails;
import de.hybris.platform.servicelayer.user.UserService;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import com.braintree.command.result.BrainTreeVoidResult;
import com.braintree.constants.BraintreeConstants;
import com.braintree.exceptions.BraintreeErrorException;
import com.braintree.facade.backoffice.BraintreeBackofficeVoidFacade;
import com.braintree.method.BrainTreePaymentService;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintreegateway.Transaction;

import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.payment.commands.request.VoidRequest;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.enums.PaymentTransactionType;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.servicelayer.model.ModelService;


public class BraintreeBackofficeVoidFacadeImpl implements BraintreeBackofficeVoidFacade {
    private final static Logger LOG = Logger.getLogger(BraintreeBackofficeVoidFacadeImpl.class);

    @Autowired
    private BrainTreePaymentService brainTreePaymentService;
    @Autowired
    private ModelService modelService;
    @Autowired
    private UserService userService;
    @Autowired
    PaymentInfoService paymentInfoService;
    @Autowired
    private BrainTreeConfigService brainTreeConfigService;

    @Override
    public boolean isVoidPossible(final OrderModel order) {
        if (null == order) {
            LOG.error("order: " + order);
            return false;
        }
        LOG.info("isVoidPossible, order.getTotalPrice: " + order.getTotalPrice());

        modelService.refresh(order);
        boolean isVoidPossible = false;
        Boolean settlementConfigParameter = brainTreeConfigService.getSettlementConfigParameter();

        if (order != null && order.getPaymentInfo() instanceof BrainTreePaymentInfoModel) {
            final String paymentProvider = ((BrainTreePaymentInfoModel) order.getPaymentInfo()).getPaymentProvider();
            final String intent = ((BrainTreePaymentInfoModel) order.getPaymentInfo()).getPayPalIntent();

            if (paidByCard(order))
            {
                if (!settlementConfigParameter && !isOrderVoided(order) && !isOrderAmountCaptured(order))
                {
                    isVoidPossible = true;
                }
            }
           else if (!isOrderVoided(order) && !isOrderAmountCaptured(order)) {
                if (BraintreeConstants.BRAINTREE_CREDITCARD_PAYMENT.equals(paymentProvider) || BraintreeConstants.APPLE_PAY_PAYMENT.equals(paymentProvider)) {
                    isVoidPossible = false;
                } else if (BraintreeConstants.PAYPAL_INTENT_SALE.equalsIgnoreCase(intent)) {
                    isVoidPossible = false;
                } else if (BraintreeConstants.PAYPAL_INTENT_AUTHORIZE.equalsIgnoreCase(intent)) {
                    isVoidPossible = isVoidAuthorizationPossible(order);
                } else if (BraintreeConstants.PAYPAL_INTENT_ORDER.equalsIgnoreCase(intent)) {
                    isVoidPossible = isVoidAuthorizationPossible(order);
                } else {
                    LOG.error("Order was placed with incorrect intent = '" + intent);
                    isVoidPossible = false;
                }
            }
        }
        return isVoidPossible;
    }

    @Override
    public List<PaymentTransactionEntryModel> getVoidableTransactions(final OrderModel order) {
        List<PaymentTransactionEntryModel> result = new ArrayList<>();

        for (PaymentTransactionModel transaction : order.getPaymentTransactions()) {
            for (PaymentTransactionEntryModel paymentEntry : transaction.getEntries()) {
                if (TransactionStatus.ACCEPTED.name().equals(paymentEntry.getTransactionStatus()) &&
                        (PaymentTransactionType.AUTHORIZATION.equals(paymentEntry.getType()) || PaymentTransactionType.ORDER.equals(paymentEntry.getType()))) {
                    result.add(paymentEntry);
                }
            }
        }

        return result;
    }

    @Override
    public void executeVoid(final PaymentTransactionEntryModel transaction) throws BraintreeErrorException {
        if(transaction.getType().equals(PaymentTransactionType.ORDER)){
            BrainTreeResponseResultData resultData = deletePaymentMethod((BrainTreePaymentInfoModel) transaction.getPaymentTransaction().getInfo(),transaction);
            if (resultData.isSuccess()) {
                transaction.setTransactionStatus(Transaction.Status.VOIDED.name());
                modelService.save(transaction);
            } else {
                LOG.error("Error, message: " + resultData.getErrorMessage());
                throw new BraintreeErrorException(resultData.getErrorMessage(), resultData.getTransactionId());
            }
        }else{
            final VoidRequest voidRequest = new VoidRequest("-not-used-", transaction.getRequestId(), StringUtils.EMPTY, StringUtils.EMPTY);
            final BrainTreeVoidResult voidResult = brainTreePaymentService.voidTransaction(voidRequest);
            if (voidResult.isSuccess()) {
                transaction.setTransactionStatus(Transaction.Status.VOIDED.name());
                modelService.save(transaction);
            } else {
                LOG.error("Error, message: " + voidResult.getErrorMessage());
                throw new BraintreeErrorException(voidResult.getErrorMessage(), voidResult.getTransactionId());
            }

        }
    }

    public boolean isVoidAuthorizationPossible(final OrderModel order) {
        if (order != null) {
            return isSuccessfulTransactionPresent(order, PaymentTransactionType.AUTHORIZATION) || isSuccessfulTransactionPresent(order, PaymentTransactionType.ORDER);
        }
        return false;
    }

    private boolean isSuccessfulTransactionPresent(final OrderModel order, final PaymentTransactionType transactionType) {
        for (PaymentTransactionModel paymentTransaction : order.getPaymentTransactions()) {
            for (PaymentTransactionEntryModel transactionEntry : paymentTransaction.getEntries()) {
                if (transactionType.equals(transactionEntry.getType()) && transactionEntry.getTransactionStatusDetails().startsWith("SUCCESFULL")
                        && TransactionStatus.ACCEPTED.name().equals(transactionEntry.getTransactionStatus())) {
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    public BrainTreePaymentMethodResult deletePaymentMethod(final BrainTreePaymentInfoModel paymentInfo)
    {
        final UserModel user = userService.getCurrentUser();
        final String braintreeCustomerId = user.getUid();
        if (braintreeCustomerId != null)
        {
            final BrainTreeDeletePaymentMethodRequest request = new BrainTreeDeletePaymentMethodRequest(braintreeCustomerId,
                    paymentInfo.getPaymentMethodToken());
            final BrainTreePaymentMethodResult result = brainTreePaymentService.deletePaymentMethod(request);
            if (result.isSuccess())
            {
                paymentInfoService.remove(paymentInfo.getCustomerId(), paymentInfo.getPaymentMethodToken());
            }
            return result;
        }
        return null;
    }

    @Override
    public BrainTreeResponseResultData deletePaymentMethod(BrainTreePaymentInfoModel paymentInfoModel, PaymentTransactionEntryModel paymentTransactionEntryModel) {
        BrainTreePaymentMethodResult brainTreePaymentMethodResult = deletePaymentMethod(paymentInfoModel);
        BrainTreeResponseResultData brainTreeResponseResultData = new BrainTreeResponseResultData();
        brainTreeResponseResultData.setErrorCode(brainTreePaymentMethodResult.getErrorCode());
        brainTreeResponseResultData.setErrorMessage(brainTreePaymentMethodResult.getErrorMessage());
        brainTreeResponseResultData.setSuccess(brainTreePaymentMethodResult.isSuccess());
        paymentTransactionEntryModel.setTransactionStatus(Transaction.Status.VOIDED.name());
        paymentTransactionEntryModel.setTransactionStatusDetails(TransactionStatusDetails.REVIEW_NEEDED.name());
        paymentTransactionEntryModel.getPaymentTransaction().getEntries().get(0).setTransactionStatus(Transaction.Status.VOIDED.name());
        paymentTransactionEntryModel.getPaymentTransaction().getEntries().get(0).setTransactionStatusDetails(TransactionStatusDetails.REVIEW_NEEDED.name());
        modelService.saveAll(paymentTransactionEntryModel);
        return brainTreeResponseResultData;
    }

    private boolean isOrderAmountCaptured(final  OrderModel order){
        Predicate<PaymentTransactionEntryModel> filterEntries = entry -> (PaymentTransactionType.CAPTURE.equals(entry.getType()) ||
                PaymentTransactionType.PARTIAL_CAPTURE.equals(entry.getType()));

        double capturedSum = order.getPaymentTransactions().stream()
                .flatMap(transaction -> transaction.getEntries().stream().filter(filterEntries))
                .mapToDouble(entry -> entry.getAmount().doubleValue()).sum();

        return capturedSum >= order.getTotalPrice();
    }

    private boolean isOrderVoided(final OrderModel order){
        boolean isOrderVoided =false;
        PaymentTransactionEntryModel firstEntry = order.getPaymentTransactions().stream().flatMap(transaction -> transaction.getEntries()
                .stream()).findFirst().orElse(null);
        if (firstEntry!=null){
            if ( Transaction.Status.VOIDED.name().equals(firstEntry.getTransactionStatus())){
                isOrderVoided = true;
            }
        }
        return isOrderVoided;
    }

    private boolean paidByCard(final OrderModel order)
    {
        boolean paidByCard = false;
        Object cardType = order.getPaymentInfo().getProperty("cardType");
        if (cardType != null)
        {
            paidByCard = true;
        }
        return paidByCard;
    }

}
