package com.braintree.facade.backoffice.impl;

import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.constants.BraintreeConstants;
import com.braintree.facade.backoffice.BraintreeBackofficeAuthorizeFacade;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintreegateway.Transaction;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.enums.PaymentTransactionType;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.servicelayer.model.ModelService;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.DoubleStream;
import java.util.stream.Stream;

public class BraintreeBackofficeAuthorizeFacadeImpl implements BraintreeBackofficeAuthorizeFacade {

    private final static Logger LOG = Logger.getLogger(BraintreeBackofficeAuthorizeFacadeImpl.class);

    @Autowired
    private ModelService modelService;
    @Autowired
    private BrainTreeConfigService brainTreeConfigService;

    @Override
    public boolean isAuthorizePossible(final OrderModel order) {
        if (null == order) {
            LOG.error("order: " + order);
            return false;
        }
        LOG.info("isAuthorizePossible, order.getTotalPrice: " + order.getTotalPrice());

        //modelService.refresh(order);
        boolean isAuthorizePossible = false;

        String paymentProvider = ((BrainTreePaymentInfoModel) order.getPaymentInfo()).getPaymentProvider();

        if (!isOrderVoided(order) && !isOrderAmountCaptured(order) && !(paidByCard(order))) {
            isAuthorizePossible = isIntentOrder(order) && !isHostedFields(paymentProvider) && !isApplePay(paymentProvider);
        }

        return isAuthorizePossible;
    }

    private boolean isIntentOrder(final OrderModel order) {
        if (order.getPaymentInfo() instanceof BrainTreePaymentInfoModel) {
            String intent = ((BrainTreePaymentInfoModel) order.getPaymentInfo()).getPayPalIntent();
            return BraintreeConstants.PAYPAL_INTENT_ORDER.equalsIgnoreCase(intent);
        } else {
            return false;
        }
    }

    private boolean isApplePay(final String paymentProvider) {
        return BraintreeConstants.APPLE_PAY_PAYMENT.equals(paymentProvider);
    }

    private boolean isHostedFields(final String paymentProvider) {
        return BraintreeConstants.BRAINTREE_CREDITCARD_PAYMENT.equals(paymentProvider);
    }

    private boolean isOrderAmountCaptured(final OrderModel order){
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
