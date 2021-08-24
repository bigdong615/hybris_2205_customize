package com.braintree.facade.backoffice.impl;

import com.braintree.command.result.BrainTreeRefundTransactionResult;
import com.braintree.exceptions.BraintreeErrorException;
import com.braintree.facade.backoffice.BraintreeBackofficePartialRefundFacade;
import com.braintree.facade.backoffice.converter.BraintreeBackofficeResponseResultDataConverter;
import com.braintree.hybris.data.BrainTreeResponseResultData;
import com.braintree.order.refund.partial.services.BraintreePartialRefundService;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.enums.PaymentTransactionType;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.servicelayer.model.ModelService;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import java.math.BigDecimal;

public class BraintreeBackofficePartialRefundFacadeImpl implements BraintreeBackofficePartialRefundFacade {
    private final static Logger LOG = Logger.getLogger(BraintreeBackofficePartialRefundFacadeImpl.class);

    @Autowired
    private BraintreePartialRefundService partialRefundService;
    @Autowired
    @Qualifier("braintreeBackofficeResponseResultDataConverter")
    private BraintreeBackofficeResponseResultDataConverter braintreeBackofficeResponseResultDataConverter;
    @Autowired
    private ModelService modelService;

    @Override
    public boolean isPartialRefundPossible(final OrderModel order) {
        if (order == null){
            return false;
        }
        LOG.info("isPartialRefundPossible, order.getTotalPrice: " + order.getTotalPrice());

       // modelService.refresh(order);

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

    @Override
    public BrainTreeResponseResultData partialRefundTransaction(final OrderModel order,
                                                                final PaymentTransactionEntryModel paymentTransactionEntry, final BigDecimal amount) throws BraintreeErrorException
    {
        BrainTreeRefundTransactionResult response = partialRefundService.partialRefundTransaction(order,
                paymentTransactionEntry, amount);
        return braintreeBackofficeResponseResultDataConverter.convert(response);
    }
}
