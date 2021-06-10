package com.braintree.customersupportbackoffice.facade;

import com.braintree.command.result.BrainTreePaymentMethodResult;
import com.braintree.customersupportbackoffice.data.BrainTreeTransactionInfo;
import com.braintree.hybris.data.BrainTreeResponseResultData;
import com.braintree.hybris.data.BraintreeTransactionEntryData;
import com.braintree.model.BrainTreeTransactionDetailModel;
import com.braintree.model.BraintreeCustomerDetailsModel;
import com.braintree.payment.dto.BraintreeInfo;
import com.hybris.backoffice.widgets.advancedsearch.impl.AdvancedSearchData;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.payment.AdapterException;
import de.hybris.platform.payment.commands.result.SubscriptionResult;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

public interface BrainTreeCustomerSupportFacade {
    BraintreeInfo getBraintreeInfo();

    SubscriptionResult createCustomer(BrainTreeTransactionInfo transactionInfo);

    BraintreeCustomerDetailsModel findCustomer(String customerId) throws AdapterException;

    List<BraintreeCustomerDetailsModel> findCustomers(final AdvancedSearchData searchData) throws AdapterException;

    List<BrainTreeTransactionDetailModel> findTransactions(final AdvancedSearchData searchData) throws AdapterException;

    BraintreeTransactionEntryData findTransaction(String transactionId);

    PaymentTransactionEntryModel authorizePayment(final AbstractOrderModel cart, Map<String, String> customFields, BigDecimal totalAmount);

    BrainTreeResponseResultData refundTransaction(final BrainTreeTransactionDetailModel currentTransaction,
                                                  final String amount);

    BrainTreeResponseResultData voidTransaction(final BrainTreeTransactionDetailModel transaction);

    BrainTreeResponseResultData cloneTransaction(BrainTreeTransactionDetailModel currentTransaction, String amount,
                                                 boolean submitForSettlement);

    @SuppressWarnings("Duplicates")
    BrainTreeResponseResultData removeCustomer(String customerId);

    BrainTreeResponseResultData createTransaction(BrainTreeTransactionInfo brainTreeInfo);

    BrainTreePaymentMethodResult createCreditCardPaymentMethod(final String customerId, final String paymentMethodNonce,
                                                               final String cardholderName, final boolean isDefault, String billingAddressId);
}
