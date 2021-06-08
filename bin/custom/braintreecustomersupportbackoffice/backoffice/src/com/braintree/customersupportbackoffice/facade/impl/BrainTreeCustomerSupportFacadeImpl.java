package com.braintree.customersupportbackoffice.facade.impl;

import com.braintree.command.request.*;
import com.braintree.command.result.*;
import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.converters.BraintreeCustomerDetailsConverter;
import com.braintree.converters.BraintreePaymentMethodConverter;
import com.braintree.converters.BraintreeTransactionConverter;
import com.braintree.converters.BraintreeTransactionDetailConverter;
import com.braintree.customersupportbackoffice.commands.request.BrainTreeCustomerBackofficeRequest;
import com.braintree.customersupportbackoffice.commands.request.BrainTreeFindTransactionBackofficeRequest;
import com.braintree.customersupportbackoffice.converters.BrainTreeCustomerSupportResponseConverter;
import com.braintree.customersupportbackoffice.data.BrainTreeTransactionInfo;
import com.braintree.customersupportbackoffice.facade.BrainTreeCustomerSupportFacade;
import com.braintree.customersupportbackoffice.services.CustomerSearchService;
import com.braintree.customersupportbackoffice.services.TransactionSearchService;
import com.braintree.hybris.data.BrainTreeResponseResultData;
import com.braintree.hybris.data.BraintreeTransactionData;
import com.braintree.hybris.data.BraintreeTransactionEntryData;
import com.braintree.method.BrainTreePaymentService;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.model.BrainTreeTransactionDetailModel;
import com.braintree.model.BraintreeCustomerDetailsModel;
import com.braintree.payment.dto.BraintreeInfo;
import com.braintree.payment.info.service.PaymentInfoService;
import com.braintree.transaction.service.BrainTreeTransactionService;
import com.braintreegateway.PaymentMethodRequest;
import com.hybris.backoffice.widgets.advancedsearch.impl.AdvancedSearchData;
import com.hybris.backoffice.widgets.advancedsearch.impl.SearchConditionData;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.payment.AdapterException;
import de.hybris.platform.payment.commands.request.CreateSubscriptionRequest;
import de.hybris.platform.payment.commands.request.VoidRequest;
import de.hybris.platform.payment.commands.result.SubscriptionResult;
import de.hybris.platform.payment.dto.BillingInfo;
import de.hybris.platform.payment.dto.CardInfo;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.servicelayer.user.UserService;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Required;
import org.zkoss.zul.Messagebox;

import java.math.BigDecimal;
import java.util.*;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;

public class BrainTreeCustomerSupportFacadeImpl implements BrainTreeCustomerSupportFacade {
    private static final String BRAINTREE_PAYMENT_INFO_SESSION_ATTRIBUTE = "BRAINTREE_PAYMENT_INFO";

    private static final Logger LOG = Logger.getLogger(BrainTreeCustomerSupportFacadeImpl.class);

    public static final Integer SEARCH_LIMIT = Integer.valueOf(300);

    private ModelService modelService;
    private UserService userService;
    private CustomerSearchService customerSearchService;
    private TransactionSearchService transactionSearchService;
    private BrainTreePaymentService brainTreePaymentService;
    private SessionService sessionService;
    private BrainTreeTransactionService brainTreeTransactionService;
    private PaymentInfoService paymentInfoService;
    private BrainTreeConfigService brainTreeConfigService;

    private BraintreeCustomerDetailsConverter customerDetailsPopulator;
    private BraintreeTransactionConverter transactionSearchResultConverter;
    private BraintreeTransactionDetailConverter transactionDetailPopulator;
    private BrainTreeCustomerSupportResponseConverter brainTreeCustomerSupportResponseConverter;
    private BraintreePaymentMethodConverter paymentMethodConverter;


    @Override
    public BraintreeInfo getBraintreeInfo() {
        final BraintreeInfo braintreeInfo = sessionService.getAttribute(BRAINTREE_PAYMENT_INFO_SESSION_ATTRIBUTE);

        if (braintreeInfo == null) {
            throw new IllegalStateException("Braintree information: nonce, device data etc, should be provided");
        }

        return braintreeInfo;
    }

    @Override
    public SubscriptionResult createCustomer(BrainTreeTransactionInfo transactionInfo) {
        BillingInfo billingInfo = new BillingInfo();
        billingInfo.setFirstName(transactionInfo.getFirstName());
        billingInfo.setLastName(transactionInfo.getLastName());
        billingInfo.setEmail(transactionInfo.getEmail());
        billingInfo.setPostalCode(transactionInfo.getBillingPostCode());
        billingInfo.setStreet1(transactionInfo.getBillingAddress());
        return brainTreePaymentService.createCustomerSubscription(new CreateSubscriptionRequest(null, billingInfo, null, null, null, null, null));
    }

    @Override
    public BraintreeCustomerDetailsModel findCustomer(final String customerId) throws AdapterException {
        final UserModel user = userService.getCurrentUser();
        final String braintreeCustomerId = user.getUid();
        if (braintreeCustomerId != null) {
            final BrainTreeCustomerBackofficeRequest findCustomerRequest = new BrainTreeCustomerBackofficeRequest(braintreeCustomerId);
            findCustomerRequest.setCustomerId(customerId);

            final BrainTreeFindCustomersResult customers = customerSearchService.findCustomers(findCustomerRequest);
            if (customers.getCustomers() != null) {
                if (customers.getCustomers().iterator().hasNext()) {
                    return customerDetailsPopulator.convert(customers.getCustomers().getFirst());
                }
            }
        }
        LOG.error("[BT Payment Service] Error user must be Customer type!");
        return null;
    }

    @Override
    public List<BraintreeCustomerDetailsModel> findCustomers(AdvancedSearchData searchData) throws AdapterException {
        final UserModel user = userService.getCurrentUser();
        final String braintreeCustomerId = user.getUid();
        if (braintreeCustomerId != null) {
            final BrainTreeCustomerBackofficeRequest findCustomerRequest = new BrainTreeCustomerBackofficeRequest(braintreeCustomerId);

            List<SearchConditionData> customerId = searchData.getConditions("braintreeTransactionCustomerID");
            if (customerId != null && !customerId.isEmpty()) {
                addCustomerIdCondition(findCustomerRequest, customerId.get(0));
            }

            List<SearchConditionData> customerEmail = searchData.getConditions("braintreeTransactionCustomerEmail");
            if (customerEmail != null && !customerEmail.isEmpty()) {
                addCustomerEmailCondition(findCustomerRequest, customerEmail.get(0));
            }

            final BrainTreeFindCustomersResult customers = customerSearchService.findCustomers(findCustomerRequest);

            if (customers.getCustomers().getMaximumSize() > SEARCH_LIMIT)
            {
                LOG.error("[BT Customer search]Too many results(" +
                        customers.getCustomers().getMaximumSize() +
                        ")! Limit is " + SEARCH_LIMIT +
                        " Please type customer id or customer email.");

                Messagebox.show("Too many search results (" + customers.getCustomers().getMaximumSize() +
                                ")! Limit is " + SEARCH_LIMIT + ". Please enter at least customer id or customer email.",
                        "Customer search", Messagebox.OK, Messagebox.ERROR);

                return Collections.emptyList();
            }

            final List<BraintreeCustomerDetailsModel> customerList = new ArrayList<>(customers.getCustomers().getMaximumSize());
            customers.getCustomers().forEach(customer -> customerList.add(customerDetailsPopulator.convert(customer)));
            return customerList;
        }
        return null;
    }

    private void addCustomerIdCondition(final BrainTreeCustomerBackofficeRequest findCustomerRequest, final SearchConditionData searchConditionData) {
        if (searchConditionData != null && searchConditionData.getValue() != null) {
            findCustomerRequest.setCustomerId(searchConditionData.getValue().toString());
        }
    }

    private void addCustomerEmailCondition(final BrainTreeCustomerBackofficeRequest findCustomerRequest, final SearchConditionData searchConditionData) {
        if (searchConditionData != null && searchConditionData.getValue() != null) {
            findCustomerRequest.setCustomerEmail(searchConditionData.getValue().toString());
            findCustomerRequest.setCustomerEmailOperator(searchConditionData.getOperator());
        }
    }

    @Override
    public List<BrainTreeTransactionDetailModel> findTransactions(AdvancedSearchData searchData) throws AdapterException {
        final UserModel user = userService.getCurrentUser();
        final String braintreeCustomerId = user.getUid();
        if (braintreeCustomerId != null) {
            final BrainTreeFindTransactionBackofficeRequest findTransactionRequest = new BrainTreeFindTransactionBackofficeRequest(braintreeCustomerId);

            List<SearchConditionData> customerId = searchData.getConditions("braintreeTransactionCustomerID");
            if (customerId != null && !customerId.isEmpty()) {
                addTransactionCustomerIdCondition(findTransactionRequest, customerId.get(0));
            }

            List<SearchConditionData> customerEmail = searchData.getConditions("braintreeTransactionCustomerEmail");
            if (customerEmail != null && !customerEmail.isEmpty()) {
                addTransactionCustomerEmailCondition(findTransactionRequest, customerEmail.get(0));
            }

            List<SearchConditionData> transactionId = searchData.getConditions("braintreeTransactionDetailID");
            if (transactionId != null && !transactionId.isEmpty()) {
                addTransactionIdCondition(findTransactionRequest, transactionId.get(0));
            }

            List<SearchConditionData> transactionStatus = searchData.getConditions("braintreeTransactionDetailStatus");
            if (transactionStatus != null && !transactionStatus.isEmpty()) {
                addTransactionStatusCondition(findTransactionRequest, transactionStatus.get(0));
            }

            List<SearchConditionData> transactionRiskDecision = searchData.getConditions("braintreeTransactionDetailRiskDecision");
            if (transactionRiskDecision != null && !transactionRiskDecision.isEmpty()) {
                addTransactionRiskDecisionCondition(findTransactionRequest, transactionRiskDecision.get(0));
            }

            List<SearchConditionData> transactionDateFrom = searchData.getConditions("braintreeTransactionDetailDateFrom");
            List<SearchConditionData> transactionDateTo = searchData.getConditions("braintreeTransactionDetailDateTo");

            SearchConditionData dateFrom = null, dateTo = null;
            if (transactionDateFrom != null && !transactionDateFrom.isEmpty()) {
                dateFrom = transactionDateFrom.get(0);
            }

            if (transactionDateTo != null && !transactionDateTo.isEmpty()) {
                dateTo = transactionDateTo.get(0);
            }

            try {
                addTransactionDateRange(findTransactionRequest, dateFrom, dateTo);
            } catch (IllegalArgumentException e) {
                Messagebox.show(e.getMessage(), "Transaction search", Messagebox.OK, Messagebox.ERROR);
                return Collections.emptyList();
            }


            final BrainTreeFindTransactionResult transactions = transactionSearchService.findTransactions(findTransactionRequest);

            if (transactions.getTransactions().getMaximumSize() > SEARCH_LIMIT)
            {
                LOG.error("[BT Transaction search]Too many results(" +
                        transactions.getTransactions().getMaximumSize() +
                        ")! Limit is " + SEARCH_LIMIT +
                        " Please type transaction id or customer email.");

                Messagebox.show("Too many search results (" + transactions.getTransactions().getMaximumSize() +
                                ")! Limit is " + SEARCH_LIMIT + ". Please enter at least transaction id or customer email.",
                        "Transaction search", Messagebox.OK, Messagebox.ERROR);

                return Collections.emptyList();
            }

            final List<BrainTreeTransactionDetailModel> transactionList = new ArrayList<>(transactions.getTransactions().getMaximumSize());
            BraintreeTransactionData transactionData = transactionSearchResultConverter.convert(transactions);
            transactionData.getTransactionEntries().forEach(transactionEntry -> transactionList.add(transactionDetailPopulator.convert(transactionEntry)));


            return filterTransactionsAfter(transactionList, searchData);
        }
        return null;
    }

    private List<BrainTreeTransactionDetailModel> filterTransactionsAfter(List<BrainTreeTransactionDetailModel> transactionList, AdvancedSearchData searchData)
    {
        List<BrainTreeTransactionDetailModel> result = new ArrayList<>();
        for(BrainTreeTransactionDetailModel brainTreeTransactionDetailModel : transactionList) {
            boolean addToResults = true;
            List<SearchConditionData> transactionRiskDecision = searchData.getConditions("braintreeTransactionDetailRiskDecision");
            if (transactionRiskDecision != null && !transactionRiskDecision.isEmpty()) {
                SearchConditionData searchConditionData = transactionRiskDecision.get(0);
                if (searchConditionData != null && searchConditionData.getValue() != null) {
                    if (!StringUtils.equalsIgnoreCase(searchConditionData.getValue().toString(),
                            brainTreeTransactionDetailModel.getRiskDecision())) {
                        addToResults = false;
                    }
                }
            }

            if (addToResults)
                result.add(brainTreeTransactionDetailModel);
        }
        return result;
    }

    @Override
    public BraintreeTransactionEntryData findTransaction(String transactionId) {
        final String merchantTransactionCode = getMerchantCode();
        if (merchantTransactionCode != null)
        {
            final BrainTreeFindTransactionBackofficeRequest findTransactionRequest = new BrainTreeFindTransactionBackofficeRequest(
                    merchantTransactionCode);

            findTransactionRequest.setTransactionId(transactionId);

            final BrainTreeFindTransactionResult transactions = transactionSearchService.findTransactions(findTransactionRequest);

            final BraintreeTransactionData convert = transactionSearchResultConverter.convert(transactions);

            List<BraintreeTransactionEntryData> brainTreeTransactions = convert.getTransactionEntries();

            if (CollectionUtils.isNotEmpty(brainTreeTransactions))
            {
                return convert.getTransactionEntries().get(0);
            }
        }

        return null;
    }

    private void addTransactionCustomerIdCondition(BrainTreeFindTransactionBackofficeRequest findTransactionRequest, SearchConditionData searchConditionData) {
        if (searchConditionData != null && searchConditionData.getValue() != null) {
            findTransactionRequest.setCustomerId(searchConditionData.getValue().toString());
        }
    }

    private void addTransactionCustomerEmailCondition(BrainTreeFindTransactionBackofficeRequest findTransactionRequest,
                                                      SearchConditionData searchConditionData) {
        if (searchConditionData != null && searchConditionData.getValue() != null) {
            findTransactionRequest.setCustomerEmail(searchConditionData.getValue().toString());
            findTransactionRequest.setCustomerEmailOperator(searchConditionData.getOperator());
        }
    }

    private void addTransactionIdCondition(BrainTreeFindTransactionBackofficeRequest findTransactionRequest, SearchConditionData searchConditionData) {
        if (searchConditionData != null && searchConditionData.getValue() != null) {
            findTransactionRequest.setTransactionId(searchConditionData.getValue().toString());
        }
    }

    private void addTransactionStatusCondition(BrainTreeFindTransactionBackofficeRequest findTransactionRequest, SearchConditionData searchConditionData) {
        if (searchConditionData != null && searchConditionData.getValue() != null) {
            findTransactionRequest.setTransactionStatus(searchConditionData.getValue().toString());
        }
    }

    private void addTransactionRiskDecisionCondition(BrainTreeFindTransactionBackofficeRequest findTransactionRequest, SearchConditionData searchConditionData) {
        if (searchConditionData != null && searchConditionData.getValue() != null) {
            findTransactionRequest.setRiskDecision(searchConditionData.getValue().toString());
        }
    }

    private void addTransactionDateRange(BrainTreeFindTransactionBackofficeRequest findTransactionRequest, SearchConditionData dateFrom, SearchConditionData dateTo) {
        Calendar start = Calendar.getInstance();
        Calendar end = Calendar.getInstance();
        start.add(Calendar.DAY_OF_MONTH, -1);

        if (dateFrom != null && dateFrom.getValue() != null) {
            start.setTime((Date) dateFrom.getValue());
        }
        if (dateTo != null && dateTo.getValue() != null) {
            end.setTime((Date) dateTo.getValue());
        }

        validateDateRange(start, end);

        findTransactionRequest.setStartDate(start);
        findTransactionRequest.setEndDate(end);
    }

    private void validateDateRange(Calendar start, Calendar end) {
        if (start.after(end))
        {
            throw new IllegalArgumentException("From date should less than To date.");
        }
    }

    @Override
    public PaymentTransactionEntryModel authorizePayment(final AbstractOrderModel cart, Map<String, String> customFields, BigDecimal totalAmount)
    {
        return getBrainTreeTransactionService().createAuthorizationTransaction(cart, customFields, totalAmount);
    }

    @Override
    public BrainTreeResponseResultData refundTransaction(final BrainTreeTransactionDetailModel currentTransaction,
                                                         final String amount)
    {
        validateParameterNotNullStandardMessage("currentTransaction", currentTransaction);
        final String merchantTransactionCode = getMerchantCode();
        if (merchantTransactionCode != null)
        {
            final BrainTreeRefundTransactionRequest request = new BrainTreeRefundTransactionRequest(merchantTransactionCode);
            request.setTransactionId(currentTransaction.getId());
            if (StringUtils.isNotBlank(amount))
            {
                request.setAmount(formedAmount(amount));
            }
            final BrainTreeRefundTransactionResult result = brainTreePaymentService.refundTransaction(request);
            if (TransactionStatus.ACCEPTED.equals(result.getTransactionStatus()))
            {
                createRefundTransaction(currentTransaction, result);
            }
            return brainTreeCustomerSupportResponseConverter.convert(result);
        }
        return new BrainTreeResponseResultData();
    }

    private String getMerchantCode()
    {
        final UserModel user = userService.getCurrentUser();
        return user.getUid();
    }

    private BigDecimal formedAmount(final String amount)
    {
        return new BigDecimal(amount);
    }

    private void createRefundTransaction(final BrainTreeTransactionDetailModel currentTransaction,
                                         final BrainTreeRefundTransactionResult result)
    {
        final OrderModel linkedOrder = currentTransaction.getLinkedOrder();
        if (linkedOrder != null)
        {
            final List<PaymentTransactionModel> paymentTransactions = linkedOrder.getPaymentTransactions();
            if (CollectionUtils.isNotEmpty(paymentTransactions))
            {
                final PaymentTransactionModel transaction = paymentTransactions.iterator().next();
                getBrainTreeTransactionService().createRefundTransaction(transaction, result);
            }
        }
    }

    @Override
    public BrainTreeResponseResultData voidTransaction(final BrainTreeTransactionDetailModel transaction)
    {
        validateParameterNotNullStandardMessage("transaction", transaction);
        final String merchantTransactionCode = getMerchantCode();
        if (merchantTransactionCode != null)
        {
            final VoidRequest voidRequest = new VoidRequest(merchantTransactionCode, transaction.getId(), StringUtils.EMPTY,
                    StringUtils.EMPTY);
            final BrainTreeVoidResult voidResult = brainTreePaymentService.voidTransaction(voidRequest);
            if (TransactionStatus.ACCEPTED.equals(voidResult.getTransactionStatus()))
            {
                forceOrderCancellation(transaction, voidResult);
            }
            return brainTreeCustomerSupportResponseConverter.convert(voidResult);
        }
        return new BrainTreeResponseResultData();
    }

    private void forceOrderCancellation(final BrainTreeTransactionDetailModel transaction, final BrainTreeVoidResult voidResult)
    {
        final OrderModel linkedOrder = transaction.getLinkedOrder();
        if (linkedOrder != null)
        {
            final List<PaymentTransactionModel> paymentTransactions = linkedOrder.getPaymentTransactions();
            if (CollectionUtils.isNotEmpty(paymentTransactions))
            {
                final PaymentTransactionModel txn = paymentTransactions.iterator().next();
                getBrainTreeTransactionService().createCancelTransaction(txn, voidResult);
            }
            linkedOrder.setStatus(OrderStatus.CANCELLED);
            modelService.save(linkedOrder);
        }
    }

    @Override
    public BrainTreeResponseResultData cloneTransaction(BrainTreeTransactionDetailModel currentTransaction, String amount, boolean submitForSettlement)
    {
        validateParameterNotNullStandardMessage("currentTransaction", currentTransaction);
        final String merchantTransactionCode = getMerchantCode();
        if (merchantTransactionCode != null)
        {
            final BrainTreeCloneTransactionRequest request = new BrainTreeCloneTransactionRequest(merchantTransactionCode);
            request.setTransactionId(currentTransaction.getId());
            if (StringUtils.isNotBlank(amount))
            {
                request.setAmount(formedAmount(amount));
            }
            request.setSubmitForSettlement(Boolean.valueOf(submitForSettlement));
            final BrainTreeCloneTransactionResult result = brainTreePaymentService.cloneTransaction(request);
            return brainTreeCustomerSupportResponseConverter.convert(result);
        }
        return new BrainTreeResponseResultData();
    }

    @Override
    @SuppressWarnings("Duplicates")
    public BrainTreePaymentMethodResult createCreditCardPaymentMethod(final String customerId, final String paymentMethodNonce,
                                                                      final String cardholderName, final boolean isDefault, String billingAddressId) {
        final UserModel user = userService.getCurrentUser();
        final String braintreeCustomerId = user.getUid();
        if (braintreeCustomerId != null) {
            final BrainTreeCreateCreditCardPaymentMethodRequest request = new BrainTreeCreateCreditCardPaymentMethodRequest(
                    braintreeCustomerId);
            final PaymentMethodRequest paymentMethodRequest = new PaymentMethodRequest();

            paymentMethodRequest.paymentMethodNonce(paymentMethodNonce).customerId(customerId).cardholderName(cardholderName);
            paymentMethodRequest.options().makeDefault(isDefault).done();

            if (StringUtils.isNotBlank(billingAddressId)) {
                paymentMethodRequest.billingAddressId(billingAddressId);
            }
            request.setRequest(paymentMethodRequest);
            final BrainTreePaymentMethodResult result = brainTreePaymentService.createCreditCardPaymentMethod(request);
            if (result.isSuccess() && result.getPaymentMethod() != null) {
                final BrainTreePaymentInfoModel paymentInfo = paymentMethodConverter.convert(result.getPaymentMethod());
                if (paymentInfo != null) {
                    paymentInfo.setCardholderName(result.getCardholderName());
                    paymentInfoService.addToCustomer(paymentInfo);
                }
            }
            return result;
        }
        return null;
    }

    @Override
    @SuppressWarnings("Duplicates")
    public BrainTreeResponseResultData removeCustomer(final String customerId) {
        final String merchantId = getMerchantCode();
        BrainTreeResponseResultData resultData = new BrainTreeResponseResultData();
        if (merchantId != null) {
            validateParameterNotNullStandardMessage("customerId", customerId);
            final BrainTreeCustomerRequest brainTreeCustomerRequest = new BrainTreeCustomerRequest(merchantId);
            brainTreeCustomerRequest.setCustomerId(customerId);

            final BrainTreeCustomerResult result = brainTreePaymentService.removeCustomer(brainTreeCustomerRequest);

            resultData.setSuccess(result.isSuccess());
            resultData.setErrorMessage(result.getErrorMessage());
            resultData.setErrorCode(result.getErrorCode());
        }
        return resultData;
    }

    @Override
    public BrainTreeResponseResultData createTransaction(BrainTreeTransactionInfo brainTreeInfo) {
        validateParameterNotNullStandardMessage("brainTreeInfo", brainTreeInfo);
        final BrainTreeResponseResultData brainTreeResponseResultData = new BrainTreeResponseResultData();
        return getBraintreeResponseResultData(brainTreeInfo, brainTreeResponseResultData);
    }

    @SuppressWarnings("Duplicates")
    private BrainTreeResponseResultData getBraintreeResponseResultData(final BrainTreeTransactionInfo brainTreeInfo,
                                                                       final BrainTreeResponseResultData brainTreeResponseResultData) {
        final String merchantTransactionCode = getMerchantCode();
        if (merchantTransactionCode != null) {
            final BrainTreeSaleTransactionRequest request = new BrainTreeSaleTransactionRequest(merchantTransactionCode,
                    createCard(brainTreeInfo), brainTreeInfo.getCurrency(), brainTreeInfo.getAmount(),
                    createShippingInfo(brainTreeInfo));

            request.setUsePaymentMethodToken(brainTreeInfo.isPaymentMethodToken());
            request.setPaymentMethodToken(brainTreeInfo.getPaymentMethodToken());
            request.setTaxAmount(brainTreeInfo.getTax());
            request.setCustomFields(brainTreeInfo.getCustom());
            request.setCustomerEmail(brainTreeInfo.getEmail());
            request.setCustomerFirstName(brainTreeInfo.getFirstName());
            request.setCustomerLastName(brainTreeInfo.getLastName());
            request.setStoreInVault(brainTreeInfo.getStoreInVault());
            request.setMethodNonce(brainTreeInfo.getNonce());
            request.setCardholderName(brainTreeInfo.getCardHolder());

            request.setMerchantAccountIdForCurrentSite(brainTreeInfo.getMerchantAccountId());
            final BrainTreeSaleTransactionResult result = brainTreePaymentService.saleTransaction(request);
            return brainTreeCustomerSupportResponseConverter.convert(result);
        }
        return brainTreeResponseResultData;
    }

    @SuppressWarnings("Duplicates")
    private CardInfo createCard(final BrainTreeTransactionInfo transactionInfo) {
        final CardInfo cardInfo = new CardInfo();
        cardInfo.setCardHolderFullName(transactionInfo.getCardHolder());
        cardInfo.setCardNumber(transactionInfo.getCardNumber());
        cardInfo.setCv2Number(transactionInfo.getCvv());
        if (transactionInfo.getExpirationDate() != null) {
            final String[] mmyy = transactionInfo.getExpirationDate().split("/");
            if (mmyy.length == 2) {
                cardInfo.setExpirationMonth(Integer.valueOf(mmyy[0]));
                cardInfo.setExpirationYear(Integer.valueOf(mmyy[1]));
            }
        }
        final BillingInfo billingInfo = new BillingInfo();
        billingInfo.setFirstName(transactionInfo.getFirstName());
        billingInfo.setLastName(transactionInfo.getLastName());
        billingInfo.setEmail(transactionInfo.getEmail());
        billingInfo.setPostalCode(transactionInfo.getBillingPostCode());
        billingInfo.setStreet1(transactionInfo.getBillingAddress());
        cardInfo.setBillingInfo(billingInfo);
        return cardInfo;
    }

    @SuppressWarnings("Duplicates")
    private BillingInfo createShippingInfo(final BrainTreeTransactionInfo transactionInfo) {
        final BillingInfo shippingInfo = new BillingInfo();
        shippingInfo.setFirstName(transactionInfo.getFirstName());
        shippingInfo.setLastName(transactionInfo.getLastName());
        shippingInfo.setEmail(transactionInfo.getEmail());
        shippingInfo.setPostalCode(transactionInfo.getShippingPostCode());
        shippingInfo.setStreet1(transactionInfo.getShippingAddress());
        return shippingInfo;
    }

    @Required
    @SuppressWarnings("unused")
    public void setUserService(UserService userService) {
        this.userService = userService;
    }

    @Required
    @SuppressWarnings("unused")
    public void setCustomerSearchService(CustomerSearchService customerSearchService) {
        this.customerSearchService = customerSearchService;
    }

    @Required
    @SuppressWarnings("unused")
    public void setTransactionSearchService(TransactionSearchService transactionSearchService) {
        this.transactionSearchService = transactionSearchService;
    }

    @Required
    @SuppressWarnings("unused")
    public void setCustomerDetailsPopulator(BraintreeCustomerDetailsConverter customerDetailsPopulator) {
        this.customerDetailsPopulator = customerDetailsPopulator;
    }

    @Required
    @SuppressWarnings("unused")
    public void setTransactionSearchResultConverter(BraintreeTransactionConverter transactionSearchResultConverter) {
        this.transactionSearchResultConverter = transactionSearchResultConverter;
    }

    @Required
    @SuppressWarnings("unused")
    public void setTransactionDetailPopulator(BraintreeTransactionDetailConverter transactionDetailPopulator) {
        this.transactionDetailPopulator = transactionDetailPopulator;
    }

    @Required
    @SuppressWarnings("unused")
    public void setBrainTreePaymentService(BrainTreePaymentService brainTreePaymentService) {
        this.brainTreePaymentService = brainTreePaymentService;
    }

    @Required
    @SuppressWarnings("unused")
    public void setSessionService(SessionService sessionService) {
        this.sessionService = sessionService;
    }

    @Required
    @SuppressWarnings("unused")
    public BrainTreeTransactionService getBrainTreeTransactionService() {
        return brainTreeTransactionService;
    }

    @Required
    @SuppressWarnings("unused")
    public void setBrainTreeTransactionService(BrainTreeTransactionService brainTreeTransactionService) {
        this.brainTreeTransactionService = brainTreeTransactionService;
    }

    @Required
    @SuppressWarnings("unused")
    public void setBrainTreeCustomerSupportResponseConverter(BrainTreeCustomerSupportResponseConverter brainTreeCustomerSupportResponseConverter) {
        this.brainTreeCustomerSupportResponseConverter = brainTreeCustomerSupportResponseConverter;
    }

    @Required
    @SuppressWarnings("unused")
    public void setModelService(ModelService modelService) {
        this.modelService = modelService;
    }

    @Required
    @SuppressWarnings("unused")
    public void setPaymentMethodConverter(BraintreePaymentMethodConverter paymentMethodConverter) {
        this.paymentMethodConverter = paymentMethodConverter;
    }

    @Required
    @SuppressWarnings("unused")
    public void setPaymentInfoService(PaymentInfoService paymentInfoService) {
        this.paymentInfoService = paymentInfoService;
    }

    @Required
    @SuppressWarnings("unused")
    public void setBrainTreeConfigService(BrainTreeConfigService brainTreeConfigService) {
        this.brainTreeConfigService = brainTreeConfigService;
    }
}
