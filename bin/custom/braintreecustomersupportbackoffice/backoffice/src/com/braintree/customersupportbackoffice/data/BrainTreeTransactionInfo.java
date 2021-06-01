package com.braintree.customersupportbackoffice.data;

import com.braintree.enums.StoreInVault;

import java.math.BigDecimal;
import java.util.Currency;
import java.util.HashMap;
import java.util.Map;


public class BrainTreeTransactionInfo {
    private final Map<String, String> custom = new HashMap<>();
    private BigDecimal amount;
    private BigDecimal tax;
    private String cardHolder;
    private String cardNumber;
    private String cvv;
    private String expirationDate;
    private String firstName;
    private String lastName;
    private String email;
    private String billingAddress;
    private String billingPostCode;
    private String shippingAddress;
    private String shippingPostCode;
    private Currency currency;
    private boolean isPaymentMethodToken;
    private String paymentMethodToken;
    private String nonce;
    private StoreInVault storeInVault;
    private String merchantAccountId;

    public BigDecimal getAmount() {
        return amount;
    }

    public BrainTreeTransactionInfo setAmount(final BigDecimal amount) {
        this.amount = amount;
        return this;
    }

    public BigDecimal getTax() {
        return tax;
    }

    public BrainTreeTransactionInfo setTax(final BigDecimal tax) {
        this.tax = tax;
        return this;
    }

    public Map<String, String> getCustom() {
        return custom;
    }

    public BrainTreeTransactionInfo setCustom(final String key, final String value) {
        this.custom.put(key, value);
        return this;
    }

    public String getCardHolder() {
        return cardHolder;
    }

    public BrainTreeTransactionInfo setCardHolder(final String cardHolder) {
        this.cardHolder = cardHolder;
        return this;
    }

    public String getCardNumber() {
        return cardNumber;
    }

    public BrainTreeTransactionInfo setCardNumber(final String cardNumber) {
        this.cardNumber = cardNumber;
        return this;
    }

    public String getCvv() {
        return cvv;
    }

    public BrainTreeTransactionInfo setCvv(final String cvv) {
        this.cvv = cvv;
        return this;
    }

    public String getExpirationDate() {
        return expirationDate;
    }

    public BrainTreeTransactionInfo setExpirationDate(final String expirationDate) {
        this.expirationDate = expirationDate;
        return this;
    }

    public String getFirstName() {
        return firstName;
    }

    public BrainTreeTransactionInfo setFirstName(final String firstName) {
        this.firstName = firstName;
        return this;
    }

    public String getLastName() {
        return lastName;
    }

    public BrainTreeTransactionInfo setLastName(final String lastName) {
        this.lastName = lastName;
        return this;
    }

    public String getEmail() {
        return email;
    }

    public BrainTreeTransactionInfo setEmail(final String email) {
        this.email = email;
        return this;
    }

    public String getBillingAddress() {
        return billingAddress;
    }

    public BrainTreeTransactionInfo setBillingAddress(final String billingAddress) {
        this.billingAddress = billingAddress;
        return this;
    }

    public String getBillingPostCode() {
        return billingPostCode;
    }

    public BrainTreeTransactionInfo setBillingPostCode(final String billingPostCode) {
        this.billingPostCode = billingPostCode;
        return this;
    }

    public String getShippingAddress() {
        return shippingAddress;
    }

    public BrainTreeTransactionInfo setShippingAddress(final String shippingAddress) {
        this.shippingAddress = shippingAddress;
        return this;
    }

    public String getShippingPostCode() {
        return shippingPostCode;
    }

    public BrainTreeTransactionInfo setShippingPostCode(final String shippingPostCode) {
        this.shippingPostCode = shippingPostCode;
        return this;
    }

    public Currency getCurrency() {
        return currency;
    }

    public BrainTreeTransactionInfo setCurrency(final String currencyCode) {
        this.currency = Currency.getInstance(currencyCode);
        return this;
    }

    public boolean isPaymentMethodToken() {
        return isPaymentMethodToken;
    }

    public String getPaymentMethodToken() {
        return paymentMethodToken;
    }

    public BrainTreeTransactionInfo setPaymentMethodToken(final String paymentMethodToken) {
        this.paymentMethodToken = paymentMethodToken;
        isPaymentMethodToken = this.paymentMethodToken != null;
        return this;
    }

    public StoreInVault getStoreInVault() {
        return storeInVault;
    }

    public BrainTreeTransactionInfo setStoreInVault(final StoreInVault storeInVault) {
        this.storeInVault = storeInVault;
        return this;
    }

    public String getNonce() {
        return nonce;
    }

    public BrainTreeTransactionInfo setNonce(String nonce) {
        this.nonce = nonce;
        return this;
    }

    public void setMerchantAccountId(String merchantAccountId) {
        this.merchantAccountId = merchantAccountId;
    }

    public String getMerchantAccountId() {
        return merchantAccountId;
    }
}
