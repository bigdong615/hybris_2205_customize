package com.braintree.customersupportbackoffice.widgets;

import java.math.BigDecimal;

public class NewTransactionByVaultForm {
    private String paymentMethodToken;
    private BigDecimal amount;
    private BigDecimal taxAmount;
    private String customFields;

    private String status;

    public String getPaymentMethodToken() {
        return paymentMethodToken;
    }

    public void setPaymentMethodToken(String paymentMethodToken) {
        this.paymentMethodToken = paymentMethodToken;
    }

    public BigDecimal getAmount() {
        return amount;
    }

    public void setAmount(BigDecimal amount) {
        this.amount = amount;
    }

    public BigDecimal getTaxAmount() {
        return taxAmount;
    }

    public void setTaxAmount(BigDecimal taxAmount) {
        this.taxAmount = taxAmount;
    }

    public String getCustomFields() {
        return customFields;
    }

    public void setCustomFields(String customFields) {
        this.customFields = customFields;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }
}
