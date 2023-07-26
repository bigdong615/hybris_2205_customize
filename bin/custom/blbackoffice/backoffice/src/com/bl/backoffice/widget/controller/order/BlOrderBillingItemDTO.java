package com.bl.backoffice.widget.controller.order;

public class BlOrderBillingItemDTO {

    private String productName;
    private String serialNo;
    private Double amount;
    private Double subtotal;
    private boolean damageWaiver;
    private boolean processingFee;
    private double tax;
    private String unpaidBillNotes;

    public BlOrderBillingItemDTO(String productName, String serialNo, Double amount, Double subtotal, boolean damageWaiver, boolean processingFee, double tax, String unpaidBillNotes) {
        this.productName = productName;
        this.serialNo = serialNo;
        this.amount = amount;
        this.subtotal = subtotal;
        this.damageWaiver = damageWaiver;
        this.processingFee = processingFee;
        this.tax = tax;
        this.unpaidBillNotes = unpaidBillNotes;
    }

    public BlOrderBillingItemDTO() {

    }

    public String getProductName() {
        return productName;
    }

    public void setProductName(String productName) {
        this.productName = productName;
    }

    public String getSerialNo() {
        return serialNo;
    }

    public void setSerialNo(String serialNo) {
        this.serialNo = serialNo;
    }

    public Double getAmount() {
        return amount;
    }

    public void setAmount(Double amount) {
        this.amount = amount;
    }

    public Double getSubtotal() {
        return subtotal;
    }

    public void setSubtotal(Double subtotal) {
        this.subtotal = subtotal;
    }

    public boolean isDamageWaiver() {
        return damageWaiver;
    }

    public void setDamageWaiver(boolean damageWaiver) {
        this.damageWaiver = damageWaiver;
    }

    public boolean isProcessingFee() {
        return processingFee;
    }

    public void setProcessingFee(boolean processingFee) {
        this.processingFee = processingFee;
    }

    public double getTax() {
        return tax;
    }

    public void setTax(double tax) {
        this.tax = tax;
    }

    public String getUnpaidBillNotes() {
        return unpaidBillNotes;
    }

    public void setUnpaidBillNotes(String unpaidBillNotes) {
        this.unpaidBillNotes = unpaidBillNotes;
    }
}
