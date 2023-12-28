package com.bl.backoffice.widget.controller.order;

public class BlOrderBillingLateFeeDTO implements Comparable<BlOrderBillingLateFeeDTO>{
    private String productName;
    private String serialCode;
    private int duration;
    private String rentalEndDate;
    private String actualReturnDate;
    private int daysLate;
    private String dailyRate;
    private String taxableSubtotal;
    private String tax;
    private String subtotalAmountDue;
    private String unpaidBillNotes;

    public BlOrderBillingLateFeeDTO(String productName, String serialCode, int duration, String rentalEndDate, String actualReturnDate, int daysLate, String dailyRate, String taxableSubtotal, String tax, String subtotalAmountDue, String unpaidBillNotes) {
        this.productName = productName;
        this.serialCode = serialCode;
        this.duration = duration;
        this.rentalEndDate = rentalEndDate;
        this.actualReturnDate = actualReturnDate;
        this.daysLate = daysLate;
        this.dailyRate = dailyRate;
        this.taxableSubtotal = taxableSubtotal;
        this.tax = tax;
        this.subtotalAmountDue = subtotalAmountDue;
        this.unpaidBillNotes = unpaidBillNotes;
    }

    public BlOrderBillingLateFeeDTO() {

    }
    public String getProductName() {
        return productName;
    }


    public void setProductName(String productName) {
        this.productName = productName;
    }

    public int getDuration() {
        return duration;
    }

    public void setDuration(int duration) {
        this.duration = duration;
    }

    public String getRentalEndDate() {
        return rentalEndDate;
    }

    public void setRentalEndDate(String rentalEndDate) {
        this.rentalEndDate = rentalEndDate;
    }

    public String getActualReturnDate() {
        return actualReturnDate;
    }

    public void setActualReturnDate(String actualReturnDate) {
        this.actualReturnDate = actualReturnDate;
    }

    public int getDaysLate() {
        return daysLate;
    }

    public void setDaysLate(int daysLate) {
        this.daysLate = daysLate;
    }

    public String getDailyRate() {
        return dailyRate;
    }

    public void setDailyRate(String dailyRate) {
        this.dailyRate = dailyRate;
    }

    public String getTaxableSubtotal() {
        return taxableSubtotal;
    }

    public void setTaxableSubtotal(String taxableSubtotal) {
        this.taxableSubtotal = taxableSubtotal;
    }

    public String getTax() {
        return tax;
    }

    public void setTax(String tax) {
        this.tax = tax;
    }

    public String getSubtotalAmountDue() {
        return subtotalAmountDue;
    }

    public void setSubtotalAmountDue(String subtotalAmountDue) {
        this.subtotalAmountDue = subtotalAmountDue;
    }

    public String getUnpaidBillNotes() {
        return unpaidBillNotes;
    }

    public void setUnpaidBillNotes(String unpaidBillNotes) {
        this.unpaidBillNotes = unpaidBillNotes;
    }

    @Override
    public int compareTo(BlOrderBillingLateFeeDTO o) {
        return 0;
    }

    public String getSerialCode() {
        return serialCode;
    }

    public void setSerialCode(String serialCode) {
        this.serialCode = serialCode;
    }

}