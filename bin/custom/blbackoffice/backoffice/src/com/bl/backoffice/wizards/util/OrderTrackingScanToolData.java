package com.bl.backoffice.wizards.util;

import de.hybris.platform.core.enums.OrderStatus;

import java.util.List;

public class OrderTrackingScanToolData {

    private String orderNumber;
    private String customerEmail;
    private String rentalStartDate;
    private String rentalEndDate;
    private String expectedReturnDate;
    private OrderStatus status;
    private List<SerialData> serialData;


    public String getOrderNumber() {
        return orderNumber;
    }

    public void setOrderNumber(String orderNumber) {
        this.orderNumber = orderNumber;
    }

    public String getCustomerEmail() {
        return customerEmail;
    }

    public void setCustomerEmail(String customerEmail) {
        this.customerEmail = customerEmail;
    }

    public String getRentalStartDate() {
        return rentalStartDate;
    }

    public void setRentalStartDate(String rentalStartDate) {
        this.rentalStartDate = rentalStartDate;
    }

    public String getRentalEndDate() {
        return rentalEndDate;
    }

    public void setRentalEndDate(String rentalEndDate) {
        this.rentalEndDate = rentalEndDate;
    }

    public String getExpectedReturnDate() {
        return expectedReturnDate;
    }

    public void setExpectedReturnDate(String expectedReturnDate) {
        this.expectedReturnDate = expectedReturnDate;
    }

    public OrderStatus getStatus() {
        return status;
    }

    public void setStatus(OrderStatus status) {
        this.status = status;
    }

    public List<SerialData> getSerialData() {
        return serialData;
    }

    public void setSerialData(List<SerialData> serialData) {
        this.serialData = serialData;
    }
}
