package com.bl.backoffice.wizards.util;

public class OrderTrackingScanToolData {

    private String serialNo;
    private String productName;
    private String productCode;
    private String shipmentStartDate;

    public String getSerialNo() {
        return serialNo;
    }

    public void setSerialNo(String serialNo) {
        this.serialNo = serialNo;
    }

    public String getProductName() {
        return productName;
    }

    public void setProductName(String productName) {
        this.productName = productName;
    }

    public String getProductCode() {
        return productCode;
    }

    public void setProductCode(String productCode) {
        this.productCode = productCode;
    }

    public String getShipmentStartDate() {
        return shipmentStartDate;
    }

    public void setShipmentStartDate(String shipmentStartDate) {
        this.shipmentStartDate = shipmentStartDate;
    }
}
