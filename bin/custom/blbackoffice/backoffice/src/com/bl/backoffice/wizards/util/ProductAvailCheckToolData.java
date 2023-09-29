package com.bl.backoffice.wizards.util;

public class ProductAvailCheckToolData implements Comparable<ProductAvailCheckToolData>{
	private String productCode;
    private String articleNumber;
    private String barcode;
    private String serialStatus;
    private String warehouseLocation;
    private String ocLocation;
    private String lastLocationScanParent;
    public String getProductCode() {
        return productCode;
    }
    public void setProductCode(final String productCode) {
        this.productCode = productCode;
    }

    public String getArticleNumber() {
        return articleNumber;
    }

    public void setArticleNumber(final String articleNumber) {
        this.articleNumber = articleNumber;
    }

    public String getBarcode() {
        return barcode;
    }

    public void setBarcode(final String barcode) {
        this.barcode = barcode;
    }

    public String getSerialStatus() {
        return serialStatus;
    }

    public void setSerialStatus(final String serialStatus) {
        this.serialStatus = serialStatus;
    }

    public String getWarehouseLocation() {
        return warehouseLocation;
    }

    public void setWarehouseLocation(final String warehouseLocation) {
        this.warehouseLocation = warehouseLocation;
    }

    public String getOcLocation() {
        return ocLocation;
    }

    public void setOcLocation(final String ocLocation) {
        this.ocLocation = ocLocation;
    }

    public String getLastLocationScanParent() {
        return lastLocationScanParent;
    }

    public void setLastLocationScanParent(final String lastLocationScanParent) {
        this.lastLocationScanParent = lastLocationScanParent;
    }

    @Override
    public int compareTo(final ProductAvailCheckToolData o) {
        return 0;
    }
}
