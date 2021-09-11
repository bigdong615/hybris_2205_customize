package com.bl.backoffice.wizards.util;

import java.util.List;

/**
 * {javadoc}
 *
 * @author Namrata Lohar
 **/
public class InventoryCycleCountScanToolData {

    private List<String> serialBarcodeInputField;

    private List<String> skusInputField;

    public List<String> getSerialBarcodeInputField() {
        return serialBarcodeInputField;
    }

    public void setSerialBarcodeInputField(List<String> serialBarcodeInputField) {
        this.serialBarcodeInputField = serialBarcodeInputField;
    }

    public List<String> getSkusInputField() {
        return skusInputField;
    }

    public void setSkusInputField(List<String> skusInputField) {
        this.skusInputField = skusInputField;
    }
}
