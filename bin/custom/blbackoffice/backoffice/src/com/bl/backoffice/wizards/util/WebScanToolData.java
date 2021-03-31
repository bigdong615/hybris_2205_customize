package com.bl.backoffice.wizards.util;

import java.util.List;

public class WebScanToolData {

    private List<String> barcodeInputField;
    private boolean validationPassed;

    public List<String> getBarcodeInputField() {
        return barcodeInputField;
    }

    public void setBarcodeInputField(List<String> barcodeInputField) {
        this.barcodeInputField = barcodeInputField;
    }

    private List<String> validations;

    public boolean isValidationPassed() {
        return validationPassed;
    }

    public void setValidationPassed(boolean validationPassed) {
        this.validationPassed = validationPassed;
    }

    public List<String> getValidations() {
        return validations;
    }

    public void setValidations(List<String> validations) {
        this.validations = validations;
    }

}
