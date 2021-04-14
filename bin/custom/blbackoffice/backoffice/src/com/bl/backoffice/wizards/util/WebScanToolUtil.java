package com.bl.backoffice.wizards.util;

import org.zkoss.zul.impl.InputElement;

import java.util.Arrays;

/**
 * {javadoc}
 *
 * @author Namrata Lohar
 **/
public class WebScanToolUtil {

    /**
     * javadoc
     * @param newPwdInput of InputElement
     * @param webScanToolData pojo
     * method will set the input barcodes provided in backoffice popup in the WebScanToolData.barcodeInputField
     */
    public void onBarcodeInputFieldTextChanged(final InputElement newPwdInput, final WebScanToolData webScanToolData) {
        webScanToolData.setBarcodeInputField(Arrays.asList(newPwdInput.getText().split("\n")));
    }
}
