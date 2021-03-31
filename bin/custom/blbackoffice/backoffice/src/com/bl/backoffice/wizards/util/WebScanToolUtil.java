package com.bl.backoffice.wizards.util;

import org.zkoss.zul.impl.InputElement;

import java.util.Arrays;

public class WebScanToolUtil {

    public void onBarcodeInputFieldTextChanged(final InputElement newPwdInput, final WebScanToolData webScanToolData)
    {
        webScanToolData.setBarcodeInputField(Arrays.asList(newPwdInput.getText().split("\n")));
    }
}
