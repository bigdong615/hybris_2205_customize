package com.bl.backoffice.wizards.util;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.zkoss.zul.impl.InputElement;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

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
        final List modifiableBarcodeInputFieldList = new ArrayList<>();
        modifiableBarcodeInputFieldList.addAll(Arrays.asList(newPwdInput.getText().split("\n")));
        if(CollectionUtils.isNotEmpty(modifiableBarcodeInputFieldList)) {
            modifiableBarcodeInputFieldList.removeAll(Collections.singleton(null));
            modifiableBarcodeInputFieldList.removeAll(Collections.singleton(StringUtils.EMPTY));
        }
        webScanToolData.setBarcodeInputField(modifiableBarcodeInputFieldList);
    }
}
