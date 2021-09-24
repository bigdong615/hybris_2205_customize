package com.bl.backoffice.wizards.util;

import com.bl.constants.BlInventoryScanLoggingConstants;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.zkoss.zul.impl.InputElement;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Util class for inventory cycle count scan data
 *
 * @author Namrata Lohar
 **/
public class InventoryCycleCountScanToolUtil {

    /**
     * method will set the input barcodes provided in backoffice popup in the WebScanToolData.barcodeInputField
     *
     * @param newPwdInput of InputElement
     * @param data pojo
     */
    public void onSerialBarcodeInputFieldTextChanged(final InputElement newPwdInput, final InventoryCycleCountScanToolData data) {
        data.setSerialBarcodeInputField(this.getFormattedInputList(newPwdInput));
    }

    /**
     * method will set the input barcodes provided in backoffice popup in the WebScanToolData.barcodeInputField
     *
     * @param newPwdInput of InputElement
     * @param data pojo
     */
    public void onSKUsInputFieldTextChanged(final InputElement newPwdInput, final InventoryCycleCountScanToolData data) {
        data.setSkusInputField(this.getFormattedInputList(newPwdInput));
    }

    /**
     * method will take input and format the list by next line
     *
     * @param newPwdInput input
     * @return list
     */
    private List getFormattedInputList(final InputElement newPwdInput) {
        final List modifiableBarcodeInputFieldList = new ArrayList<>();
        modifiableBarcodeInputFieldList.addAll(Arrays.asList(newPwdInput.getText().split(BlInventoryScanLoggingConstants.REGEX_N)));
        if (CollectionUtils.isNotEmpty(modifiableBarcodeInputFieldList)) {
            modifiableBarcodeInputFieldList.removeAll(Collections.singleton(null));
            modifiableBarcodeInputFieldList.removeAll(Collections.singleton(StringUtils.EMPTY));
        }
        return modifiableBarcodeInputFieldList;
    }

}
