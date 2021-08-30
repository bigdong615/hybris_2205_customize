package com.bl.backoffice.widget.controller;

import com.bl.backoffice.wizards.util.WebScanToolData;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.inventory.scan.service.BlInventoryScanToolService;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import java.util.Arrays;
import java.util.List;
import javax.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.WrongValueException;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Textbox;

/**
 *
 * @author Krishan Vashishth
 */
public class FrontBarCodeScanController extends DefaultWidgetController {

  private static final Logger LOG = Logger.getLogger(FrontBarCodeScanController.class);
  private static final int MAX_SCAN_ITEM = 100;
  protected static final String OUT_CONFIRM = "confirmOutput";
  private static final String SUCCESS_MSG = "Bar Code Scanning completed successfully";
  private static final String COMPLETE = "completed";
  private static final String TITLE_MESSG = "Scan Bar codes for order";
  private static final String MAX_BARCODE_LIMIT_ERROR_FAILURE = "max.bar.code.limit.failure";
  private static final String MUST_TWO_BARCODE_ERROR_FAILURE = "min.two.bar.code.limit.message";

  @Resource
  private BlInventoryScanToolService blInventoryScanToolService;

  @Wire
  private Textbox barCodes;

  private WebScanToolData shippingScanToolData;

  @SocketEvent(socketId = "inputObject")
  public void initCustomerAddressForm(final ConsignmentModel inputObject) {
    this.getWidgetInstanceManager()
        .setTitle(TITLE_MESSG + " : " + inputObject.getOrder().getCode());
    shippingScanToolData = new WebScanToolData();
  }

  @ViewEvent(componentID = "cancelChanges", eventName = "onClick")
  public void close() {
    this.sendOutput(OUT_CONFIRM, COMPLETE);
  }

  @ViewEvent(componentID = "barCodes", eventName = "onChange")
  public void updateBarCodeTextBox() {
    shippingScanToolData.setBarcodeInputField(Arrays.asList(barCodes.getValue().split("\n")));
  }

  @ViewEvent(componentID = "scanBarCodes", eventName = "onClick")
  public void scanBarCodes() {
    if (shippingScanToolData == null
        || CollectionUtils.isEmpty(shippingScanToolData.getBarcodeInputField())) {
      BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
          BlInventoryScanLoggingConstants.BAR_CODE_SCAN_EMPTY_BAR_CODES, StringUtils.EMPTY);
      throw new WrongValueException(this.barCodes,
          this.getLabel("blbackoffice.frontbarcode.scan.error.emptybarcode"));
    }
    final List<String> barCodeList = shippingScanToolData.getBarcodeInputField();
    createResponseForScanResult(barCodeList);
  }

  @ViewEvent(componentID = "updateChanges", eventName = "onClick")
  public void updateBarCodeChanges() {
    /*TODO*/
  }

  @ViewEvent(componentID = "updateBinLocation", eventName = "onClick")
  public void updateBinLocation() {
    if (shippingScanToolData == null
        || CollectionUtils.isEmpty(shippingScanToolData.getBarcodeInputField())) {
      BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
          BlInventoryScanLoggingConstants.BAR_CODE_SCAN_EMPTY_BAR_CODES, StringUtils.EMPTY);
      throw new WrongValueException(this.barCodes,
          this.getLabel("blbackoffice.frontbarcode.scan.error.emptybarcode"));
    }
    final List<String> barCodeList = shippingScanToolData.getBarcodeInputField();
    if (barCodeList.size() == 2) {
      createResponseForBinScanResult(barCodeList);
    } else {
      BlLogger.logFormatMessageInfo(LOG, Level.INFO,
          BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
          BlInventoryScanLoggingConstants.SCAN_STRING + barCodes);
      throw new WrongValueException(this.barCodes, this.getLabel(MUST_TWO_BARCODE_ERROR_FAILURE));
    }
  }

  /**
   * Method to create the response for bar code scan for bin
   *
   * @param barCodeList - the original bar code scan list
   */
  private void createResponseForBinScanResult(final List<String> barCodeList) {
    final int result = getBlInventoryScanToolService()
        .checkValidLocationInBarcodeListForBin(barCodeList, Lists.newArrayList("ALLOW_SCAN"));
    if (result == BlInventoryScanLoggingConstants.ONE) {
      final List<String> failedBarCodeList = getBlInventoryScanToolService()
          .getFailedBarcodeListForBin(barCodeList);
      checkFailedBarCodes(barCodeList, failedBarCodeList);
    } else {
      BlLogger.logFormatMessageInfo(LOG, Level.INFO,
          BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE_MSG, StringUtils.EMPTY);
      throw new WrongValueException(this.barCodes,
          this.getLabel("blbackoffice.frontbarcode.scan.many.location.error"));
    }
  }

  /**
   * javadoc
   *
   * @param barCodes of list method will check the input size and notify user accordingly with
   *                 conditions
   */
  private void createResponseForScanResult(final List<String> barCodes) {
    final int barcodeSize = barCodes.size();
    if (barcodeSize >= BlInventoryScanLoggingConstants.TWO && barcodeSize <= MAX_SCAN_ITEM) {
      createResponseMegForScan(
          getBlInventoryScanToolService().checkValidLocationInBarcodeList(barCodes, Lists.newArrayList("ALLOW_SCAN")), barCodes);
    } else {
      if (barcodeSize < BlInventoryScanLoggingConstants.TWO) {
        BlLogger.logFormatMessageInfo(LOG, Level.INFO,
            BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
            BlInventoryScanLoggingConstants.SCAN_STRING + barCodes);
        throw new WrongValueException(this.barCodes, this.getLabel(MUST_TWO_BARCODE_ERROR_FAILURE));
      } else {
        BlLogger.logFormatMessageInfo(LOG, Level.INFO,
            BlInventoryScanLoggingConstants.MAX_BARCODE_LIMIT_ERROR_FAILURE_MSG, MAX_SCAN_ITEM);
        throw new WrongValueException(this.barCodes,
            this.getLabel(MAX_BARCODE_LIMIT_ERROR_FAILURE));
      }
    }
  }

  /**
   * javadoc
   *
   * @param result - the item count
   * @param barCodeList method will notify user according to result number calculated in previous
   *                    method
   */
  private void createResponseMegForScan(final int result, final List<String> barCodeList) {
    if (result == BlInventoryScanLoggingConstants.ONE) {
      final List<String> failedBarCodeList = getBlInventoryScanToolService()
          .getFailedBarcodeList(barCodeList);
      checkFailedBarCodes(barCodeList, failedBarCodeList);
    } else if (result == BlInventoryScanLoggingConstants.TWO) {
      BlLogger.logFormatMessageInfo(LOG, Level.INFO,
          BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE_MSG, StringUtils.EMPTY);
      throw new WrongValueException(this.barCodes,
          this.getLabel("blbackoffice.frontbarcode.scan.lastScam.invalid.error"));
    } else if (result == BlInventoryScanLoggingConstants.THREE) {
      BlLogger.logFormatMessageInfo(LOG, Level.INFO,
          BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE_MSG, StringUtils.EMPTY);
      throw new WrongValueException(this.barCodes,
          this.getLabel("blbackoffice.frontbarcode.scan.lastScan.failure.error"));
    } else {
      BlLogger.logFormatMessageInfo(LOG, Level.INFO,
          BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE_MSG, StringUtils.EMPTY);
      throw new WrongValueException(this.barCodes,
          this.getLabel("blbackoffice.frontbarcode.scan.many.location.error"));
    }
  }

  /**
   * Method to show the errors or success message in case if failed bar codes is found or not.
   *
   * @param barCodeList - the original bar code list from editor
   * @param failedBarCodeList - the bar codes which are not present in the system
   */
  private void checkFailedBarCodes(final List<String> barCodeList,
      final List<String> failedBarCodeList) {
    if (CollectionUtils.isNotEmpty(failedBarCodeList)) {
      BlLogger.logFormatMessageInfo(LOG, Level.INFO,
          BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE_MSG,
          failedBarCodeList);
      throw new WrongValueException(this.barCodes,
          this.getLabel("blbackoffice.frontbarcode.scan.batch.error"));
    } else {
      BlLogger.logFormatMessageInfo(LOG, Level.INFO, SUCCESS_MSG, barCodeList.size());
      this.showMessageBox();
    }
  }

  public BlInventoryScanToolService getBlInventoryScanToolService() {
    return blInventoryScanToolService;
  }

  /**
   * Method to render the message box
   *
   */
  private void showMessageBox() {
    Messagebox.show(SUCCESS_MSG);
    this.sendOutput(OUT_CONFIRM, COMPLETE);
  }
}
