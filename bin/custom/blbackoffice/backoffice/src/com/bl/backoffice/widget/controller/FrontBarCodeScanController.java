package com.bl.backoffice.widget.controller;

import com.bl.backoffice.wizards.util.WebScanToolData;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.inventory.scan.service.BlInventoryScanToolService;
import com.bl.logging.BlLogger;
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

  private ConsignmentModel selectedConsignment;

  private WebScanToolData shippingScanToolData;

  @SocketEvent(socketId = "inputObject")
  public void initCustomerAddressForm(final ConsignmentModel inputObject) {
    this.getWidgetInstanceManager()
        .setTitle(TITLE_MESSG + " : " + inputObject.getOrder().getCode());
    selectedConsignment = inputObject;
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
    createResponseForScanResult(barCodeList);
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
          getBlInventoryScanToolService().checkValidLocationInBarcodeList(barCodes), barCodes);
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
   * @param result
   * @param barcodes method will notify user according to result number calculated in previous
   *                 method
   */
  private void createResponseMegForScan(final int result, final List<String> barcodes) {
    if (result == BlInventoryScanLoggingConstants.ONE) {
      final List<String> failedBarcodeList = getBlInventoryScanToolService()
          .getFailedBarcodeList(barcodes);
      if (CollectionUtils.isNotEmpty(failedBarcodeList)) {
        BlLogger.logFormatMessageInfo(LOG, Level.INFO,
            BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE_MSG,
            failedBarcodeList);
        throw new WrongValueException(this.barCodes,
            this.getLabel("blbackoffice.frontbarcode.scan.batch.error"));
      } else {
        BlLogger.logFormatMessageInfo(LOG, Level.INFO, SUCCESS_MSG, barcodes.size());
        this.showMessageBox(SUCCESS_MSG);
      }
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

  public BlInventoryScanToolService getBlInventoryScanToolService() {
    return blInventoryScanToolService;
  }

  /**
   * Method to render the message box
   *
   * @param message
   */
  private void showMessageBox(final String message) {
    Messagebox.show(message);
    this.sendOutput(OUT_CONFIRM, COMPLETE);
  }
}
