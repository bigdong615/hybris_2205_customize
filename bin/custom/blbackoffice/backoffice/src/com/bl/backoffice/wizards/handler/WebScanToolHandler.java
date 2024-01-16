package com.bl.backoffice.wizards.handler;

import com.bl.backoffice.wizards.renderer.WebScanToolRenderer;
import com.bl.backoffice.wizards.util.WebScanToolData;
import com.bl.backoffice.wizards.util.WebScanToolUtil;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.inventory.scan.service.BlInventoryScanToolService;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
import com.hybris.backoffice.widgets.notificationarea.event.NotificationEvent;
import com.hybris.cockpitng.config.jaxb.wizard.CustomType;
import com.hybris.cockpitng.util.notifications.NotificationService;
import com.hybris.cockpitng.widgets.configurableflow.FlowActionHandlerAdapter;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * {javadoc}
 *
 * @author Namrata Lohar
 **/
public class WebScanToolHandler implements com.hybris.cockpitng.widgets.configurableflow.FlowActionHandler {

    private static final Logger LOG = Logger.getLogger(WebScanToolHandler.class);

    private NotificationService notificationService;
    private BlInventoryScanToolService blInventoryScanToolService;

    @Autowired
    private WebScanToolUtil webScanToolUtil;

    @Autowired
    private WebScanToolRenderer webScanToolRenderer;

    /**
     *
     * @param customType
     * @param flowActionHandlerAdapter
     * @param map
     * OOB method which will perform actions on input barcodes form backoffice wizard
     */
    @Override
    public void perform(final CustomType customType, final FlowActionHandlerAdapter flowActionHandlerAdapter,
                        final Map<String, String> map) {
        long startTime = System.nanoTime();
        List<String> barcodeList = new ArrayList<>();
        final WebScanToolData webScanToolData = (WebScanToolData) flowActionHandlerAdapter.getWidgetInstanceManager().getModel().
                getValue((String) map.get(BlInventoryScanLoggingConstants.WEB_SCAN_TOOL_DATA_MODEL_KEY), WebScanToolData.class);

        if (webScanToolData == null) {
            this.displayNotificationForString(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
                    BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE, NotificationEvent.Level.FAILURE, StringUtils.EMPTY);
        } else {
            this.getNotificationService().clearNotifications(BlInventoryScanLoggingConstants.NOTIFICATION_HANDLER);
            final List<String> barcodes = webScanToolData.getBarcodeInputField();
            if (CollectionUtils.isNotEmpty(barcodes)) {
                createResponseForScanResult(barcodes);
            } else {
                this.displayNotificationForString(BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
                        BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE, NotificationEvent.Level.FAILURE, StringUtils.EMPTY);
            }
            barcodeList = webScanToolData.getBarcodeInputField();
            this.triggerClear(webScanToolData);
            long stopTime = System.nanoTime();
            if(LOG.isDebugEnabled()) {
                BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "WebScanTool perform method having barcodes {} took {} time to execute", barcodeList, stopTime - startTime);
            }
        }
    }

    /**
     * javadoc
     * @param barcodes of list
     * method will check the input size and notify user accordingly with conditions
     */
    private void createResponseForScanResult(final List<String> barcodes) {
        final int barcodeSize = barcodes.size();
        final String maxSequenceScan = getBlInventoryScanToolService().getConfigKeyFromScanConfiguration(BlInventoryScanLoggingConstants.MAX_SEQUENCE_LIMIT_KEY);
        if (barcodeSize >= BlInventoryScanLoggingConstants.TWO && barcodeSize <= Integer.parseInt(maxSequenceScan)) {
            itemScanSuccess(barcodes, maxSequenceScan);
        } else {
            if(barcodeSize == BlInventoryScanLoggingConstants.ZERO) {
                BlLogger.logFormatMessageInfo(LOG, Level.INFO,BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
                        BlInventoryScanLoggingConstants.SCAN_STRING + barcodes);
                this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                        BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
                        BlInventoryScanLoggingConstants.SCAN_STRING + barcodes);
            } else if (barcodeSize == BlInventoryScanLoggingConstants.ONE) {
                if(getBlInventoryScanToolService().checkLastBarcodeIsLocationOrNot(barcodes, maxSequenceScan, true)) {
                    this.displayNotificationForString(BlInventoryScanLoggingConstants.ONE_ITEM_SCAN_ERROR_FAILURE_MSG,
                            BlInventoryScanLoggingConstants.ONE_ITEM_SCAN_ERROR_FAILURE, NotificationEvent.Level.FAILURE, StringUtils.EMPTY);
                } else {
                    BlLogger.logFormatMessageInfo(LOG, Level.INFO,BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
                            BlInventoryScanLoggingConstants.SCAN_STRING + barcodes);
                    this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                            BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
                            BlInventoryScanLoggingConstants.SCAN_STRING + barcodes);
                }
            } else {
                this.displayNotificationForString(BlInventoryScanLoggingConstants.MAX_BARCODE_LIMIT_ERROR_FAILURE_MSG,
                        BlInventoryScanLoggingConstants.MAX_BARCODE_LIMIT_ERROR_FAILURE, NotificationEvent.Level.FAILURE, maxSequenceScan);
            }
        }
    }

    /**
     * This method will check item scan success or failure
     *
     * @param barcodes list
     * @param maxSequenceScan size
     */
    private void itemScanSuccess(final List<String> barcodes, final String maxSequenceScan) {
        if(getBlInventoryScanToolService().checkLastBarcodeIsLocationOrNot(barcodes, maxSequenceScan, false)) {
            if(getBlInventoryScanToolService().checkBINOrSerialScan(barcodes)) {
                createResponseMegForBINScan(getBlInventoryScanToolService().doBINScanFromWebScanTool(barcodes), barcodes);
            } else {
                createResponseMegForScan(getBlInventoryScanToolService().checkValidLocationInBarcodeList(barcodes,
                        Lists.newArrayList(BlInventoryScanLoggingConstants.ALLOW_SCAN)), barcodes);
            }
        } else {
            this.displayNotificationForInt(BlInventoryScanLoggingConstants.MAX_BARCODE_LIMIT_EQ_ERROR_FAILURE_MSG,
                    BlInventoryScanLoggingConstants.MAX_BARCODE_LIMIT_EQ_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
                    Integer.parseInt(maxSequenceScan)-1);
        }
    }

    /**
     * javadoc
     * @param result
     * @param barcodes
     * method will notify user according to result number calculated in previous method
     */
    private void createResponseMegForScan(final int result, final List<String> barcodes) {
        switch (result) {
            case BlInventoryScanLoggingConstants.ONE:
                checkSuccessForScan(barcodes);
                break;

            case BlInventoryScanLoggingConstants.TWO:
                this.displayNotificationForString(BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE_MSG,
                        BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
                        barcodes.get(barcodes.size() - 1));
                break;

            case BlInventoryScanLoggingConstants.THREE:
                this.displayNotificationForString(BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE_MSG,
                        BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE, NotificationEvent.Level.FAILURE, StringUtils.EMPTY);
                break;

            default:
                this.displayNotificationForString(BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE_MSG,
                        BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE, NotificationEvent.Level.FAILURE, StringUtils.EMPTY);
                break;
        }
    }

    /**
     * javadoc
     * This method will check barcodes for scan
     *
     * @param barcodes barcodes
     */
    private void checkSuccessForScan(final List<String> barcodes) {
        final List<String> failedBarcodeList = getBlInventoryScanToolService().getFailedBarcodeList(barcodes);
        if (CollectionUtils.isNotEmpty(failedBarcodeList)) {
            BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE_MSG,
                    failedBarcodeList);
            this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                    BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
                    failedBarcodeList);
        } else {
            this.displayNotificationForString(BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG,
                    BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS, NotificationEvent.Level.SUCCESS,
                    (barcodes.size() - 1) + StringUtils.EMPTY + getBlInventoryScanToolService().getSuccessString(barcodes));
        }
    }

    /**
     * javadoc
     * method will notify user according to result number calculated in previous method
     *
     * @param result result
     * @param barcodes barcodes
     */
    private void createResponseMegForBINScan(final int result, final List<String> barcodes) {
        switch (result) {
            case BlInventoryScanLoggingConstants.ONE:
                this.displayNotificationForInt(BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG,
                        BlInventoryScanLoggingConstants.SCAN_BIN_SUCCESS, NotificationEvent.Level.SUCCESS, (barcodes.size() - 1));
                break;

            case BlInventoryScanLoggingConstants.TWO:
                this.displayNotificationForString(BlInventoryScanLoggingConstants.VALID_BIN_LOCATION_ERROR_FAILURE_MSG,
                        BlInventoryScanLoggingConstants.VALID_BIN_LOCATION_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
                        barcodes.get(BlInventoryScanLoggingConstants.ZERO));
                break;

            case BlInventoryScanLoggingConstants.THREE:
                this.displayNotificationForString(BlInventoryScanLoggingConstants.VALID_PARENT_LOCATION_ERROR_FAILURE_MSG,
                        BlInventoryScanLoggingConstants.VALID_PARENT_LOCATION_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
                        barcodes.get(BlInventoryScanLoggingConstants.ONE));
                break;

            case BlInventoryScanLoggingConstants.FOUR:
                this.displayNotificationForString(BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE_MSG,
                        BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE, NotificationEvent.Level.FAILURE, StringUtils.EMPTY);
                break;

            default:
                this.displayNotificationForInt(BlInventoryScanLoggingConstants.MAX_BIN_LIMIT_ERROR_FAILURE_MSG,
                        BlInventoryScanLoggingConstants.MAX_BIN_LIMIT_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
                        BlInventoryScanLoggingConstants.TWO);
                break;
        }
    }

    /**
     * javadoc
     * This method will print and logger and display notification of type int
     *
     * @param loggerMessage logger
     * @param notificationMsg notification
     * @param notificationMode mode
     * @param lastMessage extra message
     */
    private void displayNotificationForInt(final String loggerMessage, final String notificationMsg,
                                     final NotificationEvent.Level notificationMode, final int lastMessage) {
        BlLogger.logFormatMessageInfo(LOG, Level.INFO,loggerMessage, lastMessage);
        this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.NOTIFICATION_HANDLER, notificationMsg,
                notificationMode, lastMessage);
    }

    /**
     * javadoc
     * This method will print and logger and display notification of type String
     *
     * @param loggerMessage logger
     * @param notificationMsg notification
     * @param notificationMode mode
     * @param lastMessage extra message
     */
    private void displayNotificationForString(final String loggerMessage, final String notificationMsg,
                                           final NotificationEvent.Level notificationMode, final String lastMessage) {
        BlLogger.logFormatMessageInfo(LOG, Level.INFO,loggerMessage, lastMessage);
        this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.NOTIFICATION_HANDLER, notificationMsg,
                notificationMode, lastMessage);
    }

    /**
     * javadoc
     *
     * This method will clear text box contents and set updated list to the WebScanToolData
     *
     * @param webScanToolData data
     */
    public void triggerClear(final WebScanToolData webScanToolData) {
        new WebScanToolRenderer().triggerClear(webScanToolData, this.getWebScanToolUtil());
    }

    public NotificationService getNotificationService() {
        return notificationService;
    }

    public void setNotificationService(final NotificationService notificationService) {
        this.notificationService = notificationService;
    }

    public BlInventoryScanToolService getBlInventoryScanToolService() {
        return blInventoryScanToolService;
    }

    public void setBlInventoryScanToolService(final BlInventoryScanToolService blInventoryScanToolService) {
        this.blInventoryScanToolService = blInventoryScanToolService;
    }

    public WebScanToolUtil getWebScanToolUtil() {
        return webScanToolUtil;
    }

    public void setWebScanToolUtil(WebScanToolUtil webScanToolUtil) {
        this.webScanToolUtil = webScanToolUtil;
    }

    public WebScanToolRenderer getWebScanToolRenderer() {
        return webScanToolRenderer;
    }

    public void setWebScanToolRenderer(WebScanToolRenderer webScanToolRenderer) {
        this.webScanToolRenderer = webScanToolRenderer;
    }
}
