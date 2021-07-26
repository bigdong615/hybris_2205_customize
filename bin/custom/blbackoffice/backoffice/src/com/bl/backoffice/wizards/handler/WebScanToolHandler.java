package com.bl.backoffice.wizards.handler;

import com.bl.backoffice.wizards.util.WebScanToolData;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.inventory.scan.service.BlInventoryScanToolService;
import com.bl.logging.BlLogger;
import com.hybris.backoffice.widgets.notificationarea.event.NotificationEvent;
import com.hybris.cockpitng.config.jaxb.wizard.CustomType;
import com.hybris.cockpitng.util.notifications.NotificationService;
import com.hybris.cockpitng.widgets.configurableflow.FlowActionHandlerAdapter;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import java.util.Collections;
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
        final WebScanToolData webScanToolData = (WebScanToolData) flowActionHandlerAdapter.getWidgetInstanceManager().getModel().
                getValue((String) map.get(BlInventoryScanLoggingConstants.WEB_SCAN_TOOL_DATA_MODEL_KEY), WebScanToolData.class);

        if (webScanToolData == null) {
            BlLogger.logFormatMessageInfo(LOG, Level.INFO,BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
                    StringUtils.EMPTY);
            this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                    BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE, NotificationEvent.Level.FAILURE,
                    StringUtils.EMPTY);
        } else {
            final List<String> barcodes = webScanToolData.getBarcodeInputField();
            if (CollectionUtils.isNotEmpty(barcodes)) {
                createResponseForScanResult(barcodes);
            } else {
                BlLogger.logFormatMessageInfo(LOG, Level.INFO,BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
                        StringUtils.EMPTY);
                this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                        BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
                        StringUtils.EMPTY);
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
        String maxSequenceScan = getBlInventoryScanToolService().
                getConfigKeyFromScanConfiguration(BlInventoryScanLoggingConstants.MAX_SEQUENCE_LIMIT_KEY);

        if (barcodeSize >= BlInventoryScanLoggingConstants.TWO && barcodeSize <= Integer.parseInt(maxSequenceScan)) {
            createResponseMegForScan(getBlInventoryScanToolService().checkValidLocationInBarcodeList(barcodes, Collections.emptyList()), barcodes);
        } else {
            if (barcodeSize < BlInventoryScanLoggingConstants.TWO) {
                BlLogger.logFormatMessageInfo(LOG, Level.INFO,BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
                        BlInventoryScanLoggingConstants.SCAN_STRING + barcodes);
                this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                        BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
                        BlInventoryScanLoggingConstants.SCAN_STRING + barcodes);
            } else {
                BlLogger.logFormatMessageInfo(LOG, Level.INFO,BlInventoryScanLoggingConstants.MAX_BARCODE_LIMIT_ERROR_FAILURE_MSG,
                        maxSequenceScan);
                this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                        BlInventoryScanLoggingConstants.MAX_BARCODE_LIMIT_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
                        maxSequenceScan);
            }
        }
    }

    /**
     * javadoc
     * @param result
     * @param barcodes
     * method will notify user according to result number calculated in previous method
     */
    private void createResponseMegForScan(final int result, final List<String> barcodes) {
        if (result == BlInventoryScanLoggingConstants.ONE) {
            final List<String> failedBarcodeList = getBlInventoryScanToolService().getFailedBarcodeList(barcodes);
            if (CollectionUtils.isNotEmpty(failedBarcodeList)) {
                BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE_MSG,
                        failedBarcodeList);
                this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                        BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE, NotificationEvent.Level.WARNING,
                        failedBarcodeList);
            } else {
                BlLogger.logFormatMessageInfo(LOG, Level.INFO,BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG,
                        barcodes.size());
                this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                        BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS, NotificationEvent.Level.SUCCESS,
                        barcodes.size());
            }
        } else if (result == BlInventoryScanLoggingConstants.TWO) {
            BlLogger.logFormatMessageInfo(LOG, Level.INFO,BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE_MSG,
                    StringUtils.EMPTY);
            this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                    BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
                    StringUtils.EMPTY);
        } else if (result == BlInventoryScanLoggingConstants.THREE) {
            BlLogger.logFormatMessageInfo(LOG, Level.INFO,BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE_MSG,
                    StringUtils.EMPTY);
            this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                    BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
                    StringUtils.EMPTY);
        } else {
            BlLogger.logFormatMessageInfo(LOG, Level.INFO,BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE_MSG,
                    StringUtils.EMPTY);
            this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                    BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
                    StringUtils.EMPTY);
        }
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
}
