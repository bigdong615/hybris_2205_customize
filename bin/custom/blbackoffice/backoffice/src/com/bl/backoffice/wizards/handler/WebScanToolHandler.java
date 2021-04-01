package com.bl.backoffice.wizards.handler;

import com.bl.backoffice.wizards.util.WebScanToolData;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.inventory.scan.service.BlInventoryScanToolService;
import com.hybris.backoffice.widgets.notificationarea.event.NotificationEvent;
import com.hybris.cockpitng.config.jaxb.wizard.CustomType;
import com.hybris.cockpitng.util.notifications.NotificationService;
import com.hybris.cockpitng.widgets.configurableflow.FlowActionHandlerAdapter;
import de.hybris.platform.util.Config;
import org.apache.commons.collections.CollectionUtils;

import java.util.List;
import java.util.Map;

/**
 * {javadoc}
 *
 * @author Namrata Lohar
 **/
public class WebScanToolHandler implements com.hybris.cockpitng.widgets.configurableflow.FlowActionHandler {

    private NotificationService notificationService;
    private BlInventoryScanToolService blInventoryScanToolService;

    /**
     *
     * @param customType
     * @param flowActionHandlerAdapter
     * @param map
     */
    @Override
    public void perform(final CustomType customType, final FlowActionHandlerAdapter flowActionHandlerAdapter,
                        final Map<String, String> map) {
        final WebScanToolData webScanToolData = (WebScanToolData) flowActionHandlerAdapter.getWidgetInstanceManager().getModel().
                getValue((String) map.get(BlInventoryScanLoggingConstants.WEB_SCAN_TOOL_DATA_MODEL_KEY), WebScanToolData.class);

        if (webScanToolData == null) {
            this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                    BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE, NotificationEvent.Level.FAILURE,
                    "");
        } else {
            final List<String> barcodes = webScanToolData.getBarcodeInputField();
            if (CollectionUtils.isNotEmpty(barcodes)) {
                createResponseForScanResult(barcodes);
            } else {
                this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                        BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
                        "");
            }
        }
    }

    /**
     *
     * @param barcodes
     */
    private void createResponseForScanResult(final List<String> barcodes) {
        final int barcodeSize = barcodes.size();
        if (barcodeSize >= BlInventoryScanLoggingConstants.TWO && barcodeSize < BlInventoryScanLoggingConstants.EIGHT) {
            createResponseMegForScan(getBlInventoryScanToolService().checkValidLocationInBarcodeList(barcodes), barcodes);
        } else {
            if (barcodeSize < BlInventoryScanLoggingConstants.TWO) {
                this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                        BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
                        barcodes);
            } else {
                this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                        BlInventoryScanLoggingConstants.MAX_BARCODE_LIMIT_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
                        Config.getParameter(BlInventoryScanLoggingConstants.MAX_SEQUENCE_LIMIT_KEY));
            }
        }
    }

    /**
     *
     * @param result
     * @param barcodes
     */
    private void createResponseMegForScan(final int result, final List<String> barcodes) {
        if (result == BlInventoryScanLoggingConstants.ONE) {
            final List<String> failedBarcodeList = getBlInventoryScanToolService().getFailedBarcodeList(barcodes);
            if (CollectionUtils.isNotEmpty(failedBarcodeList)) {
                this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                        BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE, NotificationEvent.Level.WARNING,
                        failedBarcodeList);
            } else {
                this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                        BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS, NotificationEvent.Level.SUCCESS,
                        barcodes.size());
            }
        } else if (result == BlInventoryScanLoggingConstants.TWO) {
            this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                    BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
                    "");
        } else if (result == BlInventoryScanLoggingConstants.THREE) {
            this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                    BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
                    "");
        } else {
            this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                    BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
                    "");
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
