package com.bl.backoffice.wizards.handler;

import com.bl.backoffice.wizards.util.WebScanToolData;
import com.bl.constants.BLInventoryScanLoggingConstants;
import com.bl.core.inventory.scan.service.BLInventoryScanToolService;
import com.hybris.backoffice.widgets.notificationarea.event.NotificationEvent;
import com.hybris.cockpitng.config.jaxb.wizard.CustomType;
import com.hybris.cockpitng.util.notifications.NotificationService;
import com.hybris.cockpitng.widgets.configurableflow.FlowActionHandlerAdapter;
import de.hybris.platform.util.Config;
import org.apache.commons.collections.CollectionUtils;

import java.util.List;
import java.util.Map;

public class WebScanToolHandler implements com.hybris.cockpitng.widgets.configurableflow.FlowActionHandler {

    private NotificationService notificationService;
    private BLInventoryScanToolService blInventoryScanToolService;

    public NotificationService getNotificationService() {
        return notificationService;
    }

    public void setNotificationService(NotificationService notificationService) {
        this.notificationService = notificationService;
    }

    public BLInventoryScanToolService getBlInventoryScanToolService() {
        return blInventoryScanToolService;
    }

    public void setBlInventoryScanToolService(BLInventoryScanToolService blInventoryScanToolService) {
        this.blInventoryScanToolService = blInventoryScanToolService;
    }

    @Override
    public void perform(CustomType customType, FlowActionHandlerAdapter flowActionHandlerAdapter, Map<String, String> map) {
        WebScanToolData webScanToolData = (WebScanToolData) flowActionHandlerAdapter.getWidgetInstanceManager().getModel().
                getValue((String) map.get(BLInventoryScanLoggingConstants.WEB_SCAN_TOOL_DATA_MODEL_KEY), WebScanToolData.class);
        if (webScanToolData == null) {
            this.getNotificationService().notifyUser(BLInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                    BLInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE, NotificationEvent.Level.FAILURE,
                    new Object[0]);
        } else {
            List<String> barcodes = webScanToolData.getBarcodeInputField();
            if(CollectionUtils.isNotEmpty(barcodes)){
                int barcodeSize = barcodes.size();
                if (barcodeSize >= BLInventoryScanLoggingConstants.TWO && barcodeSize < BLInventoryScanLoggingConstants.EIGHT) {
                    createResponseMegForScan(getBlInventoryScanToolService().checkValidLocationInBarcodeList(barcodes), barcodes);
                } else {
                    if(barcodeSize < BLInventoryScanLoggingConstants.TWO) {
                        this.getNotificationService().notifyUser(BLInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                                BLInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
                                new Object[]{barcodes});
                    } else {
                        this.getNotificationService().notifyUser(BLInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                                BLInventoryScanLoggingConstants.MAX_BARCODE_LIMIT_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
                                new Object[]{Config.getParameter(BLInventoryScanLoggingConstants.MAX_SEQUENCE_LIMIT_KEY)});
                    }
                }
            } else {
                this.getNotificationService().notifyUser(BLInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                        BLInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
                        new Object[0]);
            }
        }
    }

    private void createResponseMegForScan(int result, List<String> barcodes) {
        if (result == BLInventoryScanLoggingConstants.ONE) {
            List<String> failedBarcodeList = getBlInventoryScanToolService().getFailedBarcodeList(barcodes);
            if (CollectionUtils.isNotEmpty(failedBarcodeList)) {
                this.getNotificationService().notifyUser(BLInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                        BLInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE, NotificationEvent.Level.WARNING,
                        new Object[]{failedBarcodeList});
            } else {
                this.getNotificationService().notifyUser(BLInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                        BLInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS, NotificationEvent.Level.SUCCESS,
                        new Object[]{barcodes.size()});
            }
        } else if (result == BLInventoryScanLoggingConstants.TWO) {
            this.getNotificationService().notifyUser(BLInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                    BLInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
                    new Object[0]);
        } else if (result == BLInventoryScanLoggingConstants.THREE) {
            this.getNotificationService().notifyUser(BLInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                    BLInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
                    new Object[0]);
        } else {
            this.getNotificationService().notifyUser(BLInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                    BLInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
                    new Object[0]);
        }
    }
}
