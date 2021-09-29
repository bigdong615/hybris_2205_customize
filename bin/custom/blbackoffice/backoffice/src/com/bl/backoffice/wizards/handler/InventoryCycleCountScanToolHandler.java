package com.bl.backoffice.wizards.handler;

import com.bl.backoffice.wizards.renderer.InventoryCycleCountScanToolRenderer;
import com.bl.backoffice.wizards.util.InventoryCycleCountScanToolData;
import com.bl.backoffice.wizards.util.InventoryCycleCountScanToolUtil;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.inventory.cycle.count.service.BlInventoryCycleCountService;
import com.bl.logging.BlLogger;
import com.hybris.backoffice.widgets.notificationarea.event.NotificationEvent;
import com.hybris.cockpitng.config.jaxb.wizard.CustomType;
import com.hybris.cockpitng.util.notifications.NotificationService;
import com.hybris.cockpitng.widgets.configurableflow.FlowActionHandlerAdapter;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;
import java.util.Map;

/**
 * Inventory Cycle Count Scan window Handler
 *
 * @author Namrata Lohar
 **/
public class InventoryCycleCountScanToolHandler implements com.hybris.cockpitng.widgets.configurableflow.FlowActionHandler {

    private static final Logger LOG = Logger.getLogger(InventoryCycleCountScanToolHandler.class);

    private NotificationService notificationService;
    private BlInventoryCycleCountService blInventoryCycleCountService;

    @Autowired
    private InventoryCycleCountScanToolUtil inventoryCycleCountScanToolUtil;

    @Autowired
    private InventoryCycleCountScanToolRenderer inventoryCycleCountScanToolRenderer;

    /**
     * OOB method which will perform actions on input barcodes form backoffice wizard
     *
     * @param customType type
     * @param flowActionHandlerAdapter adapter
     * @param map map
     */
    @Override
    public void perform(final CustomType customType, final FlowActionHandlerAdapter flowActionHandlerAdapter, final Map<String, String> map) {
        final InventoryCycleCountScanToolData inventoryCycleCountScanToolData = (InventoryCycleCountScanToolData)
                flowActionHandlerAdapter.getWidgetInstanceManager().getModel().getValue((String) map.get(
                BlInventoryScanLoggingConstants.INVENTORY_CYCLE_COUNT_SCAN_TOOL_DATA_MODEL_KEY), InventoryCycleCountScanToolData.class);
        if (inventoryCycleCountScanToolData == null) {
            BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG);
            this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.ICC_NOTIFICATION_HANDLER,
                    BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE, NotificationEvent.Level.FAILURE, StringUtils.EMPTY);
        } else {
            this.getNotificationService().clearNotifications(BlInventoryScanLoggingConstants.ICC_NOTIFICATION_HANDLER);
            final List<String> skuList = inventoryCycleCountScanToolData.getSkusInputField();
            final List<String> serialBarcodes = inventoryCycleCountScanToolData.getSerialBarcodeInputField();
            if (CollectionUtils.isNotEmpty(skuList) && CollectionUtils.isNotEmpty(serialBarcodes)) {
                if(this.getBlInventoryCycleCountService().getActiveInventoryCycleCount() != null) {
                    this.executeInventoryCycleCount(skuList, serialBarcodes);
                } else {
                    BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.PREVIOUS_CYCLE_COUNT_ENDED);
                    this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.ICC_NOTIFICATION_HANDLER,
                            BlInventoryScanLoggingConstants.ICC_NO_ACTIVE_NOTIFICATION_FAILURE, NotificationEvent.Level.FAILURE, StringUtils.EMPTY);
                }
            } else {
                BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.ICC_INPUT_EMPTY_ERROR_NOTIF_MSG);
                this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.ICC_NOTIFICATION_HANDLER,
                        BlInventoryScanLoggingConstants.ICC_INPUT_EMPTY_ERROR_NOTIF, NotificationEvent.Level.FAILURE, StringUtils.EMPTY);
            }
            this.triggerClear(inventoryCycleCountScanToolData);
        }
    }

    /**
     * This method will execute inventory cycle count for the day
     *
     * @param skuList sku's
     * @param serialBarcodes barcodes
     */
    private void executeInventoryCycleCount(final List<String> skuList, final List<String> serialBarcodes) {
        if(Boolean.TRUE.equals(getBlInventoryCycleCountService().checkIsSKUListMatching(skuList))) {
            try {
                final String result = this.getBlInventoryCycleCountService().executeInventoryCycleCount(serialBarcodes);
                if(StringUtils.isNotEmpty(result)) {
                    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.ICC_SUCCESS, result,
                        this.getBlInventoryCycleCountService().getInventoryDayCode(), this.getBlInventoryCycleCountService()
                                    .getInventoryDayDate());
                    this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.ICC_NOTIFICATION_HANDLER,
                            BlInventoryScanLoggingConstants.ICC_SUCCESS_NOTIF, NotificationEvent.Level.SUCCESS, result,
                            this.getBlInventoryCycleCountService().getInventoryDayCode(), this.getBlInventoryCycleCountService()
                            .getInventoryDayDate());
                } else {
                    BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.ICC_NO_SERIALS);
                    this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.ICC_NOTIFICATION_HANDLER,
                            BlInventoryScanLoggingConstants.ICC_SERIAL_DB_ERROR_NOTIF, NotificationEvent.Level.FAILURE, StringUtils.EMPTY);
                }
            } catch(final ModelSavingException e) {
                BlLogger.logFormatMessageInfo(LOG, Level.ERROR, BlInventoryScanLoggingConstants.MODEL_SAVING_EXCEPTION, e.getMessage());
                this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.ICC_NOTIFICATION_HANDLER,
                        BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE, NotificationEvent.Level.FAILURE, StringUtils.EMPTY);
            }
        } else {
            BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.SKU_LIST_ERROR);
            this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.ICC_NOTIFICATION_HANDLER,
                    BlInventoryScanLoggingConstants.ICC_SERIAL_ERROR_NOTIF, NotificationEvent.Level.FAILURE, StringUtils.EMPTY);
        }
    }


    /**
     * This method will clear text box contents and set updated list to the WebScanToolData
     *
     * @param inventoryCycleCountScanToolData data
     */
    public void triggerClear(final InventoryCycleCountScanToolData inventoryCycleCountScanToolData) {
        new InventoryCycleCountScanToolRenderer().triggerClear(inventoryCycleCountScanToolData, this.getInventoryCycleCountScanToolUtil());
    }

    public NotificationService getNotificationService() {
        return notificationService;
    }

    public void setNotificationService(final NotificationService notificationService) {
        this.notificationService = notificationService;
    }

    public InventoryCycleCountScanToolUtil getInventoryCycleCountScanToolUtil() {
        return inventoryCycleCountScanToolUtil;
    }

    public void setInventoryCycleCountScanToolUtil(InventoryCycleCountScanToolUtil inventoryCycleCountScanToolUtil) {
        this.inventoryCycleCountScanToolUtil = inventoryCycleCountScanToolUtil;
    }

    public InventoryCycleCountScanToolRenderer getInventoryCycleCountScanToolRenderer() {
        return inventoryCycleCountScanToolRenderer;
    }

    public void setInventoryCycleCountScanToolRenderer(InventoryCycleCountScanToolRenderer inventoryCycleCountScanToolRenderer) {
        this.inventoryCycleCountScanToolRenderer = inventoryCycleCountScanToolRenderer;
    }

    public BlInventoryCycleCountService getBlInventoryCycleCountService() {
        return blInventoryCycleCountService;
    }

    public void setBlInventoryCycleCountService(BlInventoryCycleCountService blInventoryCycleCountService) {
        this.blInventoryCycleCountService = blInventoryCycleCountService;
    }
}
