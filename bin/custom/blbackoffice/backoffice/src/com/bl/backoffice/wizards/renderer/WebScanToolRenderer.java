package com.bl.backoffice.wizards.renderer;

import com.bl.backoffice.wizards.util.WebScanToolData;
import com.bl.backoffice.wizards.util.WebScanToolUtil;
import com.bl.constants.BLInventoryScanLoggingConstants;
import com.hybris.backoffice.widgets.notificationarea.event.NotificationEvent;
import com.hybris.cockpitng.config.jaxb.wizard.ViewType;
import com.hybris.cockpitng.dataaccess.facades.type.DataType;
import com.hybris.cockpitng.engine.WidgetInstanceManager;
import com.hybris.cockpitng.util.notifications.NotificationService;
import com.hybris.cockpitng.widgets.configurableflow.renderer.DefaultCustomViewRenderer;
import org.zkoss.zk.ui.Component;
import org.zkoss.zul.Textbox;

import java.util.Map;

public class WebScanToolRenderer extends DefaultCustomViewRenderer {

    private NotificationService notificationService;
    private WebScanToolUtil webScanToolUtil;

    public NotificationService getNotificationService() {
        return notificationService;
    }

    public void setNotificationService(NotificationService notificationService) {
        this.notificationService = notificationService;
    }

    public WebScanToolUtil getWebScanToolUtil() {
        return webScanToolUtil;
    }

    public void setWebScanToolUtil(WebScanToolUtil webScanToolUtil) {
        this.webScanToolUtil = webScanToolUtil;
    }

    @Override
    public void render(Component component, ViewType viewType, Map<String, String> map, DataType dataType, WidgetInstanceManager widgetInstanceManager) {
        WebScanToolData webScanToolData = (WebScanToolData) widgetInstanceManager.getModel().
                getValue((String) map.get(BLInventoryScanLoggingConstants.WEB_SCAN_TOOL_DATA_MODEL_KEY), WebScanToolData.class);
        if (webScanToolData == null) {
            this.getNotificationService().notifyUser(BLInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                    BLInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE, NotificationEvent.Level.FAILURE,
                    new Object[0]);
        } else {
            Textbox barcodeInputField = new Textbox();
            barcodeInputField.setRows(BLInventoryScanLoggingConstants.SEVEN);
            barcodeInputField.setCols(BLInventoryScanLoggingConstants.FORTY);
            barcodeInputField.setMultiline(true);
            barcodeInputField.setHeight(BLInventoryScanLoggingConstants.HUN_PER);
            barcodeInputField.setWidth(BLInventoryScanLoggingConstants.HUN_PER);

            barcodeInputField.addEventListener("onChange", event ->
                    this.webScanToolUtil.onBarcodeInputFieldTextChanged(barcodeInputField, webScanToolData));
            component.appendChild(barcodeInputField);
        }
    }
}
