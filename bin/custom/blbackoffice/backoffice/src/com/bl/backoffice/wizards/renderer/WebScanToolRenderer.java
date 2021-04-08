package com.bl.backoffice.wizards.renderer;

import com.bl.backoffice.wizards.util.WebScanToolData;
import com.bl.backoffice.wizards.util.WebScanToolUtil;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.hybris.backoffice.widgets.notificationarea.event.NotificationEvent;
import com.hybris.cockpitng.config.jaxb.wizard.ViewType;
import com.hybris.cockpitng.dataaccess.facades.type.DataType;
import com.hybris.cockpitng.engine.WidgetInstanceManager;
import com.hybris.cockpitng.util.notifications.NotificationService;
import com.hybris.cockpitng.widgets.configurableflow.renderer.DefaultCustomViewRenderer;
import org.apache.commons.lang.StringUtils;
import org.zkoss.zk.ui.Component;
import org.zkoss.zul.Textbox;

import java.util.Map;

/**
 * {javadoc}
 *
 * @author Namrata Lohar
 **/
public class WebScanToolRenderer extends DefaultCustomViewRenderer {

    private NotificationService notificationService;
    private WebScanToolUtil webScanToolUtil;

    @Override
    public void render(final Component component, final ViewType viewType, final Map<String, String> map,
                       final DataType dataType, final WidgetInstanceManager widgetInstanceManager) {
        final WebScanToolData webScanToolData = (WebScanToolData) widgetInstanceManager.getModel().
                getValue((String) map.get(BlInventoryScanLoggingConstants.WEB_SCAN_TOOL_DATA_MODEL_KEY), WebScanToolData.class);
        if (webScanToolData == null) {
            this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                    BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE, NotificationEvent.Level.FAILURE,
                    StringUtils.EMPTY);
        } else {
            final Textbox barcodeInputField = new Textbox();
            barcodeInputField.setRows(BlInventoryScanLoggingConstants.SEVEN);
            barcodeInputField.setCols(BlInventoryScanLoggingConstants.FORTY);
            barcodeInputField.setMultiline(true);
            barcodeInputField.setHeight(BlInventoryScanLoggingConstants.HUN_PER);
            barcodeInputField.setWidth(BlInventoryScanLoggingConstants.HUN_PER);

            barcodeInputField.addEventListener("onChange", event ->
                    this.webScanToolUtil.onBarcodeInputFieldTextChanged(barcodeInputField, webScanToolData));

            component.appendChild(barcodeInputField);
        }
    }

    public NotificationService getNotificationService() {
        return notificationService;
    }

    public void setNotificationService(final NotificationService notificationService) {
        this.notificationService = notificationService;
    }

    public WebScanToolUtil getWebScanToolUtil() {
        return webScanToolUtil;
    }

    public void setWebScanToolUtil(final WebScanToolUtil webScanToolUtil) {
        this.webScanToolUtil = webScanToolUtil;
    }

}
