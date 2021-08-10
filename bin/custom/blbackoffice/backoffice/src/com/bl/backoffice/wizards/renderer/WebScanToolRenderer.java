package com.bl.backoffice.wizards.renderer;

import com.bl.backoffice.wizards.util.WebScanToolData;
import com.bl.backoffice.wizards.util.WebScanToolUtil;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.logging.BlLogger;
import com.hybris.backoffice.widgets.notificationarea.event.NotificationEvent;
import com.hybris.cockpitng.config.jaxb.wizard.ViewType;
import com.hybris.cockpitng.dataaccess.facades.type.DataType;
import com.hybris.cockpitng.engine.WidgetInstanceManager;
import com.hybris.cockpitng.util.notifications.NotificationService;
import com.hybris.cockpitng.widgets.configurableflow.renderer.DefaultCustomViewRenderer;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.Component;
import org.zkoss.zul.Button;
import org.zkoss.zul.Textbox;

import java.util.Map;

/**
 * {javadoc}
 *
 * @author Namrata Lohar
 **/
public class WebScanToolRenderer extends DefaultCustomViewRenderer {

    private static final Logger LOG = Logger.getLogger(WebScanToolRenderer.class);

    private NotificationService notificationService;
    private WebScanToolUtil webScanToolUtil;

    /**
     * javadoc
     * @param component
     * @param viewType
     * @param map
     * @param dataType
     * @param widgetInstanceManager
     * OOB method which will render custom UI in backoffice popup for scanning purpose
     */
    @Override
    public void render(final Component component, final ViewType viewType, final Map<String, String> map,
                       final DataType dataType, final WidgetInstanceManager widgetInstanceManager) {
        final WebScanToolData webScanToolData = (WebScanToolData) widgetInstanceManager.getModel().
                getValue((String) map.get(BlInventoryScanLoggingConstants.WEB_SCAN_TOOL_DATA_MODEL_KEY), WebScanToolData.class);
        if (webScanToolData == null) {
            BlLogger.logFormatMessageInfo(LOG, Level.INFO,BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
                    StringUtils.EMPTY);
            this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.NOTIFICATION_HANDLER,
                    BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE, NotificationEvent.Level.FAILURE,
                    StringUtils.EMPTY);
        } else {
            final Textbox barcodeInputField = new Textbox();
            barcodeInputField.setRows(BlInventoryScanLoggingConstants.TEN);
            barcodeInputField.setCols(BlInventoryScanLoggingConstants.FORTY);
            barcodeInputField.setMultiline(true);
            barcodeInputField.setHeight(BlInventoryScanLoggingConstants.HUN_PER);
            barcodeInputField.setWidth(BlInventoryScanLoggingConstants.HUN_PER);
            barcodeInputField.setStyle("resize:none;display:block");

            barcodeInputField.addEventListener("onChange", event ->
                    this.webScanToolUtil.onBarcodeInputFieldTextChanged(barcodeInputField, webScanToolData));
            component.appendChild(barcodeInputField);

            final Button clear = new Button();
            clear.setLabel("Clear");
            clear.setVisible(Boolean.TRUE);
            clear.setStyle("margin-top:66px;float:left;margin-left: -10px;position: fixed;z-index: 1;");
            clear.addEventListener("onClick", event -> {
                barcodeInputField.setValue(StringUtils.EMPTY);
                this.webScanToolUtil.onBarcodeInputFieldTextChanged(barcodeInputField, webScanToolData);
            });
            component.appendChild(clear);
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
