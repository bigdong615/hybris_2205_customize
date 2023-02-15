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
import de.hybris.platform.core.Registry;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.Component;
import org.zkoss.zul.Button;
import org.zkoss.zul.Textbox;
import org.zkoss.zul.impl.InputElement;

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

    private InputElement newPwdInput;


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
            barcodeInputField.setRows(BlInventoryScanLoggingConstants.FIFTEEN);
            barcodeInputField.setCols(BlInventoryScanLoggingConstants.FORTY);
            barcodeInputField.setMultiline(true);
            barcodeInputField.setHeight(BlInventoryScanLoggingConstants.HUN_PER);
            barcodeInputField.setWidth(BlInventoryScanLoggingConstants.HUN_PER);
            barcodeInputField.setStyle("resize:none;display:block");

            barcodeInputField.addEventListener("onChange", event -> {
                this.webScanToolUtil.onBarcodeInputFieldTextChanged(barcodeInputField, webScanToolData);
                this.setNewPwdInput(barcodeInputField);
            });
            component.appendChild(barcodeInputField);

            final Button clear = new Button();
            clear.setLabel("Clear");
            clear.setVisible(Boolean.FALSE);
            clear.setTabindex(BlInventoryScanLoggingConstants.TWO);
            clear.setStyle("margin-top:21px;float:left;margin-left: 122px;position: fixed;z-index: 1;");

            clear.addEventListener("onClick", event -> {
                barcodeInputField.setValue(StringUtils.EMPTY);
                this.getWebScanToolUtil().onBarcodeInputFieldTextChanged(barcodeInputField, webScanToolData);
            });
            //component.appendChild(clear);
        }
    }

    /**
     * javadoc
     *
     * This method will clear text box contents and set updated list to the WebScanToolData
     *
     * @param webScanToolData data
     * @param webScanToolUtil util
     */
    public void triggerClear(final WebScanToolData webScanToolData, final WebScanToolUtil webScanToolUtil) {
        final WebScanToolRenderer webScanToolRenderer = Registry.getApplicationContext().getBean("defaultWebScanToolRenderer",
                WebScanToolRenderer.class);
        if(webScanToolRenderer != null) {
            final InputElement barcodeInput = webScanToolRenderer.getNewPwdInput();
            if(barcodeInput != null) {
                barcodeInput.setFocus(Boolean.TRUE);
                barcodeInput.setText(StringUtils.EMPTY);
                webScanToolUtil.onBarcodeInputFieldTextChanged(barcodeInput, webScanToolData);
                webScanToolRenderer.setNewPwdInput(barcodeInput);
            }
        }
    }

    public InputElement getNewPwdInput() {
        return newPwdInput;
    }

    public void setNewPwdInput(InputElement newPwdInput) {
        this.newPwdInput = newPwdInput;
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
