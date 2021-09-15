package com.bl.backoffice.wizards.renderer;

import com.bl.backoffice.wizards.util.InventoryCycleCountScanToolData;
import com.bl.backoffice.wizards.util.InventoryCycleCountScanToolUtil;
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

import javax.swing.*;
import java.awt.*;
import java.util.Map;

/**
 * {javadoc}
 *
 * @author Namrata Lohar
 **/
public class InventoryCycleCountScanToolRenderer extends DefaultCustomViewRenderer {

    private static final Logger LOG = Logger.getLogger(InventoryCycleCountScanToolRenderer.class);
    public static final String ON_CHANGE = "onChange";
    public static final String RESIZE_NONE_DISPLAY_BLOCK = "resize:none;display:block";
    public static final String CLEAR = "Clear";
    public static final String ON_CLICK = "onClick";
    public static final String DEFAULT_INVENTORY_CYCLE_COUNT_SCAN_TOOL_RENDERER = "defaultInventoryCycleCountScanToolRenderer";

    private NotificationService notificationService;
    private InventoryCycleCountScanToolUtil inventoryCycleCountScanToolUtil;

    private InputElement newPwdInput;
    private InputElement newSKUInput;

    /**
     * @param component component
     * @param viewType type
     * @param map map
     * @param dataType type
     * @param widgetInstanceManager manager
     * OOB method which will render custom UI in backoffice popup for scanning purpose
     */
    @Override
    public void render(final Component component, final ViewType viewType, final Map<String, String> map,
                       final DataType dataType, final WidgetInstanceManager widgetInstanceManager) {
        final InventoryCycleCountScanToolData inventoryCycleCountScanToolData = (InventoryCycleCountScanToolData) widgetInstanceManager
                .getModel().getValue((String) map.get(BlInventoryScanLoggingConstants.INVENTORY_CYCLE_COUNT_SCAN_TOOL_DATA_MODEL_KEY),
                InventoryCycleCountScanToolData.class);
        if (inventoryCycleCountScanToolData == null) {
            BlLogger.logFormatMessageInfo(LOG, Level.INFO,BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
                    StringUtils.EMPTY);
            this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.ICC_NOTIFICATION_HANDLER,
                    BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE, NotificationEvent.Level.FAILURE,
                    StringUtils.EMPTY);
        } else {
            JFrame jFrame = new JFrame();

            GridLayout experimentLayout = new GridLayout(0,2);

            final Textbox skuInputField = new Textbox();
            skuInputField.setRows(BlInventoryScanLoggingConstants.FIFTEEN);
            skuInputField.setCols(BlInventoryScanLoggingConstants.FORTY);
            skuInputField.setMultiline(true);
            skuInputField.setHeight(BlInventoryScanLoggingConstants.HUN_PER);
            skuInputField.setWidth(BlInventoryScanLoggingConstants.HUN_PER);
            skuInputField.setStyle(RESIZE_NONE_DISPLAY_BLOCK);
            skuInputField.addEventListener(ON_CHANGE, event -> {
                this.getInventoryCycleCountScanToolUtil().onSKUsInputFieldTextChanged(skuInputField, inventoryCycleCountScanToolData);
                this.setNewSKUInput(skuInputField);
            });
            component.appendChild(skuInputField);

            final Textbox barcodeInputField = new Textbox();
            barcodeInputField.setRows(BlInventoryScanLoggingConstants.FIFTEEN);
            barcodeInputField.setCols(BlInventoryScanLoggingConstants.FORTY);
            barcodeInputField.setMultiline(true);
            barcodeInputField.setHeight(BlInventoryScanLoggingConstants.HUN_PER);
            barcodeInputField.setWidth(BlInventoryScanLoggingConstants.HUN_PER);
            barcodeInputField.setStyle(RESIZE_NONE_DISPLAY_BLOCK);
            barcodeInputField.addEventListener(ON_CHANGE, event -> {
                this.getInventoryCycleCountScanToolUtil().onSKUsInputFieldTextChanged(barcodeInputField, inventoryCycleCountScanToolData);
                this.setNewPwdInput(barcodeInputField);
            });
            component.appendChild(barcodeInputField);

            final Button clear = new Button();
            clear.setLabel(CLEAR);
            clear.setVisible(Boolean.FALSE);
            clear.setTabindex(BlInventoryScanLoggingConstants.TWO);
            clear.setStyle("margin-top:21px;float:left;margin-left: 122px;position: fixed;z-index: 1;");
            clear.addEventListener(ON_CLICK, event -> {
                skuInputField.setValue(StringUtils.EMPTY);
                barcodeInputField.setValue(StringUtils.EMPTY);
                this.getInventoryCycleCountScanToolUtil().onSKUsInputFieldTextChanged(barcodeInputField, inventoryCycleCountScanToolData);
                this.getInventoryCycleCountScanToolUtil().onSerialBarcodeInputFieldTextChanged(barcodeInputField, inventoryCycleCountScanToolData);
            });
        }
    }

    /**
     * This method will clear text box contents and set updated list to the WebScanToolData
     *
     * @param inventoryCycleCountScanToolData data
     * @param inventoryCycleCountScanToolUtil util
     */
    public void triggerClear(final InventoryCycleCountScanToolData inventoryCycleCountScanToolData,
                             final InventoryCycleCountScanToolUtil inventoryCycleCountScanToolUtil) {
        final InventoryCycleCountScanToolRenderer inventoryCycleCountScanToolRenderer = Registry.getApplicationContext()
                .getBean(DEFAULT_INVENTORY_CYCLE_COUNT_SCAN_TOOL_RENDERER, InventoryCycleCountScanToolRenderer.class);
        if(inventoryCycleCountScanToolRenderer != null) {
            final InputElement barcodeInput = inventoryCycleCountScanToolRenderer.getNewPwdInput();
            final InputElement skuInput = inventoryCycleCountScanToolRenderer.getNewSKUInput();
            if(barcodeInput != null && skuInput != null) {
                barcodeInput.setText(StringUtils.EMPTY);
                skuInput.setText(StringUtils.EMPTY);

                inventoryCycleCountScanToolUtil.onSerialBarcodeInputFieldTextChanged(barcodeInput, inventoryCycleCountScanToolData);
                inventoryCycleCountScanToolRenderer.setNewPwdInput(barcodeInput);

                inventoryCycleCountScanToolUtil.onSKUsInputFieldTextChanged(skuInput, inventoryCycleCountScanToolData);
                inventoryCycleCountScanToolRenderer.setNewSKUInput(skuInput);
            }
        }
    }

    public InputElement getNewPwdInput() {
        return newPwdInput;
    }

    public void setNewPwdInput(InputElement newPwdInput) {
        this.newPwdInput = newPwdInput;
    }

    public InputElement getNewSKUInput() {
        return newSKUInput;
    }

    public void setNewSKUInput(InputElement newSKUInput) {
        this.newSKUInput = newSKUInput;
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

}
