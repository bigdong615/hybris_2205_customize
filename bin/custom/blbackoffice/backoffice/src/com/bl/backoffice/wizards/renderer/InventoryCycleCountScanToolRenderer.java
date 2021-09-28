package com.bl.backoffice.wizards.renderer;

import com.bl.backoffice.wizards.util.InventoryCycleCountScanToolData;
import com.bl.backoffice.wizards.util.InventoryCycleCountScanToolUtil;
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
 * Inventory Cycle Count Renderer
 *
 * @author Namrata Lohar
 **/
public class InventoryCycleCountScanToolRenderer extends DefaultCustomViewRenderer {

    private static final Logger LOG = Logger.getLogger(InventoryCycleCountScanToolRenderer.class);

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
            BlLogger.logMessage(LOG, Level.INFO,BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG);
            this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.ICC_NOTIFICATION_HANDLER,
                    BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE, NotificationEvent.Level.FAILURE,
                    StringUtils.EMPTY);
        } else {
            final Textbox skuInputField = this.getSkuInputField(inventoryCycleCountScanToolData);
            component.appendChild(skuInputField);

            final Textbox barcodeInputField = this.getBarcodeInputField(inventoryCycleCountScanToolData);
            component.appendChild(barcodeInputField);

            final Button clear = new Button();
            clear.setLabel(BlInventoryScanLoggingConstants.CLEAR);
            clear.setVisible(Boolean.FALSE);
            clear.setTabindex(BlInventoryScanLoggingConstants.TWO);
            clear.setStyle(BlInventoryScanLoggingConstants.MARGIN_TOP_21_PX_FLOAT_LEFT_MARGIN_LEFT_122_PX_POSITION_FIXED_Z_INDEX_1);
            clear.addEventListener(BlInventoryScanLoggingConstants.ON_CLICK, event -> {
                skuInputField.setValue(StringUtils.EMPTY);
                barcodeInputField.setValue(StringUtils.EMPTY);
                this.getInventoryCycleCountScanToolUtil().onSKUsInputFieldTextChanged(barcodeInputField, inventoryCycleCountScanToolData);
                this.getInventoryCycleCountScanToolUtil().onSerialBarcodeInputFieldTextChanged(barcodeInputField, inventoryCycleCountScanToolData);
            });
        }
    }

    /**
     * This method will create barcode input field
     *
     * @param inventoryCycleCountScanToolData data
     * @return text box
     */
    private Textbox getBarcodeInputField(final InventoryCycleCountScanToolData inventoryCycleCountScanToolData) {
        final Textbox barcodeInputField = new Textbox();
        barcodeInputField.setRows(BlInventoryScanLoggingConstants.FIFTEEN);
        barcodeInputField.setCols(BlInventoryScanLoggingConstants.FORTY);
        barcodeInputField.setMultiline(true);
        barcodeInputField.setHeight(BlInventoryScanLoggingConstants.HUN_PER);
        barcodeInputField.setWidth(BlInventoryScanLoggingConstants.WIDTH_FOURTY_EIGHT);
        barcodeInputField.setPlaceholder(BlInventoryScanLoggingConstants.BARCODE_INPUT_PLACEHOLDER_SCAN_SERIAL_BARCODES);
        barcodeInputField.setStyle(BlInventoryScanLoggingConstants.RESIZE_NONE_DISPLAY_BLOCK_SERIAL);
        barcodeInputField.addEventListener(BlInventoryScanLoggingConstants.ON_CHANGE, event -> {
            this.getInventoryCycleCountScanToolUtil().onSerialBarcodeInputFieldTextChanged(barcodeInputField, inventoryCycleCountScanToolData);
            this.setNewPwdInput(barcodeInputField);
        });
        return barcodeInputField;
    }

    /**
     * This method will create SKU input field
     *
     * @param inventoryCycleCountScanToolData data
     * @return text box
     */
    private Textbox getSkuInputField(final InventoryCycleCountScanToolData inventoryCycleCountScanToolData) {
        final Textbox skuInputField = new Textbox();
        skuInputField.setRows(BlInventoryScanLoggingConstants.FIFTEEN);
        skuInputField.setCols(BlInventoryScanLoggingConstants.FORTY);
        skuInputField.setMultiline(true);
        skuInputField.setHeight(BlInventoryScanLoggingConstants.HUN_PER);
        skuInputField.setWidth(BlInventoryScanLoggingConstants.WIDTH_FOURTY_EIGHT);
        skuInputField.setPlaceholder(BlInventoryScanLoggingConstants.SKU_INPUT_PLACEHOLDER_ENTER_SKU_LIST);
        skuInputField.setStyle(BlInventoryScanLoggingConstants.RESIZE_NONE_DISPLAY_BLOCK_SKU);
        skuInputField.addEventListener(BlInventoryScanLoggingConstants.ON_CHANGE, event -> {
            this.getInventoryCycleCountScanToolUtil().onSKUsInputFieldTextChanged(skuInputField, inventoryCycleCountScanToolData);
            this.setNewSKUInput(skuInputField);
        });
        return skuInputField;
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
                .getBean(BlInventoryScanLoggingConstants.DEFAULT_INVENTORY_CYCLE_COUNT_SCAN_TOOL_RENDERER,
                InventoryCycleCountScanToolRenderer.class);
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
