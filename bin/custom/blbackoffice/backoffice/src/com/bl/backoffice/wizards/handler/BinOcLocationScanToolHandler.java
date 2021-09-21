package com.bl.backoffice.wizards.handler;

import com.bl.backoffice.wizards.util.WebScanToolData;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.enums.InventoryLocationTypeEnum;
import com.bl.core.inventory.scan.service.BlInventoryScanToolService;
import com.bl.core.model.BlInventoryLocationModel;
import com.bl.logging.BlLogger;
import com.hybris.backoffice.widgets.notificationarea.event.NotificationEvent;
import com.hybris.cockpitng.config.jaxb.wizard.CustomType;
import com.hybris.cockpitng.util.notifications.NotificationService;
import com.hybris.cockpitng.widgets.configurableflow.FlowActionHandler;
import com.hybris.cockpitng.widgets.configurableflow.FlowActionHandlerAdapter;
import java.util.Map;
import java.util.Objects;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This class is added for scanning the bin location and
 * remove reference from serial and parent location
 * @author Ritika
 *
 */
public class BinOcLocationScanToolHandler implements FlowActionHandler {

  private static final Logger LOG = Logger.getLogger(BinOcLocationScanToolHandler.class);

  private NotificationService notificationService;
  private BlInventoryScanToolService blInventoryScanToolService;

  /**
   * OOB method which will perform actions on input barcodes form backoffice wizard
   * @param customType
   * @param flowActionHandlerAdapter
   * @param map
   */
  @Override
  public void perform(final CustomType customType, final FlowActionHandlerAdapter flowActionHandlerAdapter, final Map<String, String> map) {
    final WebScanToolData webScanToolData = flowActionHandlerAdapter.getWidgetInstanceManager().getModel().
        getValue(map.get(BlInventoryScanLoggingConstants.WEB_SCAN_TOOL_DATA_MODEL_KEY), WebScanToolData.class);

    if (webScanToolData == null) {
      BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG, StringUtils.EMPTY);
      this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.BIN_OCLOCATION_NOTIFICATION_HANDLER,BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE, NotificationEvent.Level.FAILURE, StringUtils.EMPTY);
    }
    else {
      this.getNotificationService().clearNotifications(BlInventoryScanLoggingConstants.NOTIFICATION_HANDLER);
      if (CollectionUtils.isNotEmpty(webScanToolData.getBarcodeInputField()) && webScanToolData.getBarcodeInputField().size() == 1 && StringUtils.isNotBlank(webScanToolData.getBarcodeInputField().get(0))) {
        final String locationId = webScanToolData.getBarcodeInputField().get(0);
        createResponseForScanResult(locationId);
      } else {
        BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.MUST_ONE_LOCATION_ERROR_FAILURE,StringUtils.EMPTY);
        this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.BIN_OCLOCATION_NOTIFICATION_HANDLER, BlInventoryScanLoggingConstants.MUST_ONE_LOCATION_ERROR_FAILURE, NotificationEvent.Level.FAILURE, StringUtils.EMPTY);
      }
    }
  }

  /**
   * Create Response from scan Result
   * @param locationCode
   */

  private void createResponseForScanResult(final String locationCode) {
    final BlInventoryLocationModel blInventoryLocation = getBlInventoryScanToolService().getInventoryLocationById(locationCode);
    if(Objects.nonNull(blInventoryLocation) && InventoryLocationTypeEnum.BIN.equals(blInventoryLocation.getInventoryType())){
      clearBinRelationForParentLocationAndSerial(blInventoryLocation);
    }
    else if(Objects.nonNull(blInventoryLocation) && !InventoryLocationTypeEnum.BIN.equals(blInventoryLocation.getInventoryType())) {
      BlLogger.logFormatMessageInfo(LOG, Level.ERROR, BlInventoryScanLoggingConstants.TYPE_NOT_SUPPORTED, locationCode);
      this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.BIN_OCLOCATION_NOTIFICATION_HANDLER, BlInventoryScanLoggingConstants.TYPE_NOT_SUPPORTED_ERROR_FAILURE, NotificationEvent.Level.FAILURE, StringUtils.EMPTY);
    }

    else{
      BlLogger.logFormatMessageInfo(LOG, Level.ERROR, BlInventoryScanLoggingConstants.LOCATION_NOT_FOUND, locationCode);
      this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.BIN_OCLOCATION_NOTIFICATION_HANDLER, BlInventoryScanLoggingConstants.VALID_BIN_LOCATION_ERROR_FAILURE, NotificationEvent.Level.FAILURE, StringUtils.EMPTY);
    }

  }

  /**
   *  Clear Bin Contents for related serials and parent location
   * @param blInventoryLocation
   */
  private void clearBinRelationForParentLocationAndSerial(final BlInventoryLocationModel blInventoryLocation) {
    getBlInventoryScanToolService().removeSerialsAndParentLocationFromBinOcLocation(blInventoryLocation);
    BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG,
        StringUtils.EMPTY);
    this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.BIN_OCLOCATION_NOTIFICATION_HANDLER,BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS, NotificationEvent.Level.SUCCESS,
        StringUtils.EMPTY);
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

