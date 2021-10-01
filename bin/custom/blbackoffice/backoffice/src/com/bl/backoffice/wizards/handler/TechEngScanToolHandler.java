package com.bl.backoffice.wizards.handler;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.backoffice.wizards.util.WebScanToolData;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.inventory.scan.service.BlInventoryScanToolService;
import com.bl.core.utils.BlInventoryScanUtility;
import com.bl.logging.BlLogger;
import com.hybris.backoffice.widgets.notificationarea.event.NotificationEvent;
import com.hybris.cockpitng.config.jaxb.wizard.CustomType;
import com.hybris.cockpitng.util.notifications.NotificationService;
import com.hybris.cockpitng.widgets.configurableflow.FlowActionHandler;
import com.hybris.cockpitng.widgets.configurableflow.FlowActionHandlerAdapter;


/**
 * Handler for scanning tool for Tech. Eng. Member
 *
 * @author Ravikumar
 **/
public class TechEngScanToolHandler implements FlowActionHandler
{

	private static final Logger LOG = Logger.getLogger(TechEngScanToolHandler.class);

	private NotificationService notificationService;
	private BlInventoryScanToolService blInventoryScanToolService;

	/**
	 * Perform action on scanning process.
	 *
	 * @param customType
	 *           the custom type
	 * @param flowActionHandlerAdapter
	 *           the flow action handler adapter
	 * @param dataMap
	 *           the data map
	 */
	@Override
	public void perform(final CustomType customType, final FlowActionHandlerAdapter flowActionHandlerAdapter,
			final Map<String, String> dataMap)
	{
		final WebScanToolData webScanToolData = flowActionHandlerAdapter.getWidgetInstanceManager().getModel()
				.getValue(dataMap.get(BlInventoryScanLoggingConstants.WEB_SCAN_TOOL_DATA_MODEL_KEY), WebScanToolData.class);

		if (Objects.isNull(webScanToolData))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
					StringUtils.EMPTY);
			this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.TECH_ENG_NOTIFICATION_HANDLER,
					BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE, NotificationEvent.Level.FAILURE,
					StringUtils.EMPTY);
		}
		else
		{
			final List<String> barcodes = webScanToolData.getBarcodeInputField();
			if (CollectionUtils.isEmpty(barcodes))
			{
				BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
						StringUtils.EMPTY);
				this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.TECH_ENG_NOTIFICATION_HANDLER,
						BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
						StringUtils.EMPTY);
			}
			else
			{
				final List<String> filteredBarcode = new ArrayList<>();
				barcodes.forEach(barcode -> {
					if (StringUtils.isNotBlank(barcode))
					{
						filteredBarcode.add(barcode.trim());
					}
				});
				createResponseForScanResult(filteredBarcode);
			}
		}
	}


	/**
	 * Creates the response for scan result.
	 *
	 * @param barcodes
	 *           the barcodes
	 */
	private void createResponseForScanResult(final List<String> barcodes)
	{
		if (barcodes.size() >= BlInventoryScanLoggingConstants.TWO)
		{
			if (getBlInventoryScanToolService().checkIfFirstEntryIsLocation(barcodes))
			{
				BlLogger.logFormatMessageInfo(LOG, Level.ERROR, BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
						StringUtils.EMPTY);
				this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.TECH_ENG_NOTIFICATION_HANDLER,
						BlInventoryScanLoggingConstants.FIRST_SCAN_LOCATION_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
						StringUtils.EMPTY);
			}
			else
			{
				createResponseMsgForScan(getBlInventoryScanToolService().isValidTechEngLocationBarcode(barcodes,
						BlInventoryScanUtility.getTechEngAllowedLocations()), barcodes);
			}
		}
		else
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
					BlInventoryScanLoggingConstants.SCAN_STRING + barcodes);
			this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.TECH_ENG_NOTIFICATION_HANDLER,
					BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
					BlInventoryScanLoggingConstants.SCAN_STRING + barcodes);
		}
	}

	/**
	 * Creates the response message for scan by performing the location update on serials.
	 *
	 * @param result
	 *           the result
	 * @param barcodes
	 *           the barcodes
	 */
	private void createResponseMsgForScan(final int result, final List<String> barcodes)
	{
		switch (result)
		{
			case BlInventoryScanLoggingConstants.ONE:
				executeLocationUpdate(barcodes);
				break;
			case BlInventoryScanLoggingConstants.TWO:
				BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE_MSG,
						StringUtils.EMPTY);
				addMessageToNotifyUser(BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE,
						NotificationEvent.Level.FAILURE, barcodes.get(barcodes.size() - BlInventoryScanLoggingConstants.ONE));
				break;
			case BlInventoryScanLoggingConstants.THREE:
				BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE_MSG,
						StringUtils.EMPTY);
				addMessageToNotifyUser(BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
						StringUtils.EMPTY);
				break;
			default:
				BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE_MSG,
						StringUtils.EMPTY);
				addMessageToNotifyUser(BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
						StringUtils.EMPTY);
				break;
		}
	}

	/**
	 * Execute location update if every thing is validated.
	 *
	 * @param barcodes
	 *           the barcodes
	 */
	private void executeLocationUpdate(final List<String> barcodes)
	{
		final Map<String, List<String>> failedBarcodeList = getBlInventoryScanToolService().doTechEngSerialLocationUpdate(barcodes);
		if (MapUtils.isNotEmpty(failedBarcodeList))
		{
			failedBarcodeList.forEach(this::doHandleErrorMessage);
		}
	}

	/**
	 * Do handle error message.
	 *
	 * @param errorBarcode
	 *           the error barcode
	 */
	private void doHandleErrorMessage(final String key, final List<String> errorBarcode)
	{
		switch (key)
		{
			case BlInventoryScanLoggingConstants.MISSING_BARCODE_ITEMS:
				doNotifyUser(BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE, NotificationEvent.Level.FAILURE, errorBarcode);
				break;
			case BlInventoryScanLoggingConstants.WRONG_ITEM_CLEAN_CART:
				doNotifyUser(BlInventoryScanLoggingConstants.WRONG_CLEAN_CART_LOCATION,
						BlInventoryScanLoggingConstants.CLEAN_CART_SCAN_ERROR_FAILURE, NotificationEvent.Level.FAILURE, errorBarcode);
				break;
			case BlInventoryScanLoggingConstants.WRONG_ITEM_CLEAN_PRIORITY_CART:
				doNotifyUser(BlInventoryScanLoggingConstants.WRONG_CLEAN_PRIORITY_CART_LOCATION,
						BlInventoryScanLoggingConstants.CLEAN_PRIORITY_CART_SCAN_ERROR_FAILURE, NotificationEvent.Level.FAILURE,
						errorBarcode);
				break;
			case BlInventoryScanLoggingConstants.SUCCESS:
				BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG,
						StringUtils.EMPTY);
				addMessageToNotifyUser(BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS, NotificationEvent.Level.SUCCESS,
						StringUtils.EMPTY);
				break;
			default:
				doNotifyUser(BlInventoryScanLoggingConstants.LOG_SOMETHING_WENT_WRONG,
						BlInventoryScanLoggingConstants.SCAN_ERROR_FAILURE, NotificationEvent.Level.FAILURE, errorBarcode);
				break;
		}
	}

	/**
	 * Do notify user.
	 *
	 * @param logMessage
	 *           the log message
	 * @param notifyErrorType
	 *           the notify error type
	 * @param notificationEventLevel
	 *           the notification event level
	 * @param barcodeList
	 *           the barcode list
	 */
	private void doNotifyUser(final String logMessage, final String notifyErrorType,
			final NotificationEvent.Level notificationEventLevel, final List<String> barcodeList)
	{
		BlLogger.logFormatMessageInfo(LOG, Level.ERROR, logMessage, barcodeList);
		addMessageToNotifyUser(notifyErrorType, notificationEventLevel, barcodeList);
	}

	/**
	 * Adds the message to notify user.
	 *
	 * @param errorType
	 *           the error type
	 * @param notificationEventLevel
	 *           the notification event level
	 * @param referenceObjects
	 *           the reference objects
	 */
	private void addMessageToNotifyUser(final String errorType, final NotificationEvent.Level notificationEventLevel,
			final Object... referenceObjects)
	{
		this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.TECH_ENG_NOTIFICATION_HANDLER, errorType,
				notificationEventLevel, referenceObjects);
	}

	/**
	 * Gets the notification service.
	 *
	 * @return the notification service
	 */
	public NotificationService getNotificationService()
	{
		return notificationService;
	}

	/**
	 * Sets the notification service.
	 *
	 * @param notificationService
	 *           the new notification service
	 */
	public void setNotificationService(final NotificationService notificationService)
	{
		this.notificationService = notificationService;
	}

	/**
	 * Gets the bl inventory scan tool service.
	 *
	 * @return the bl inventory scan tool service
	 */
	public BlInventoryScanToolService getBlInventoryScanToolService()
	{
		return blInventoryScanToolService;
	}

	/**
	 * Sets the bl inventory scan tool service.
	 *
	 * @param blInventoryScanToolService
	 *           the new bl inventory scan tool service
	 */
	public void setBlInventoryScanToolService(final BlInventoryScanToolService blInventoryScanToolService)
	{
		this.blInventoryScanToolService = blInventoryScanToolService;
	}
}
