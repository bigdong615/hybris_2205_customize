package com.bl.backoffice.wizards.handler;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.apache.commons.collections.MapUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.backoffice.wizards.util.WebScanToolData;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.inventory.scan.service.BlInventoryScanToolService;
import com.bl.logging.BlLogger;
import com.hybris.backoffice.widgets.notificationarea.event.NotificationEvent;
import com.hybris.cockpitng.config.jaxb.wizard.CustomType;
import com.hybris.cockpitng.util.notifications.NotificationService;
import com.hybris.cockpitng.widgets.configurableflow.FlowActionHandler;
import com.hybris.cockpitng.widgets.configurableflow.FlowActionHandlerAdapter;


/**
 * Handler for scanning tool for Unboxing Member
 *
 * @author Namrata Lohar
 **/
public class UnboxingScanToolHandler implements FlowActionHandler
{

	private static final Logger LOG = Logger.getLogger(UnboxingScanToolHandler.class);

	private NotificationService notificationService;
	private BlInventoryScanToolService blInventoryScanToolService;
	private static final NotificationEvent.Level NOTIFICATION_LEVEL_FAILURE = NotificationEvent.Level.FAILURE;
	private static final NotificationEvent.Level NOTIFICATION_LEVEL_WARNING = NotificationEvent.Level.WARNING;
	private static final NotificationEvent.Level NOTIFICATION_LEVEL_SUCCESS = NotificationEvent.Level.SUCCESS;

	/**
	 * values OOB method which will perform actions on input barcodes form backoffice wizard
	 *
	 * @param customType
	 *           type
	 * @param flowActionHandlerAdapter
	 *           handler
	 * @param map
	 */
	@Override
	public void perform(final CustomType customType, final FlowActionHandlerAdapter flowActionHandlerAdapter,
			final Map<String, String> map)
	{
		final WebScanToolData webScanToolData = flowActionHandlerAdapter.getWidgetInstanceManager().getModel()
				.getValue(map.get(BlInventoryScanLoggingConstants.WEB_SCAN_TOOL_DATA_MODEL_KEY), WebScanToolData.class);

		if (Objects.isNull(webScanToolData))
		{
			addMessageToNotifyUser(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
					BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE, NOTIFICATION_LEVEL_FAILURE, StringUtils.EMPTY);
		}
		else
		{
			final List<String> barcodes = webScanToolData.getBarcodeInputField();
			if (CollectionUtils.isEmpty(barcodes))
			{
				addMessageToNotifyUser(BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE, StringUtils.EMPTY);
			}
			else
			{
				createResponseForScanResult(barcodes);
			}
		}
	}

	/**
	 * method will check the input size and notify user accordingly with conditions
	 *
	 * @param barcodes
	 *           of list
	 */
	private void createResponseForScanResult(final List<String> barcodes)
	{
		if (barcodes.size() >= BlInventoryScanLoggingConstants.TWO)
		{
			createResponseMegForScan(getBlInventoryScanToolService().checkValidLocationInBarcodeListOfDPC(barcodes), barcodes);
		}
		else
		{
			addMessageToNotifyUser(BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
					BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE,
					BlInventoryScanLoggingConstants.SCAN_STRING + barcodes);
		}
	}

	/**
	 * method will notify user according to result number calculated in previous method
	 *
	 * @param result
	 *           result
	 * @param barcodes
	 *           list
	 */
	private void createResponseMegForScan(final int result, final List<String> barcodes)
	{

		switch (result)
		{
			case BlInventoryScanLoggingConstants.ONE:
				final Map<Integer, Collection<String>> unboxingResultMap = getBlInventoryScanToolService().doUnboxing(barcodes);
				if (MapUtils.isNotEmpty(unboxingResultMap))
				{
					logUnboxingStatus(barcodes, unboxingResultMap);
				}
				break;

			case BlInventoryScanLoggingConstants.TWO:
				addMessageToNotifyUser(BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE, StringUtils.EMPTY);
				break;

			case BlInventoryScanLoggingConstants.THREE:
				addMessageToNotifyUser(BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE, StringUtils.EMPTY);
				break;

			default:
				addMessageToNotifyUser(BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE, StringUtils.EMPTY);
				break;
		}

	}

	/**
	 * This method will log success and partial success scenario
	 *
	 * @param barcodes
	 *           list
	 * @param unboxingResultMap
	 *           result map
	 */
	private void logUnboxingStatus(final List<String> barcodes, final Map<Integer, Collection<String>> unboxingResultMap)
	{
		if (unboxingResultMap.containsKey(BlInventoryScanLoggingConstants.ZERO))
		{
			doActionOnFailedBarcodeList(barcodes, unboxingResultMap);
		}
		else if (unboxingResultMap.containsKey(BlInventoryScanLoggingConstants.TWO))
		{
			doActionOnErrorSerialList(barcodes, unboxingResultMap);
		}
		else if (unboxingResultMap.containsKey(BlInventoryScanLoggingConstants.THREE))
		{
			doActionOnDcOrDpcErrorSerialList(barcodes, unboxingResultMap);
		}
		else
		{
			addMessageToNotifyUser(BlInventoryScanLoggingConstants.UNBOX_SAN_TOOL_PACKAGE_FAILURE_MSG,
					BlInventoryScanLoggingConstants.UNBOX_SAN_TOOL_PACKAGE_FAILURE, NOTIFICATION_LEVEL_FAILURE,
					barcodes.subList(0, barcodes.size() - 1));
		}
	}

	/**
	 * Do action on failed barcode list.
	 *
	 * @param barcodes
	 *           the barcodes
	 * @param unboxingResultMap
	 *           the unboxing result map
	 */
	private void doActionOnFailedBarcodeList(final List<String> barcodes, final Map<Integer, Collection<String>> unboxingResultMap)
	{
		final Collection<String> failedBarcodeList = unboxingResultMap.get(BlInventoryScanLoggingConstants.ZERO);
		if (CollectionUtils.isEmpty(failedBarcodeList))
		{
			addMessageToNotifyUser(BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG,
					BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS, NOTIFICATION_LEVEL_SUCCESS, barcodes.size());
		}
		else
		{
			addMessageToNotifyUser(BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE_MSG,
					BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE, NOTIFICATION_LEVEL_WARNING, failedBarcodeList);
		}
		final Collection<String> dpSerials = unboxingResultMap.containsKey(BlInventoryScanLoggingConstants.ONE)
				? unboxingResultMap.get(BlInventoryScanLoggingConstants.ONE)
				: Collections.emptyList();
		if (CollectionUtils.isNotEmpty(dpSerials))
		{
			addMessageToNotifyUser(BlInventoryScanLoggingConstants.UNBOX_SAN_TOOL_DPC_FAILURE_MSG,
					BlInventoryScanLoggingConstants.UNBOX_SAN_TOOL_DPC_WARNING, NOTIFICATION_LEVEL_WARNING, dpSerials);
		}
	}

	/**
	 * Do action on error serial list.
	 *
	 * @param barcodes
	 *           the barcodes
	 * @param unboxingResultMap
	 *           the unboxing result map
	 */
	private void doActionOnErrorSerialList(final List<String> barcodes, final Map<Integer, Collection<String>> unboxingResultMap)
	{
		final Collection<String> errorSerialList = unboxingResultMap.get(BlInventoryScanLoggingConstants.TWO);
		if (Objects.isNull(errorSerialList))
		{
			addMessageToNotifyUser(BlInventoryScanLoggingConstants.UNBOX_SAN_TOOL_PACKAGE_FAILURE_MSG,
					BlInventoryScanLoggingConstants.UNBOX_SAN_TOOL_PACKAGE_FAILURE, NOTIFICATION_LEVEL_FAILURE,
					barcodes.subList(0, barcodes.size() - 1));
		}
		else
		{
			addMessageToNotifyUser(BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG,
					BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS, NOTIFICATION_LEVEL_SUCCESS, barcodes.size());
		}
	}

	/**
	 * Do action on dirty cart or dirty priority cart scanned error serial list.
	 *
	 * @param barcodes
	 *           the barcodes
	 * @param unboxingResultMap
	 *           the unboxing result map
	 */
	private void doActionOnDcOrDpcErrorSerialList(final List<String> barcodes,
			final Map<Integer, Collection<String>> unboxingResultMap)
	{
		final Collection<String> errorSerialList = unboxingResultMap.get(BlInventoryScanLoggingConstants.THREE);
		if (CollectionUtils.isEmpty(errorSerialList))
		{
			addMessageToNotifyUser(BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG,
					BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS, NOTIFICATION_LEVEL_SUCCESS, barcodes.size());
		}
		else
		{
			if (getBlInventoryScanToolService().getStatusOfLocationDP())
			{
				addMessageToNotifyUser(BlInventoryScanLoggingConstants.UNBOX_SAN_TOOL_DC_FAILURE_MSG,
						BlInventoryScanLoggingConstants.UNBOX_SAN_TOOL_DPC_FAILURE, NOTIFICATION_LEVEL_FAILURE, errorSerialList);
			}
			else
			{
				addMessageToNotifyUser(BlInventoryScanLoggingConstants.UNBOX_SAN_TOOL_DPC_FAILURE_MSG,
						BlInventoryScanLoggingConstants.UNBOX_SAN_TOOL_DC_FAILURE, NOTIFICATION_LEVEL_FAILURE, errorSerialList);
			}
		}
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
	private void addMessageToNotifyUser(final String logMessage, final String errorType,
			final NotificationEvent.Level notificationEventLevel, final Object... referenceObjects)
	{
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, logMessage, referenceObjects);
		this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.UNBOX_NOTIFICATION_HANDLER, errorType,
				notificationEventLevel, referenceObjects);
	}

	public NotificationService getNotificationService()
	{
		return notificationService;
	}

	public void setNotificationService(final NotificationService notificationService)
	{
		this.notificationService = notificationService;
	}

	public BlInventoryScanToolService getBlInventoryScanToolService()
	{
		return blInventoryScanToolService;
	}

	public void setBlInventoryScanToolService(final BlInventoryScanToolService blInventoryScanToolService)
	{
		this.blInventoryScanToolService = blInventoryScanToolService;
	}
}
