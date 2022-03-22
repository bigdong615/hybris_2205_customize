package com.bl.backoffice.wizards.handler;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.backoffice.wizards.util.WebScanToolData;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.inventory.scan.service.BlInventoryScanToolService;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.utils.BlInventoryScanUtility;
import com.bl.logging.BlLogger;
import com.google.common.collect.Maps;
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
	private static final NotificationEvent.Level NOTIFICATION_LEVEL_FAILURE = NotificationEvent.Level.FAILURE;
	private static final NotificationEvent.Level NOTIFICATION_LEVEL_SUCCESS = NotificationEvent.Level.SUCCESS;

	private static final Logger LOG = Logger.getLogger(TechEngScanToolHandler.class);

	private NotificationService notificationService;
	private BlInventoryScanToolService blInventoryScanToolService;
	private Boolean allowSuccessMsgDisplay;

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
		setAllowSuccessMsgDisplay(Boolean.TRUE);
		final WebScanToolData webScanToolData = flowActionHandlerAdapter.getWidgetInstanceManager().getModel()
				.getValue(dataMap.get(BlInventoryScanLoggingConstants.WEB_SCAN_TOOL_DATA_MODEL_KEY), WebScanToolData.class);

		if (Objects.isNull(webScanToolData))
		{
			doNotifyUser(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
					BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE, NOTIFICATION_LEVEL_FAILURE, StringUtils.EMPTY);
		}
		else
		{
			final List<String> barcodes = webScanToolData.getBarcodeInputField();
			if (CollectionUtils.isEmpty(barcodes))
			{
				doNotifyUser(BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE, StringUtils.EMPTY);
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
			if (getBlInventoryScanToolService().checkIfLocationIsBin(barcodes.get(BlInventoryScanLoggingConstants.ZERO), false))
			{
				processScanningIfFirstScanIsBin(barcodes);
			}
			else if (getBlInventoryScanToolService()
					.checkIfLocationIsBin(barcodes.get(barcodes.size() - BlInventoryScanLoggingConstants.ONE), true))
			{
				logTechEngErrorsForBinScan(getBlInventoryScanToolService().doSerialLocationToBinScanningForTechEng(barcodes));
			}
			else if (getBlInventoryScanToolService().checkIfFirstEntryIsLocation(barcodes))
			{
				doNotifyUser(BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.FIRST_SCAN_LOCATION_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE,
						StringUtils.EMPTY);
				setAllowSuccessMsgDisplay(Boolean.FALSE);
			}
			else
			{
				createResponseMsgForScan(getBlInventoryScanToolService().isValidTechEngLocationBarcode(barcodes,
						BlInventoryScanUtility.getTechEngAllowedLocations()), barcodes);
			}
		}
		else
		{
			doNotifyUser(BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
					BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE,
					BlInventoryScanLoggingConstants.SCAN_STRING + barcodes);
			setAllowSuccessMsgDisplay(Boolean.FALSE);
		}
	}


	/**
	 * Process scanning if first scan is bin.
	 *
	 * @param barcodes
	 *           the barcodes
	 */
	private void processScanningIfFirstScanIsBin(final List<String> barcodes)
	{
		if (barcodes.size() > BlInventoryScanLoggingConstants.TWO)
		{
			doNotifyUser(BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG_FOR_BIN,
					BlInventoryScanLoggingConstants.FIRST_SCAN_LOCATION_BIN_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE,
					StringUtils.EMPTY);
			setAllowSuccessMsgDisplay(Boolean.FALSE);
		}
		else
		{
			if (getBlInventoryScanToolService()
					.checkIfGivenBarcodeIsValidLocation(barcodes.get(BlInventoryScanLoggingConstants.ONE)))
			{
				logTechEngErrorsForBinScan(getBlInventoryScanToolService().performBinToCartScanning(barcodes, false));
			}
			else
			{
				doNotifyUser(BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE, StringUtils.EMPTY);
				setAllowSuccessMsgDisplay(Boolean.FALSE);
			}
		}
	}

	/**
	 * Log tech eng errors for bin scan.
	 *
	 * @param barcodes
	 *           the barcodes
	 * @param scanningResultMap
	 *           the scanning result map
	 */
	private void logTechEngErrorsForBinScan(final Map<Integer, Collection<String>> scanningResultMap)
	{
		if (MapUtils.isNotEmpty(scanningResultMap))
		{
			scanningResultMap.forEach((errorCode, barcodeList) -> {
				switch (errorCode)
				{
					case BlInventoryScanLoggingConstants.ZERO: //priority serials error
						final List<String> barcodeWithNameList = new ArrayList<>();
						getBarcodeByProductNameString(barcodeList, barcodeWithNameList);
						doNotifyUser(BlInventoryScanLoggingConstants.WRONG_CLEAN_CART_LOCATION,
								BlInventoryScanLoggingConstants.CLEAN_CART_SCAN_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE,
								barcodeWithNameList);
						setAllowSuccessMsgDisplay(Boolean.FALSE);
						break;
					case BlInventoryScanLoggingConstants.ONE: //non priority serials
						final List<String> barcodeWithNameList1 = new ArrayList<>();
						getBarcodeByProductNameString(barcodeList, barcodeWithNameList1);
						doNotifyUser(BlInventoryScanLoggingConstants.WRONG_CLEAN_PRIORITY_CART_LOCATION,
								BlInventoryScanLoggingConstants.CLEAN_PRIORITY_CART_SCAN_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE,
								barcodeWithNameList1);
						setAllowSuccessMsgDisplay(Boolean.FALSE);
						break;
					case BlInventoryScanLoggingConstants.TWO: //not a Workstation or CC or CPC or Repair Cart location
						doNotifyUser(BlInventoryScanLoggingConstants.LAST_SCAN_CPC_OR_CC_TYPE_ERROR_MSG,
								BlInventoryScanLoggingConstants.LAST_SCAN_CPC_OR_CC_TYPE_ERROR, NOTIFICATION_LEVEL_FAILURE, barcodeList);
						setAllowSuccessMsgDisplay(Boolean.FALSE);
						break;
					case BlInventoryScanLoggingConstants.INT_SIX: // location not found in database
						doNotifyUser(BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE_MSG,
								BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE, barcodeList);
						setAllowSuccessMsgDisplay(Boolean.FALSE);
						break;
					case BlInventoryScanLoggingConstants.INT_SEVEN: // not a bin location
						doNotifyUser(BlInventoryScanLoggingConstants.BIN_TYPE_ERROR_MSG,
								BlInventoryScanLoggingConstants.TECH_ENG_BIN_TYPE_ERROR, NOTIFICATION_LEVEL_FAILURE, barcodeList);
						setAllowSuccessMsgDisplay(Boolean.FALSE);
						break;
					case BlInventoryScanLoggingConstants.INT_TEN: // Serial Not found in system
						doNotifyUser(BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE_MSG,
								BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE, barcodeList);
						setAllowSuccessMsgDisplay(Boolean.FALSE);
						break;
					case BlInventoryScanLoggingConstants.INT_TWELVE: //More than one location found
						doNotifyUser(BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE_MSG,
								BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE,
								StringUtils.EMPTY);
						setAllowSuccessMsgDisplay(Boolean.FALSE);
						break;
					default:
						break;
				}
			});
		}
		if (getAllowSuccessMsgDisplay().booleanValue())
		{
			doNotifyUser(BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG,
					BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS, NOTIFICATION_LEVEL_SUCCESS, StringUtils.EMPTY);
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
				doNotifyUser(BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE,
						barcodes.get(barcodes.size() - BlInventoryScanLoggingConstants.ONE));
				break;
			case BlInventoryScanLoggingConstants.THREE:
				doNotifyUser(BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE, StringUtils.EMPTY);
				break;
			default:
				doNotifyUser(BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE, StringUtils.EMPTY);
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
						BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE, errorBarcode);
				break;
			case BlInventoryScanLoggingConstants.WRONG_ITEM_CLEAN_CART:
				final List<String> barcodeWithNameList = new ArrayList<>();
				getBarcodeByProductNameString(errorBarcode, barcodeWithNameList);
				doNotifyUser(BlInventoryScanLoggingConstants.WRONG_CLEAN_CART_LOCATION,
						BlInventoryScanLoggingConstants.CLEAN_CART_SCAN_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE, barcodeWithNameList);
				break;
			case BlInventoryScanLoggingConstants.WRONG_ITEM_CLEAN_PRIORITY_CART:
				final List<String> barcodeWithNameList1 = new ArrayList<>();
				getBarcodeByProductNameString(errorBarcode, barcodeWithNameList1);
				doNotifyUser(BlInventoryScanLoggingConstants.WRONG_CLEAN_PRIORITY_CART_LOCATION,
						BlInventoryScanLoggingConstants.CLEAN_PRIORITY_CART_SCAN_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE,
						barcodeWithNameList1);
				break;
			case BlInventoryScanLoggingConstants.SUCCESS:
				doNotifyUser(BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG,
						BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS, NOTIFICATION_LEVEL_SUCCESS, StringUtils.EMPTY);
				break;
			default:
				doNotifyUser(BlInventoryScanLoggingConstants.LOG_SOMETHING_WENT_WRONG,
						BlInventoryScanLoggingConstants.SCAN_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE, errorBarcode);
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
			final NotificationEvent.Level notificationEventLevel, final Object... referenceObjects)
	{
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, logMessage, referenceObjects);
		this.getNotificationService().notifyUser(BlInventoryScanLoggingConstants.TECH_ENG_NOTIFICATION_HANDLER, notifyErrorType,
				notificationEventLevel, referenceObjects);
	}

	/**
	 * Gets the barcode by product name string.
	 *
	 * @param serialBarcodeList
	 *           the serial barcode list
	 * @param barcodeWithNameList
	 *           the barcode with name list
	 * @return the barcode by product name string
	 */
	private void getBarcodeByProductNameString(final Collection<String> serialBarcodeList, final List<String> barcodeWithNameList)
	{
		final Collection<BlSerialProductModel> serialProductsByBarcode = getBlInventoryScanToolService()
				.getSerialProductsByBarcode(serialBarcodeList, BlInventoryScanLoggingConstants.ONLINE);
		if (CollectionUtils.isNotEmpty(serialProductsByBarcode))
		{
			final Map<String, String> barcodeByProductNameMap = getBarcodeByProductNameMap(serialProductsByBarcode);
			serialBarcodeList.forEach(barcode -> {
				if (barcodeByProductNameMap.containsKey(barcode))
				{
					barcodeWithNameList.add(barcode.concat(BlInventoryScanLoggingConstants.FOR_PRODUCT_MESSAGE)
							.concat(barcodeByProductNameMap.get(barcode)));
				}
				else
				{
					barcodeWithNameList
							.add(barcode.concat(BlInventoryScanLoggingConstants.FOR_PRODUCT_MESSAGE).concat(StringUtils.EMPTY));
				}
			});
		}
	}

	/**
	 * Gets the barcode by product name map.
	 *
	 * @param serialProductsByBarcode
	 *           the serial products by barcode
	 * @return the barcode by product name map
	 */
	private Map<String, String> getBarcodeByProductNameMap(final Collection<BlSerialProductModel> serialProductsByBarcode)
	{
		if (CollectionUtils.isNotEmpty(serialProductsByBarcode))
		{
			return serialProductsByBarcode.stream().collect(Collectors.toMap(BlSerialProductModel::getBarcode, serial -> {
				if (Objects.nonNull(serial.getBlProduct()) && StringUtils.isNotBlank(serial.getBlProduct().getName()))
				{
					return serial.getBlProduct().getName();
				}
				return StringUtils.EMPTY;
			}));
		}
		return Maps.newHashMap();
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


	/**
	 * @return the allowSuccessMsgDisplay
	 */
	public Boolean getAllowSuccessMsgDisplay()
	{
		return allowSuccessMsgDisplay;
	}


	/**
	 * @param allowSuccessMsgDisplay
	 *           the allowSuccessMsgDisplay to set
	 */
	public void setAllowSuccessMsgDisplay(final Boolean allowSuccessMsgDisplay)
	{
		this.allowSuccessMsgDisplay = allowSuccessMsgDisplay;
	}
}
