package com.bl.backoffice.wizards.handler;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.assertj.core.util.Lists;

import com.bl.backoffice.wizards.util.WebScanToolData;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.inventory.scan.service.BlInventoryScanToolService;
import com.bl.core.model.BlSerialProductModel;
import com.bl.logging.BlLogger;
import com.google.common.collect.Maps;
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
	private Boolean allowSuccessMsgDisplay;

	/**
	 * This OOB method which will perform actions on input barcodes form backoffice wizard
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
		setAllowSuccessMsgDisplay(Boolean.TRUE);
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
	 * method will check the input size and notify user accordingly with conditions
	 *
	 * @param barcodes
	 *           of list
	 */
	private void createResponseForScanResult(final List<String> barcodes)
	{
		if (barcodes.size() >= BlInventoryScanLoggingConstants.TWO)
		{
			if (getBlInventoryScanToolService().checkIfLocationIsBin(barcodes.get(BlInventoryScanLoggingConstants.ZERO), false))
			{
				performBinLocationUpdate(barcodes);
			}
			else if (getBlInventoryScanToolService()
					.checkIfLocationIsBin(barcodes.get(barcodes.size() - BlInventoryScanLoggingConstants.ONE), true))
			{
				logUnboxingBinErrors(barcodes, getBlInventoryScanToolService().doSerialLocationToBinScanningForUnboxing(barcodes));
			}
			else if (getBlInventoryScanToolService().checkIfFirstEntryIsLocation(barcodes))
			{
				addMessageToNotifyUser(BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.FIRST_SCAN_LOCATION_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE,
						StringUtils.EMPTY);
				setAllowSuccessMsgDisplay(Boolean.FALSE);
			}
			else
			{
				createResponseMegForScan(getBlInventoryScanToolService().checkValidLocationInBarcodeListOfDPC(barcodes), barcodes);
			}
		}
		else
		{
			addMessageToNotifyUser(BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
					BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE,
					BlInventoryScanLoggingConstants.SCAN_STRING + barcodes);
			setAllowSuccessMsgDisplay(Boolean.FALSE);
		}
	}

	/**
	 * Perform bin location update.
	 *
	 * @param barcodes
	 *           the barcodes
	 */
	private void performBinLocationUpdate(final List<String> barcodes)
	{
		if (barcodes.size() > BlInventoryScanLoggingConstants.TWO)
		{
			addMessageToNotifyUser(BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG_FOR_BIN,
					BlInventoryScanLoggingConstants.FIRST_SCAN_LOCATION_BIN_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE,
					StringUtils.EMPTY);
			setAllowSuccessMsgDisplay(Boolean.FALSE);
		}
		else
		{
			if (getBlInventoryScanToolService()
					.checkIfGivenBarcodeIsValidLocation(barcodes.get(BlInventoryScanLoggingConstants.ONE)))
			{
				logUnboxingBinErrors(barcodes, getBlInventoryScanToolService().performBinToCartScanning(barcodes, true));
			}
			else
			{
				addMessageToNotifyUser(BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE, StringUtils.EMPTY);
				setAllowSuccessMsgDisplay(Boolean.FALSE);
			}
		}
	}

	/**
	 * Log unboxing bin errors.
	 *
	 * @param barcodes
	 *           the barcodes
	 * @param unboxingResultMap
	 *           the unboxing result map
	 */
	private void logUnboxingBinErrors(final List<String> barcodes, final Map<Integer, Collection<String>> unboxingResultMap)
	{
		final List<String> successBarcodes = Lists.newArrayList(barcodes);
		if (MapUtils.isNotEmpty(unboxingResultMap))
		{
			unboxingResultMap.forEach((errorCode, barcodeList) -> {
				switch (errorCode)
				{
					case BlInventoryScanLoggingConstants.FOUR: // handle error for dirty list
						processErrorCodeFour(barcodes, unboxingResultMap, false);
						successBarcodes.removeAll(barcodeList);
						break;
					case BlInventoryScanLoggingConstants.FIVE: // handle error for dirty priority list
						processErrorCodeFive(barcodes, unboxingResultMap, false);
						successBarcodes.removeAll(barcodeList);
						break;
					case BlInventoryScanLoggingConstants.INT_SIX: // location not found in database
						addMessageToNotifyUser(BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE_MSG,
								BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE, barcodeList);
						successBarcodes.removeAll(barcodes);
						setAllowSuccessMsgDisplay(Boolean.FALSE);
						break;
					case BlInventoryScanLoggingConstants.INT_SEVEN: // not a bin location
						addMessageToNotifyUser(BlInventoryScanLoggingConstants.BIN_TYPE_ERROR_MSG,
								BlInventoryScanLoggingConstants.BIN_TYPE_ERROR, NOTIFICATION_LEVEL_FAILURE, barcodeList);
						successBarcodes.removeAll(barcodes);
						setAllowSuccessMsgDisplay(Boolean.FALSE);
						break;
					case BlInventoryScanLoggingConstants.INT_EIGHT: //not a Dirty Cart or Dirty Priority Cart location
						addMessageToNotifyUser(BlInventoryScanLoggingConstants.LAST_SCAN_DPC_OR_DC_TYPE_ERROR_MSG,
								BlInventoryScanLoggingConstants.LAST_SCAN_DPC_OR_DC_TYPE_ERROR, NOTIFICATION_LEVEL_FAILURE,
								StringUtils.EMPTY);
						successBarcodes.removeAll(barcodes);
						setAllowSuccessMsgDisplay(Boolean.FALSE);
						break;
					case BlInventoryScanLoggingConstants.INT_NINE: // missing packages for serials
						addMessageToNotifyUser(BlInventoryScanLoggingConstants.MISSING_BARCODE_PACKAGE_ERROR_MSG,
								BlInventoryScanLoggingConstants.MISSING_BARCODE_PACKAGE_ERROR, NOTIFICATION_LEVEL_FAILURE, barcodeList);
						successBarcodes.removeAll(barcodeList);
						setAllowSuccessMsgDisplay(Boolean.FALSE);
						break;
					case BlInventoryScanLoggingConstants.INT_TEN: // Serial Not found in system
						addMessageToNotifyUser(BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE_MSG,
								BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE, barcodeList);
						successBarcodes.removeAll(barcodeList);
						setAllowSuccessMsgDisplay(Boolean.FALSE);
						break;
					case BlInventoryScanLoggingConstants.INT_ELEVEN: //dirty priority barcodes
						addMessageToNotifyUser(BlInventoryScanLoggingConstants.UNBOX_SAN_TOOL_DPC_FAILURE_MSG,
								BlInventoryScanLoggingConstants.UNBOX_SAN_TOOL_DPC_WARNING, NOTIFICATION_LEVEL_WARNING,
								getBarcodeByProductNameString(barcodeList));
						break;
					case BlInventoryScanLoggingConstants.INT_TWELVE: //More than one location found
						addMessageToNotifyUser(BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE_MSG,
								BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE,
								StringUtils.EMPTY);
						successBarcodes.removeAll(barcodes);
						setAllowSuccessMsgDisplay(Boolean.FALSE);
						break;
					case BlInventoryScanLoggingConstants.INT_THIRTEEN: //dirty barcodes
						addMessageToNotifyUser(BlInventoryScanLoggingConstants.UNBOX_SAN_TOOL_DPC_FAILURE_MSG,
								BlInventoryScanLoggingConstants.UNBOX_SAN_TOOL_DC_FAILURE, NOTIFICATION_LEVEL_WARNING,
								getBarcodeByProductNameString(barcodeList));
						break;
					default:
						break;
				}
			});
		}
		if (getAllowSuccessMsgDisplay().booleanValue()
				|| (CollectionUtils.isNotEmpty(successBarcodes) && successBarcodes.size() > 1))
		{
			addMessageToNotifyUser(BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG,
					BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS, NOTIFICATION_LEVEL_SUCCESS,
					successBarcodes.subList(0, successBarcodes.size() - 1));
		}
	}

	/**
	 * Check for DPC or DC errors.
	 *
	 * @param barcodes
	 *           the barcodes
	 * @param unboxingResultMap
	 *           the unboxing result map
	 * @param showSuccessMsg
	 *           the show success msg
	 */
	private void checkForDPCOrDCErrors(final List<String> barcodes, final Map<Integer, Collection<String>> unboxingResultMap,
			final boolean showSuccessMsg)
	{
		if (unboxingResultMap.containsKey(BlInventoryScanLoggingConstants.FOUR)) // handle error for dirty list
		{
			processErrorCodeFour(barcodes, unboxingResultMap, showSuccessMsg);
		}
		if (unboxingResultMap.containsKey(BlInventoryScanLoggingConstants.FIVE)) // handle error for dirty priority list
		{
			processErrorCodeFive(barcodes, unboxingResultMap, showSuccessMsg);
		}
	}

	/**
	 * Process error message for dirty priority serial list .
	 *
	 * @param barcodes
	 *           the barcodes
	 * @param unboxingResultMap
	 *           the unboxing result map
	 * @param showSuccessMsg
	 *           the show success msg
	 */
	private void processErrorCodeFive(final List<String> barcodes, final Map<Integer, Collection<String>> unboxingResultMap,
			final boolean showSuccessMsg)
	{
		final Collection<String> dirtyPriorityErrorSerialList = unboxingResultMap.get(BlInventoryScanLoggingConstants.FIVE);
		if (CollectionUtils.isNotEmpty(dirtyPriorityErrorSerialList) && !getBlInventoryScanToolService().getStatusOfLocationDP())
		{
			doScanToDPCErrorMessage(dirtyPriorityErrorSerialList, barcodes, showSuccessMsg);
		}
	}

	/**
	 * Process error message for dirty serial list .
	 *
	 * @param barcodes
	 *           the barcodes
	 * @param unboxingResultMap
	 *           the unboxing result map
	 * @param showSuccessMsg
	 *           the show success msg
	 */
	private void processErrorCodeFour(final List<String> barcodes, final Map<Integer, Collection<String>> unboxingResultMap,
			final boolean showSuccessMsg)
	{
		final Collection<String> dirtyErrorSerialList = unboxingResultMap.get(BlInventoryScanLoggingConstants.FOUR);
		if (CollectionUtils.isNotEmpty(dirtyErrorSerialList))
		{
			doScanToAnotherLocationMessage(dirtyErrorSerialList, barcodes, showSuccessMsg);
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
				evaluateErrors(barcodes);
				break;

			case BlInventoryScanLoggingConstants.TWO:
				addMessageToNotifyUser(BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE,
						barcodes.get(barcodes.size() - BlInventoryScanLoggingConstants.ONE));
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
	 * Evaluate errors if any found while performing Unboxing of Serials.
	 *
	 * @param barcodes
	 *           the barcodes
	 */
	private void evaluateErrors(final List<String> barcodes)
	{
		final Map<Integer, Collection<String>> unboxingResultMap = getBlInventoryScanToolService().doUnboxing(barcodes);
		if (MapUtils.isEmpty(unboxingResultMap))
		{
			addMessageToNotifyUser(BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG,
					BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS, NOTIFICATION_LEVEL_SUCCESS,
					barcodes.subList(BlInventoryScanLoggingConstants.INT_ZERO, barcodes.size() - BlInventoryScanLoggingConstants.ONE));
		}
		else
		{
			logUnboxingStatus(barcodes, unboxingResultMap);
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
		final List<String> successBarcodes = new ArrayList<>(barcodes);
		unboxingResultMap.forEach((errorCode,barcodeList)->{
			switch (errorCode)
			{
				case BlInventoryScanLoggingConstants.ZERO: // missing barcode in scan
					doActionOnFailedBarcodeList(barcodeList);
					successBarcodes.removeAll(barcodeList);
					break;
					
				case BlInventoryScanLoggingConstants.ONE: // dirty priority serial
					doAddMessageForDirtyPriortySerial(barcodeList);
					successBarcodes.removeAll(barcodeList);
					break;
				
				case BlInventoryScanLoggingConstants.TWO:
					doActionOnErrorSerialList(barcodes, unboxingResultMap);
					successBarcodes.removeAll(barcodeList);
					break;

				case BlInventoryScanLoggingConstants.THREE:
					doActionOnDcOrDpcErrorSerialList(barcodes, unboxingResultMap);
					successBarcodes.removeAll(barcodeList);
					break;
				case BlInventoryScanLoggingConstants.INT_NINE:
					addMessageToNotifyUser(BlInventoryScanLoggingConstants.MISSING_BARCODE_PACKAGE_ERROR_MSG,
							BlInventoryScanLoggingConstants.MISSING_BARCODE_PACKAGE_ERROR, NOTIFICATION_LEVEL_FAILURE, barcodeList);
					successBarcodes.removeAll(barcodeList);
					break;
					
				case BlInventoryScanLoggingConstants.INT_TEN:
					addMessageToNotifyUser(BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE_MSG,
							BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE, barcodeList);
					successBarcodes.removeAll(barcodeList);			
					break;
				default:
					break;
			}
		});
		if(CollectionUtils.isNotEmpty(successBarcodes) && successBarcodes.size() > BlInventoryScanLoggingConstants.ONE)
		{
			addMessageToNotifyUser(BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG,
					BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS, NOTIFICATION_LEVEL_SUCCESS, successBarcodes.subList(BlInventoryScanLoggingConstants.INT_ZERO, successBarcodes.size()-BlInventoryScanLoggingConstants.ONE));
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
	private void doActionOnFailedBarcodeList(final Collection<String> barcodes)
	{
	if (CollectionUtils.isNotEmpty(barcodes))
		{
			addMessageToNotifyUser(BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE_MSG,
					BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE, NOTIFICATION_LEVEL_FAILURE, barcodes);
		}
	}

	/**
	 * Do add message for dirty priorty serial.
	 *
	 * @param dpSerials
	 *           the dp serials
	 */
	private void doAddMessageForDirtyPriortySerial(final Collection<String> dpSerials)
	{
		if(CollectionUtils.isNotEmpty(dpSerials))
		{
		addMessageToNotifyUser(BlInventoryScanLoggingConstants.UNBOX_SAN_TOOL_DPC_FAILURE_MSG,
				BlInventoryScanLoggingConstants.UNBOX_SAN_TOOL_DPC_WARNING, NOTIFICATION_LEVEL_WARNING,
				getBarcodeByProductNameString(dpSerials));
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
					barcodes.subList(BlInventoryScanLoggingConstants.ZERO, barcodes.size() - BlInventoryScanLoggingConstants.ONE));
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
		if (CollectionUtils.isNotEmpty(errorSerialList))
		{
			checkForDPCOrDCErrors(barcodes, unboxingResultMap, true);
		}
	}

	/**
	 * Do scan to another location message.
	 *
	 * @param dirtyErrorSerialList
	 *           the dirty error serial list
	 */
	private void doScanToAnotherLocationMessage(final Collection<String> dirtyErrorSerialList, final List<String> barcodes,
			final boolean showSuccessMsg)
	{
		final List<String> barcodeWithNameList = getBarcodeByProductNameString(dirtyErrorSerialList);
		if (getBlInventoryScanToolService().getStatusOfLocationDP())
		{
			addMessageToNotifyUser(BlInventoryScanLoggingConstants.UNBOX_SAN_TOOL_DC_FAILURE_MSG,
					BlInventoryScanLoggingConstants.UNBOX_SAN_TOOL_DC_FROM_DPC_FAILURE, NOTIFICATION_LEVEL_FAILURE,
					barcodeWithNameList);
			setAllowSuccessMsgDisplay(Boolean.FALSE);
		}
		else if (!getBlInventoryScanToolService().getStatusOfLocationDC())
		{
			if (showSuccessMsg)
			{
				addMessageToNotifyUser(BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG,
						BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS, NOTIFICATION_LEVEL_SUCCESS,
						barcodes.subList(BlInventoryScanLoggingConstants.ZERO, barcodes.size() - BlInventoryScanLoggingConstants.ONE));
			}
			addMessageToNotifyUser(BlInventoryScanLoggingConstants.UNBOX_SAN_TOOL_DC_FAILURE_MSG,
					BlInventoryScanLoggingConstants.UNBOX_SAN_TOOL_DC_FAILURE, NOTIFICATION_LEVEL_WARNING, barcodeWithNameList);
		}
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
	private List<String> getBarcodeByProductNameString(final Collection<String> serialBarcodeList)
	{
		final List<String> barcodeWithNameList = Lists.newArrayList();
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
		return barcodeWithNameList;
	}

	/**
	 * Do scan to DPC error message.
	 *
	 * @param dirtyPriorityErrorSerialList
	 *           the dirty priority error serial list
	 */
	private void doScanToDPCErrorMessage(final Collection<String> dirtyPriorityErrorSerialList, final List<String> barcodes,
			final boolean showSuccessMsg)
	{
		if (showSuccessMsg)
		{
			addMessageToNotifyUser(BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG,
					BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS, NOTIFICATION_LEVEL_SUCCESS, barcodes.size());
		}
		addMessageToNotifyUser(BlInventoryScanLoggingConstants.UNBOX_SAN_TOOL_DPC_FAILURE_MSG,
				BlInventoryScanLoggingConstants.UNBOX_SAN_TOOL_DPC_FAILURE, NOTIFICATION_LEVEL_WARNING,
				getBarcodeByProductNameString(dirtyPriorityErrorSerialList));
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
