package com.bl.backoffice.widget.controller;

import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.util.Config;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import javax.annotation.Resource;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.WrongValueException;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Textbox;

import com.bl.backoffice.wizards.util.WebScanToolData;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.inventory.scan.service.BlInventoryScanToolService;
import com.bl.core.model.BlProductModel;
import com.bl.core.utils.BlInventoryScanUtility;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;


/**
 * This class is responsible to scan the items to working location and then verify the scanned items
 *
 * @author Aditi Sharma
 */

public class BlFrontDeskOrderScanController extends DefaultWidgetController
{
	protected static final String OUT_CONFIRM = "confirmOutput";
	protected static final String COMPLETE = "completed";

	private static final Logger LOG = Logger.getLogger(BlFrontDeskOrderScanController.class);

	@Wire
	Textbox scanningArea;

	private transient WebScanToolData shippingScanToolData;

	@Resource(name = "blInventoryScanToolService")
	private BlInventoryScanToolService blInventoryScanToolService;

	ConsignmentModel selectedConsignment = new ConsignmentModel();

	/**
	 * This method is used to load the default values at the time of opening the popup
	 *
	 * @param inputObject
	 */
	@SocketEvent(socketId = BlInventoryScanLoggingConstants.SOCKET_ID)
	public void initCustomerAddressForm(final ConsignmentModel inputObject)
	{
		selectedConsignment = inputObject;
		this.getWidgetInstanceManager()
				.setTitle(String.valueOf(this.getWidgetInstanceManager().getLabel("blbackoffice.frontdesk.order.scan.heading")));
		shippingScanToolData = new WebScanToolData();
	}

	/**
	 * This method is used to update the barcode values on change event
	 */
	@ViewEvent(componentID = BlInventoryScanLoggingConstants.SCANNING_AREA, eventName = BlInventoryScanLoggingConstants.ON_CHANGE_EVENT)
	public void changeScan()
	{
		shippingScanToolData
				.setBarcodeInputField(Arrays.asList(scanningArea.getValue().split(BlInventoryScanLoggingConstants.NEW_LINE)));
	}

	/**
	 * This method is used to close the Front Desk Scanning Popup
	 */
	@ViewEvent(componentID = BlInventoryScanLoggingConstants.CANCEL_EVENT, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void cancel()
	{
		this.sendOutput(OUT_CONFIRM, COMPLETE);
	}

	/**
	 * This method is used to Scan serial to working location
	 */
	@ViewEvent(componentID = BlInventoryScanLoggingConstants.SCAN_TO_WORKSTATION, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void scanSerialToWorkStation()
	{
		if (CollectionUtils.isEmpty(shippingScanToolData.getBarcodeInputField()))
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
					BlInventoryScanLoggingConstants.FRONT_DESK_NO_ITEM_SCAN_KEY);
		}
		else
		{
			validateWorkingDeskLocation();
		}
	}


	/**
	 * This method is used to verify front desk Scan
	 */
	@ViewEvent(componentID = BlInventoryScanLoggingConstants.VERIFY_SCAN_EVENT, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void verifyScan()
	{
		if (CollectionUtils.isEmpty(shippingScanToolData.getBarcodeInputField()))
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
					BlInventoryScanLoggingConstants.SHIPPING_NO_ITEM_SCAN_KEY);
		}
		else
		{
			validateShippingScan();
		}
	}

	/**
	 * This method is used to Scan bin to working location
	 */
	@ViewEvent(componentID = BlInventoryScanLoggingConstants.SCAN_TO_BIN, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void scanToBinLocation()
	{
		if (CollectionUtils.isEmpty(shippingScanToolData.getBarcodeInputField()))
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
					BlInventoryScanLoggingConstants.SHIPPING_NO_ITEM_SCAN_KEY);
		}
		else
		{
			validateFDBinLocation();
		}
	}

	/**
	 * method will validate bin location
	 */
	private void validateFDBinLocation()
	{
		if (OrderStatus.CANCELLED.equals(selectedConsignment.getOrder().getStatus()))
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.FRONT_DESK_CANCEL_ORDER_FAILURE_MSG,
					BlInventoryScanLoggingConstants.FRONT_DESK_CANCEL_ORDER_FAILURE);
		}
		else if (ConsignmentStatus.SHIPPING_MANUAL_REVIEW.equals(selectedConsignment.getStatus()))
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.FRONT_DESK_MANUAL_REVIEW_FAILURE_MSG,
					BlInventoryScanLoggingConstants.FRONT_DESK_MANUAL_REVIEW_FAILURE);
		}
		else
		{
			final List<String> barcodes = shippingScanToolData.getBarcodeInputField();
			createResponseForFDScan(barcodes);
		}
	}

	/**
	 * This method is used to create shipping scan response
	 *
	 * @param barcodes
	 */
	private void createResponseForFDScan(final List<String> barcodes)
	{
		final int barcodeSize = barcodes.size();

		if (barcodeSize == BlInventoryScanLoggingConstants.TWO
				&& barcodeSize <= Config.getInt("blbackoffice.max.product.scan", BlInventoryScanLoggingConstants.HUNDERED))
		{
			createResponseMsgForScan(
					getBlInventoryScanToolService().checkLocationWithTypeForFD(barcodes, BlInventoryScanUtility.getFrontDeskShelfInitial(),
							BlInventoryScanUtility.getFrontDeskAllowedWorkStationLocations()),
					barcodes);
		}
		else
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.TWO_BARCODE_SCAN_ERROR_MSG,
					BlInventoryScanLoggingConstants.SHIPPING_TWO_BARCODE_SCAN_ERROR_KEY);
		}
	}

	/**
	 * This method is used to create response message for shipping scan
	 *
	 * @param result
	 * @param barcodes
	 */
	private void createResponseMsgForScan(final int result, final List<String> barcodes)
	{
		switch (result)
		{
			case BlInventoryScanLoggingConstants.ONE:
				final Map<Integer, List<String>> failedBinBarcodeMap = getBlInventoryScanToolService()
						.getFailedBinBarcodeList(barcodes, BlInventoryScanUtility.getFrontDeskAllowedWorkStationLocations());
				if (MapUtils.isNotEmpty(failedBinBarcodeMap))
				{
					createResponseForScan(failedBinBarcodeMap);
				}
				break;

			case BlInventoryScanLoggingConstants.TWO:
				notifyErrorMessage(BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.FRONT_DESK_LOCATION_ERROR_KEY);
				break;

			case BlInventoryScanLoggingConstants.THREE:
				notifyErrorMessage(BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.FRONT_DESK_LAST_INVALID_LOCATION_ERROR);
				break;

			default:
				notifyErrorMessage(BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.FRONT_DESK_MANY_LOCATION_ERROR);
		}

	}

	/**
	 * This method is used to create success response for shipping scan
	 *
	 * @param barcodes
	 * @param failedBinBarcodeMap
	 */
	private void createResponseForScan(final Map<Integer, List<String>> failedBinBarcodeMap)
	{
		if (failedBinBarcodeMap.containsKey(BlInventoryScanLoggingConstants.ZERO))
		{
			BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG);
			Messagebox.show(BlInventoryScanLoggingConstants.SCANNING_SUCCESS_MSG);
			this.scanningArea.setValue(BlInventoryScanLoggingConstants.EMPTY_STRING);
		}
		if (failedBinBarcodeMap.containsKey(BlInventoryScanLoggingConstants.ONE))
		{
			notifyInvalidScan(BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE_MSG,
					BlInventoryScanLoggingConstants.SHIPPING_INVALID_LOCATION_ERROR, failedBinBarcodeMap);
		}
	}


	/**
	 * method will used to validate front desk scan
	 */
	private void validateShippingScan()
	{
		if (OrderStatus.CANCELLED.equals(selectedConsignment.getOrder().getStatus()))
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.FRONT_DESK_CANCEL_ORDER_FAILURE_MSG,
					BlInventoryScanLoggingConstants.FRONT_DESK_CANCEL_ORDER_FAILURE);
		}
		else if (ConsignmentStatus.SHIPPING_MANUAL_REVIEW.equals(selectedConsignment.getStatus()))
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.FRONT_DESK_MANUAL_REVIEW_FAILURE_MSG,
					BlInventoryScanLoggingConstants.FRONT_DESK_MANUAL_REVIEW_FAILURE);
		}
		else
		{
			verifyShippingScan();
		}
	}

	/**
	 * method will verify front desk scan
	 */
	private void verifyShippingScan()
	{
		final List<String> barcodes = shippingScanToolData.getBarcodeInputField();
		final int barcodeSize = barcodes.size();
		if (barcodeSize >= BlInventoryScanLoggingConstants.ONE)
		{
			final Map<String, List<BlProductModel>> scannedBarcodeMap = getBlInventoryScanToolService().verifyShippingScan(barcodes,
					selectedConsignment);
			createResponseMsgForShippingScan(scannedBarcodeMap);
		}
		else
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
					BlInventoryScanLoggingConstants.FRONT_DESK_INVALID_SCAN_ERROR);
		}
	}

	/**
	 * method will validate working station location
	 */
	private void validateWorkingDeskLocation()
	{
		if (OrderStatus.CANCELLED.equals(selectedConsignment.getOrder().getStatus()))
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.SHIPPING_CANCEL_ORDER_FAILURE_MSG,
					BlInventoryScanLoggingConstants.FRONT_DESK_CANCEL_ORDER_FAILURE);
		}
		else if (ConsignmentStatus.SHIPPING_MANUAL_REVIEW.equals(selectedConsignment.getStatus()))
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.SHIPPING_MANUAL_REVIEW_FAILURE_MSG,
					BlInventoryScanLoggingConstants.FRONT_DESK_MANUAL_REVIEW_FAILURE);
		}
		else
		{
			final List<String> barcodes = shippingScanToolData.getBarcodeInputField();
			createResponseForFDOrderScan(barcodes);
		}
	}

	/**
	 * method will be used to create response message for FD Scan
	 * @param scannedBarcodeList
	 */
	private void createResponseMsgForShippingScan(final Map<String, List<BlProductModel>> scannedBarcodeMap)
	{
		if (MapUtils.isNotEmpty(scannedBarcodeMap))
		{
			if (scannedBarcodeMap.containsKey(BlInventoryScanLoggingConstants.SUCCESS_SCAN))
			{
				BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG);
				Messagebox.show(BlInventoryScanLoggingConstants.SCANNING_SUCCESS_MSG);
				this.scanningArea.setValue(BlInventoryScanLoggingConstants.EMPTY_STRING);
			}
			else
			{
				if (scannedBarcodeMap.containsKey(BlInventoryScanLoggingConstants.MISSING_IN_CONSIGNMENT))
				{
					notifyInvalidScan(BlInventoryScanLoggingConstants.SERIAL_MISSING_ON_CONSIGNMENT_MSG,
							BlInventoryScanLoggingConstants.FRONT_DESK_SERIAL_MISSING_ON_CONSIGNMENT_KEY,
							scannedBarcodeMap.get(BlInventoryScanLoggingConstants.MISSING_IN_CONSIGNMENT));
				}

				if (scannedBarcodeMap.containsKey(BlInventoryScanLoggingConstants.MISSING_IN_SCAN))
				{
					notifyInvalidScan(BlInventoryScanLoggingConstants.SERIAL_MISSING_ON_SCAN_MSG,
							BlInventoryScanLoggingConstants.FRONT_DESK_SERIAL_MISSING_ON_SCAN_KEY,
							scannedBarcodeMap.get(BlInventoryScanLoggingConstants.MISSING_IN_SCAN));
				}
				if (scannedBarcodeMap.containsKey(BlInventoryScanLoggingConstants.MISSING_SCAN_BARCODE))
				{
					notifyInvalidScan(BlInventoryScanLoggingConstants.SERIAL_MISSING_ON_CONSIGNMENT_MSG,
							BlInventoryScanLoggingConstants.FRONT_DESK_SERIAL_MISSING_ON_CONSIGNMENT_KEY,
							scannedBarcodeMap.get(BlInventoryScanLoggingConstants.MISSING_IN_CONSIGNMENT));
				}
			}
		}
	}

	/**
	 * This method is used to create front desk scan response
	 *
	 * @param barcodes
	 */
	private void createResponseForFDOrderScan(final List<String> barcodes)
	{
		final int barcodeSize = barcodes.size();

		if (barcodeSize >= BlInventoryScanLoggingConstants.TWO
				&& barcodeSize <= Config.getInt("blbackoffice.max.product.scan", BlInventoryScanLoggingConstants.HUNDERED))
		{
			createResponseMsgForFDScan(getBlInventoryScanToolService().checkValidLocationInBarcodeList(barcodes,
					Lists.newArrayList(BlInventoryScanLoggingConstants.ALLOW_SCAN)), barcodes);
		}
		else
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
					BlInventoryScanLoggingConstants.FRONT_DESK_TWO_BARCODE_SCAN_ERROR_KEY);
		}
	}

	/**
	 * This method is used to create response message for front desk scan
	 *
	 * @param result
	 * @param barcodes
	 */
	private void createResponseMsgForFDScan(final int result, final List<String> barcodes)
	{
		if (result == BlInventoryScanLoggingConstants.ONE)
		{
			final List<String> failedBinBarcodeList = getBlInventoryScanToolService().getFailedBarcodeList(barcodes);
			if (CollectionUtils.isNotEmpty(failedBinBarcodeList))
			{
				notifyInvalidScan(BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.FRONT_DESK_INVALID_LOCATION_ERROR, failedBinBarcodeList);
			}
			else
			{
				BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG);
				Messagebox.show(BlInventoryScanLoggingConstants.SCANNING_SUCCESS_MSG);
				this.scanningArea.setValue(BlInventoryScanLoggingConstants.EMPTY_STRING);
			}
		}
		else
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE_MSG,
					BlInventoryScanLoggingConstants.FRONT_DESK_LAST_INVALID_LOCATION_ERROR);
		}
	}

	/**
	 * This method is used to notify user for invalid scan
	 *
	 * @param logMsg
	 *           as log message
	 * @param exceptionLabel
	 *           as exception label
	 * @param logParam
	 *           as log param
	 * @throws WrongValueException
	 *            as wrong value exception
	 */
	private void notifyInvalidScan(final String logMsg, final String exceptionLabel, final Object... logParam)
	{
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, logMsg, logParam);
		throw new WrongValueException(this.scanningArea, this.getLabel(exceptionLabel));
	}

	/**
	 * method will use to notify error message for failure
	 *
	 * @param logMsg
	 *           as log message
	 * @param exceptionLabel
	 *           as exception label
	 */
	private void notifyErrorMessage(final String logMsg, final String exceptionLabel)
	{
		BlLogger.logMessage(LOG, Level.DEBUG, logMsg);
		throw new WrongValueException(this.scanningArea, this.getLabel(exceptionLabel));
	}


	/**
	 * @return the blInventoryScanToolService
	 */
	public BlInventoryScanToolService getBlInventoryScanToolService()
	{
		return blInventoryScanToolService;
	}

	/**
	 * @param blInventoryScanToolService
	 *           the blInventoryScanToolService to set
	 */
	public void setBlInventoryScanToolService(final BlInventoryScanToolService blInventoryScanToolService)
	{
		this.blInventoryScanToolService = blInventoryScanToolService;
	}


}

