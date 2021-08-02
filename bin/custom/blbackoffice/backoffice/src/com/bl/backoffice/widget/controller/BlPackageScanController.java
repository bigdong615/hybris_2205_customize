/**
 *
 */
package com.bl.backoffice.widget.controller;

import de.hybris.platform.ordersplitting.model.ConsignmentModel;

import java.util.Arrays;
import java.util.List;

import javax.annotation.Resource;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Value;
import org.zkoss.zk.ui.WrongValueException;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Textbox;

import com.bl.backoffice.wizards.util.WebScanToolData;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.inventory.scan.service.BlInventoryScanToolService;
import com.bl.core.utils.BlInventoryScanUtility;
import com.bl.logging.BlLogger;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;


public class BlPackageScanController extends DefaultWidgetController
{
	/**
	 * @author Aditi Sharma
	 */

	protected static final String OUT_CONFIRM = "confirmOutput";
	protected static final String COMPLETE = "completed";

	private static final Logger LOG = Logger.getLogger(BlPackageScanController.class);

	@Wire
	Textbox scanningArea;

	WebScanToolData shippingScanToolData;

	@Value("${blbackoffice.max.product.scan}")
	private int maxProductScan;

	@Resource(name = "blInventoryScanToolService")
	private BlInventoryScanToolService blInventoryScanToolService;

	ConsignmentModel selectedConsignment = new ConsignmentModel();

	@SocketEvent(socketId = BlInventoryScanLoggingConstants.SOCKET_ID)
	public void initCustomerAddressForm(final ConsignmentModel inputObject)
	{
		selectedConsignment = inputObject;
		this.getWidgetInstanceManager()
				.setTitle(String.valueOf(this.getWidgetInstanceManager().getLabel("blbackoffice.shipping.scan.heading")));
		shippingScanToolData = new WebScanToolData();
	}

	@ViewEvent(componentID = BlInventoryScanLoggingConstants.SCANNING_AREA, eventName = BlInventoryScanLoggingConstants.ON_CHANGE_EVENT)
	public void changeScan()
	{
		shippingScanToolData
				.setBarcodeInputField(Arrays.asList(scanningArea.getValue().split(BlInventoryScanLoggingConstants.NEW_LINE)));
	}

	@ViewEvent(componentID = BlInventoryScanLoggingConstants.CANCEL_EVENT, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void cancel()
	{
		this.sendOutput(OUT_CONFIRM, COMPLETE);
	}

	/**
	 * This method is used to scan bin to working location.
	 */
	@ViewEvent(componentID = BlInventoryScanLoggingConstants.SCAN_TO_BIN, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void scanToBinLocation()
	{
		if (CollectionUtils.isEmpty(shippingScanToolData.getBarcodeInputField()))
		{
			notifyInvalidScan(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
					BlInventoryScanLoggingConstants.NO_ITEM_SCAN_KEY, StringUtils.EMPTY);
		}
		else
		{
			final List<String> barcodes = shippingScanToolData.getBarcodeInputField();
			createResponseForPackageScan(barcodes);
		}
	}


	/**
	 * This method is used to verify scanned item and update there location to tracking number
	 */
	@ViewEvent(componentID = BlInventoryScanLoggingConstants.SCAN_TO_TRACKING_ID, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void scanToTrackingLabel()
	{
		if (CollectionUtils.isEmpty(shippingScanToolData.getBarcodeInputField()))
		{
			notifyInvalidScan(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
					BlInventoryScanLoggingConstants.NO_ITEM_SCAN_KEY, StringUtils.EMPTY);
		}
		else
		{
			final List<String> barcodes = shippingScanToolData.getBarcodeInputField();
			final int barcodeSize = barcodes.size();
			if (barcodeSize >= BlInventoryScanLoggingConstants.TWO && barcodeSize <= maxProductScan)
			{
				final String lastScannedItem = (barcodes.get(barcodes.size() - BlInventoryScanLoggingConstants.ONE));
				createReponseMsgForPackageScan(getBlInventoryScanToolService().checkValidTrackingId(lastScannedItem), barcodes);
			}
			else
			{
				notifyInvalidScan(BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.INVALID_SCAN_ERROR, StringUtils.EMPTY);
			}
		}

	}

	/**
	 * This method is used to scan tracking label to UPS Outbound cart location
	 */
	@ViewEvent(componentID = BlInventoryScanLoggingConstants.SCAN_TO_UPS_OUTBOUND, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void scanToOutBoundCart()
	{
		if (CollectionUtils.isEmpty(shippingScanToolData.getBarcodeInputField()))
		{
			notifyInvalidScan(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
					BlInventoryScanLoggingConstants.NO_ITEM_SCAN_KEY, StringUtils.EMPTY);
		}
		else
		{
			final List<String> barcodes = shippingScanToolData.getBarcodeInputField();
			if (CollectionUtils.isEmpty(barcodes))
			{
				notifyInvalidScan(BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.INVALID_SCAN_ERROR, StringUtils.EMPTY);
			}
			else
			{
				createResponseMegForScan(getBlInventoryScanToolService().checkLocationWithType(barcodes,
						BlInventoryScanUtility.getShippingWorkstationInitial(), BlInventoryScanUtility.getShippingAllowedLocations()),
						barcodes);
			}
		}
	}

	/**
	 * This method is used to create response for package scan
	 *
	 * @param barcodes
	 */
	private void createResponseForPackageScan(final List<String> barcodes)
	{
		final int barcodeSize = barcodes.size();
		if (barcodeSize == BlInventoryScanLoggingConstants.TWO)
		{
			createResponseMsgForPackageScan(getBlInventoryScanToolService().checkLocationWithType(barcodes,
					BlInventoryScanUtility.getShippingWorkstationInitial(), BlInventoryScanUtility.getShippingAllowedLocations()),
					barcodes);
		}
	}

	/**
	 * This method is used to create response message for scanning bin to working location
	 *
	 * @param result
	 * @param barcodes
	 */
	private void createResponseMsgForPackageScan(final int result, final List<String> barcodes)
	{
		switch (result)
		{
			case BlInventoryScanLoggingConstants.ONE:
				final List<String> failedBinBarcodeList = getBlInventoryScanToolService().getFailedBinBarcodeList(barcodes);
				createSuccessResponse(barcodes, failedBinBarcodeList);
				break;

			case BlInventoryScanLoggingConstants.TWO:
				notifyInvalidScan(BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.LAST_LOCATION_ERROR_KEY, StringUtils.EMPTY);
				break;

			case BlInventoryScanLoggingConstants.THREE:
				notifyInvalidScan(BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.LAST_INVALID_LOCATION_ERROR, StringUtils.EMPTY);
				break;

			default:
				notifyInvalidScan(BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.LAST_LOCATION_ERROR_KEY, StringUtils.EMPTY);
		}
	}

	/**
	 * This method is used to create response message for package scan while scanning items to tracking Id
	 *
	 * @param checkValidTrackingId
	 * @param barcodes
	 */
	private void createReponseMsgForPackageScan(final int result, final List<String> barcodes)
	{
		switch (result)
		{
			case BlInventoryScanLoggingConstants.ONE:
				final List<String> failedPackageBarcodeList = getBlInventoryScanToolService().getFailedPackageBarcodeList(barcodes);
				createSuccessResponse(barcodes, failedPackageBarcodeList);
				break;

			case BlInventoryScanLoggingConstants.TWO:
				notifyInvalidScan(BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.LAST_LOCATION_ERROR_KEY, StringUtils.EMPTY);
				break;

			default:
				BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE_MSG,
						StringUtils.EMPTY);
				throw new WrongValueException(this.scanningArea, this.getLabel(BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR));
		}

	}

	/**
	 * This method is used to create success response
	 *
	 * @param barcodes
	 * @param failedBinBarcodeList
	 */
	private void createSuccessResponse(final List<String> barcodes, final List<String> failedBinBarcodeList)
	{
		if (CollectionUtils.isEmpty(failedBinBarcodeList))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG,
					barcodes.size());
			Messagebox.show(BlInventoryScanLoggingConstants.SCANNING_SUCCESS_MSG);
			this.scanningArea.setValue(BlInventoryScanLoggingConstants.EMPTY_STRING);
		}
		else
		{
			notifyInvalidScan(BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE_MSG,
					BlInventoryScanLoggingConstants.INVALID_LOCATION_ERROR, failedBinBarcodeList);
		}
	}

	/**
	 * This method is used to create the response message for scan
	 *
	 * @param result
	 * @param barcodes
	 */
	private void createResponseMegForScan(final int result, final List<String> barcodes)
	{
		switch (result)
		{
			case BlInventoryScanLoggingConstants.ONE:
				final int checkValidTrackingId = getBlInventoryScanToolService().checkValidTrackingId(barcodes.get(0));
				if (checkValidTrackingId == BlInventoryScanLoggingConstants.ONE)
				{
					getBlInventoryScanToolService().updateToUpsBound();

					BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG,
							barcodes.size());
					Messagebox.show(BlInventoryScanLoggingConstants.SCANNING_SUCCESS_MSG);
					this.scanningArea.setValue(BlInventoryScanLoggingConstants.EMPTY_STRING);
				}
				break;

			case BlInventoryScanLoggingConstants.TWO:
				notifyInvalidScan(BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.LAST_LOCATION_ERROR_KEY, StringUtils.EMPTY);
				break;

			case BlInventoryScanLoggingConstants.THREE:
				notifyInvalidScan(BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.LAST_INVALID_LOCATION_ERROR, StringUtils.EMPTY);
				break;

			default:
				notifyInvalidScan(BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR, StringUtils.EMPTY);
		}
	}

	/**
	 * This method is used to notify user for invalid scan
	 *
	 * @param logMsg
	 * @param exceptionLabel
	 * @param logParam
	 * @throws WrongValueException
	 */
	private void notifyInvalidScan(final String logMsg, final String exceptionLabel, final Object... logParam)
			throws WrongValueException
	{
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, logMsg, logParam);
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

