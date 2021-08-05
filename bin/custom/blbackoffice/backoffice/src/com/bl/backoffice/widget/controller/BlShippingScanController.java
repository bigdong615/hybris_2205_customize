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


/**
 * This class is responsible to scan the items to working location and then verify the scanned items
 *
 * @author Aditi Sharma
 */

public class BlShippingScanController extends DefaultWidgetController
{
	protected static final String OUT_CONFIRM = "confirmOutput";
	protected static final String COMPLETE = "completed";

	private static final Logger LOG = Logger.getLogger(BlShippingScanController.class);

	@Value("${blbackoffice.max.product.scan}")
	private int maxProductScan;

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
				.setTitle(String.valueOf(this.getWidgetInstanceManager().getLabel("blbackoffice.shipping.scan.heading")));
		shippingScanToolData = new WebScanToolData();
	}

	/**
	 * This method is used to update the barcode values on change event
	 */
	@ViewEvent(componentID = BlInventoryScanLoggingConstants.SCANNING_AREA, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void changeScan()
	{
		shippingScanToolData
				.setBarcodeInputField(Arrays.asList(scanningArea.getValue().split(BlInventoryScanLoggingConstants.NEW_LINE)));
	}

	/**
	 * This method is used to close the Package Shipping Popup
	 */
	@ViewEvent(componentID = BlInventoryScanLoggingConstants.CANCEL_EVENT, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void cancel()
	{
		this.sendOutput(OUT_CONFIRM, COMPLETE);
	}

	/**
	 * This method is used to Scan bin to working location
	 */
	@ViewEvent(componentID = BlInventoryScanLoggingConstants.SCAN_TO_BIN, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void scanToBinLocation()
	{
		if (CollectionUtils.isEmpty(shippingScanToolData.getBarcodeInputField()))
		{
			notifyInvalidScan(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
					BlInventoryScanLoggingConstants.SHIPPING_NO_ITEM_SCAN_KEY, StringUtils.EMPTY);
		}
		else
		{
			final List<String> barcodes = shippingScanToolData.getBarcodeInputField();
			createResponseForShippingScan(barcodes);
		}
	}


	/**
	 * This method is used to verify shipping Scan
	 */
	@ViewEvent(componentID = BlInventoryScanLoggingConstants.VERIFY_SCAN_EVENT, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void verifyScan()
	{
		if (CollectionUtils.isEmpty(shippingScanToolData.getBarcodeInputField()))
		{
			notifyInvalidScan(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
					BlInventoryScanLoggingConstants.SHIPPING_NO_ITEM_SCAN_KEY, StringUtils.EMPTY);
		}
		else
		{
			final List<String> barcodes = shippingScanToolData.getBarcodeInputField();
			final int barcodeSize = barcodes.size();
			if (barcodeSize >= BlInventoryScanLoggingConstants.TWO && barcodeSize <= maxProductScan)
			{
				final List<String> scannedBarcodeList = getBlInventoryScanToolService().verifyShippingScan(barcodes,
						selectedConsignment);
				createResponseForScan(scannedBarcodeList);
			}
			else
			{
				notifyInvalidScan(BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.SHIPPING_INVALID_SCAN_ERROR, StringUtils.EMPTY);
			}
		}
	}

	/**
	 * This method is used to create shipping scan response
	 *
	 * @param barcodes
	 */
	private void createResponseForShippingScan(final List<String> barcodes)
	{
		final int barcodeSize = barcodes.size();
		if (barcodeSize == BlInventoryScanLoggingConstants.TWO)
		{
			createResponseMegForShippingScan(getBlInventoryScanToolService().checkLocationWithType(barcodes,
					BlInventoryScanUtility.getShippingWorkstationInitial(), BlInventoryScanUtility.getShippingAllowedLocations()),
					barcodes);
		}
		else
		{
			notifyInvalidScan(BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_MSG,
					BlInventoryScanLoggingConstants.SHIPPING_SCAN_ITEM_ERROR, StringUtils.EMPTY);
		}
	}

	/**
	 * This method is used to create response message for shipping scan
	 *
	 * @param result
	 * @param barcodes
	 */
	private void createResponseMegForShippingScan(final int result, final List<String> barcodes)
	{
		switch (result)
		{
			case BlInventoryScanLoggingConstants.ONE:
				final List<String> failedBinBarcodeList = getBlInventoryScanToolService().getFailedBinBarcodeList(barcodes);
				createResponseForScan(failedBinBarcodeList);
				break;

			case BlInventoryScanLoggingConstants.TWO:
				notifyInvalidScan(BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.SHIPPING_LAST_LOCATION_ERROR_KEY, StringUtils.EMPTY);
				break;

			case BlInventoryScanLoggingConstants.THREE:
				notifyInvalidScan(BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.SHIPPING_LAST_INVALID_LOCATION_ERROR, StringUtils.EMPTY);
				break;

			default:
				notifyInvalidScan(BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.SHIPPING_MANY_LOCATION_ERROR, StringUtils.EMPTY);
		}

	}

	/**
	 * This method is used to notify for invalid scan
	 *
	 * @param logMsg
	 * @param exceptionLabel
	 * @param logParam
	 * @throws WrongValueException
	 */
	private void notifyInvalidScan(final String logMsg, final String exceptionLabel, final Object... logParam)
	{
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, logMsg, logParam);
		throw new WrongValueException(this.scanningArea, this.getLabel(exceptionLabel));
	}


	/**
	 * This method is used to create success response for shipping scan
	 *
	 * @param barcodes
	 * @param scannedBarcodeList
	 */
	private void createResponseForScan(final List<String> scannedBarcodeList)
	{
		if (CollectionUtils.isNotEmpty(scannedBarcodeList))
		{
			notifyInvalidScan(BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE_MSG,
					BlInventoryScanLoggingConstants.SHIPPING_INVALID_LOCATION_ERROR, scannedBarcodeList);
		}
		else
		{
			BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG);
			Messagebox.show(BlInventoryScanLoggingConstants.SCANNING_SUCCESS_MSG);
			this.scanningArea.setValue(BlInventoryScanLoggingConstants.EMPTY_STRING);
		}
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

