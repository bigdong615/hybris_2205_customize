/**
 *@author Aditi Sharma
 */
package com.bl.backoffice.widget.controller;

import de.hybris.platform.ordersplitting.model.ConsignmentModel;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import javax.annotation.Resource;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
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


public class BlShippingScanController extends DefaultWidgetController
{
	protected static final String OUT_CONFIRM = "confirmOutput";
	protected static final String COMPLETE = "completed";

	private static final Logger LOG = Logger.getLogger(BlShippingScanController.class);

	@Wire
	Textbox scanningArea;

	WebScanToolData shippingScanToolData;

	@Resource(name = "blInventoryScanToolService")
	private BlInventoryScanToolService blInventoryScanToolService;

	ConsignmentModel selectedConsignment = new ConsignmentModel();

	@SocketEvent(socketId = "inputObject")
	public void initCustomerAddressForm(final ConsignmentModel inputObject)
	{
		selectedConsignment = inputObject;
		this.getWidgetInstanceManager()
				.setTitle(String.valueOf(this.getWidgetInstanceManager().getLabel("blbackoffice.shipping.scan.heading")));
		shippingScanToolData = new WebScanToolData();
	}

	@ViewEvent(componentID = "scanningArea", eventName = "onChange")
	public void changeScan()
	{
		shippingScanToolData.setBarcodeInputField(Arrays.asList(scanningArea.getValue().split("\n")));
	}

	@ViewEvent(componentID = "cancel", eventName = "onClick")
	public void cancel()
	{
		this.sendOutput(OUT_CONFIRM, COMPLETE);
	}

	@ViewEvent(componentID = "scanToBin", eventName = "onClick")
	public void scanToBinLocation()
	{
		if (CollectionUtils.isEmpty(shippingScanToolData.getBarcodeInputField()))
		{
			notifyErrorForEmptyScan();
		}
		else
		{
			final List<String> barcodes = shippingScanToolData.getBarcodeInputField();
			createResponseForShippingScan(barcodes);
		}
	}


	@ViewEvent(componentID = "verifyScan", eventName = "onClick")
	public void verifyScan()
	{
		if (CollectionUtils.isEmpty(shippingScanToolData.getBarcodeInputField()))
		{
			notifyErrorForEmptyScan();
		}
		else
		{
			final List<String> barcodes = shippingScanToolData.getBarcodeInputField();
			final int barcodeSize = barcodes.size();
			if (barcodeSize >= BlInventoryScanLoggingConstants.TWO && barcodeSize <= 100)
			{
				final List<String> scannedBarcodeList = getBlInventoryScanToolService().verifyShippingScan(barcodes,
						selectedConsignment);
				createResponseForScan(barcodes, scannedBarcodeList);
			}
			else
			{
				BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
						StringUtils.EMPTY);
				throw new WrongValueException(this.scanningArea, this.getLabel("blbackoffice.shipping.scan.invalid.scan.error"));
			}
		}
	}

	/**
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
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_MSG,
					StringUtils.EMPTY);
			throw new WrongValueException(this.scanningArea, this.getLabel("blbackoffice.shipping.scan.scan.item.error"));
		}
	}

	/**
	 * @param result
	 * @param barcodes
	 */
	private void createResponseMegForShippingScan(final int result, final List<String> barcodes)
	{
		switch (result)
		{
			case BlInventoryScanLoggingConstants.ONE:
				final List<String> failedBinBarcodeList = getBlInventoryScanToolService().getFailedBinBarcodeList(barcodes);
				createResponseForScan(barcodes, failedBinBarcodeList);
				break;

			case BlInventoryScanLoggingConstants.TWO:
				notifyInvalidScan(BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE_MSG,
						"blbackoffice.shipping.scan.last.location.error", StringUtils.EMPTY);
				break;

			case BlInventoryScanLoggingConstants.THREE:
				notifyInvalidScan(BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE_MSG,
						"blbackoffice.shipping.scan.last.invalid.location.error", StringUtils.EMPTY);
				break;

			default:
				notifyInvalidScan(BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE_MSG,
						"blbackoffice.shipping.scan.many.location.error", StringUtils.EMPTY);
		}

	}

	/**
	 *
	 */
	private void notifyInvalidScan(final String logMsg, final String exceptionLabel, final Object... logParam)
			throws WrongValueException
	{
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, logMsg, logParam);
		throw new WrongValueException(this.scanningArea, this.getLabel(exceptionLabel));
	}

	
	/**
	 *
	 */
	private void notifyErrorForEmptyScan()
	{
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
				StringUtils.EMPTY);
		throw new WrongValueException(this.scanningArea, this.getLabel("blbackoffice.shipping.scan.tool.noitem.scan.error"));
	}

	/**
	 * @param barcodes
	 * @param scannedBarcodeList
	 */
	private void createResponseForScan(final List<String> barcodes, final List<String> scannedBarcodeList)
	{
		if (CollectionUtils.isNotEmpty(scannedBarcodeList))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE_MSG,
					scannedBarcodeList);
			throw new WrongValueException(this.scanningArea,
					this.getLabel("blbackoffice.shipping.scan.invalid.location.error" + scannedBarcodeList));
		}
		else
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG,
					barcodes.size());
			Messagebox.show("Scanning completed successfully");
			this.scanningArea.setValue("");
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

