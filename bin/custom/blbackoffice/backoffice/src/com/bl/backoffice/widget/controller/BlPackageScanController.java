/**
 *@author Keyur Patel
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
import com.bl.logging.BlLogger;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;


public class BlPackageScanController extends DefaultWidgetController
{
	protected static final String OUT_CONFIRM = "confirmOutput";
	protected static final String COMPLETE = "completed";

	private static final Logger LOG = Logger.getLogger(BlPackageScanController.class);

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
		if (shippingScanToolData.getBarcodeInputField() == null)
		{
			notifyErrorForEmptyScan();
		}
		else
		{
			final List<String> barcodes = shippingScanToolData.getBarcodeInputField();
			if (CollectionUtils.isNotEmpty(barcodes))
			{
				createResponseForShippingScan(barcodes);
			}
			else
			{
				invalidNumberBarCode();
			}
		}
	}


	@ViewEvent(componentID = "scanToTrackingLabel", eventName = "onClick")
	public void scanToTrackingLabel()
	{
		if (shippingScanToolData.getBarcodeInputField() == null)
		{
			notifyErrorForEmptyScan();
		}
		else
		{
			final List<String> barcodes = shippingScanToolData.getBarcodeInputField();
			if (CollectionUtils.isNotEmpty(barcodes))
			{
				getBlInventoryScanToolService().verifyShippingScan(barcodes, selectedConsignment);
			}
			else
			{
				invalidNumberBarCode();
			}
		}

	}

	@ViewEvent(componentID = "scanToOutBoundCart", eventName = "onClick")
	public void scanToOutBoundCart()
	{
		if (shippingScanToolData.getBarcodeInputField() == null)
		{
			notifyErrorForEmptyScan();
		}
		else
		{
			final List<String> barcodes = shippingScanToolData.getBarcodeInputField();
			if (CollectionUtils.isNotEmpty(barcodes))
			{
				createResponseMegForScan(
						getBlInventoryScanToolService().checkValidLocationInBarcodeList(barcodes, Collections.emptyList()), barcodes);
			}
			else
			{
				invalidNumberBarCode();
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
			createResponseMegForShippingScan(
					getBlInventoryScanToolService().checkValidLocationInBarcodeList(barcodes, Collections.emptyList()), barcodes);
		}
		else
		{
			invalidNumberBarCode();
		}
	}

	/**
	 * @param result
	 * @param barcodes
	 */
	private void createResponseMegForShippingScan(final int result, final List<String> barcodes)
	{
		if (result == BlInventoryScanLoggingConstants.ONE)
		{
			final List<String> failedBinBarcodeList = getBlInventoryScanToolService().getFailedBinBarcodeList(barcodes);
			if (CollectionUtils.isNotEmpty(failedBinBarcodeList))
			{
				BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE_MSG,
						failedBinBarcodeList);
				throw new WrongValueException(this.scanningArea,
						this.getLabel("blbackoffice.shipping.scan.invalid.location.error" + failedBinBarcodeList));
			}
			else
			{
				BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG,
						barcodes.size());
				Messagebox.show("Scanning completed successfully");
				this.scanningArea.setValue("");
			}
		}
		else if (result == BlInventoryScanLoggingConstants.TWO)
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE_MSG,
					StringUtils.EMPTY);
			throw new WrongValueException(this.scanningArea, this.getLabel("blbackoffice.shipping.scan.last.location.error"));
		}
		else if (result == BlInventoryScanLoggingConstants.THREE)
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE_MSG,
					StringUtils.EMPTY);
			throw new WrongValueException(this.scanningArea,
					this.getLabel("blbackoffice.shipping.scan.last.invalid.location.error"));
		}
		else
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE_MSG,
					StringUtils.EMPTY);
			throw new WrongValueException(this.scanningArea, this.getLabel("blbackoffice.shipping.scan.many.location.error "));
		}

	}

	/**
	 * javadoc
	 *
	 * @param result
	 * @param barcodes
	 *           method will notify user according to result number calculated in previous method
	 */
	private void createResponseMegForScan(final int result, final List<String> barcodes)
	{
		if (result == BlInventoryScanLoggingConstants.ONE)
		{
			final List<String> failedBarcodeList = getBlInventoryScanToolService().getFailedBarcodeList(barcodes);
			if (CollectionUtils.isNotEmpty(failedBarcodeList))
			{
				BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE_MSG,
						failedBarcodeList);
				throw new WrongValueException(this.scanningArea, this.getLabel("blbackoffice.shipping.scan.invalid.location.error"));
			}
			else
			{
				BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG,
						barcodes.size());
				Messagebox.show("Scanning completed successfully");
				this.scanningArea.setValue("");
			}
		}
		else if (result == BlInventoryScanLoggingConstants.TWO)
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE_MSG,
					StringUtils.EMPTY);
			throw new WrongValueException(this.scanningArea, this.getLabel("blbackoffice.shipping.scan.last.location.error"));
		}
		else if (result == BlInventoryScanLoggingConstants.THREE)
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE_MSG,
					StringUtils.EMPTY);
			throw new WrongValueException(this.scanningArea,
					this.getLabel("blbackoffice.shipping.scan.last.invalid.location.error"));
		}
		else
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE_MSG,
					StringUtils.EMPTY);
			throw new WrongValueException(this.scanningArea, this.getLabel("blbackoffice.shipping.scan.many.location.error "));
		}
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
	 *
	 */
	private void invalidNumberBarCode()
	{
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
				StringUtils.EMPTY);
		throw new WrongValueException(this.scanningArea, this.getLabel("blbackoffice.shipping.scan.invalid.scan.error"));
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

