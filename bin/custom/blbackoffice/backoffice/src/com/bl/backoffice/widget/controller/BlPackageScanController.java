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
import com.bl.core.utils.BlInventoryScanUtility;
import com.bl.logging.BlLogger;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;


/**
 * This class is responsible to scan the package and update there location to BIN,Tracking Id and to UPS OUTBOUND cart
 * on different step of scanning
 *
 * @author Aditi Sharma
 */

public class BlPackageScanController extends DefaultWidgetController
{
	protected static final String OUT_CONFIRM = "confirmOutput";
	protected static final String COMPLETE = "completed";

	private static final Logger LOG = Logger.getLogger(BlPackageScanController.class);

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
				.setTitle(String.valueOf(this.getWidgetInstanceManager().getLabel("blbackoffice.order.scan.heading")));
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
	 * This method is used to close the Package Shipping Popup
	 */
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
			notifyErrorMessage(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
					BlInventoryScanLoggingConstants.NO_ITEM_SCAN_KEY);
		}
		else
		{
			validatePackageBinLocation();
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
			notifyErrorMessage(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
					BlInventoryScanLoggingConstants.NO_ITEM_SCAN_KEY);
		}
		else
		{
			final List<String> barcodes = shippingScanToolData.getBarcodeInputField();
			final int barcodeSize = barcodes.size();
			if (barcodeSize >= BlInventoryScanLoggingConstants.TWO
					&& barcodeSize <= Config.getInt("blbackoffice.max.product.scan", BlInventoryScanLoggingConstants.HUNDERED))
			{
				validatePackageScan(barcodes);

			}
			else
			{
				notifyErrorMessage(BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.INVALID_SCAN_ERROR);
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
			notifyErrorMessage(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
					BlInventoryScanLoggingConstants.NO_ITEM_SCAN_KEY);
		}
		else
		{
			final List<String> barcodes = shippingScanToolData.getBarcodeInputField();
			if (CollectionUtils.isEmpty(barcodes))
			{
				notifyErrorMessage(BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.INVALID_SCAN_ERROR);
			}
			else
			{
				scanToUpsOutBoundCart(barcodes);
			}
		}
	}

	/**
	 * method will used to scan serial to ups out bound cart
	 *
	 * @param barcodes
	 */
	private void scanToUpsOutBoundCart(final List<String> barcodes)
	{
		if (OrderStatus.CANCELLED.equals(selectedConsignment.getOrder().getStatus()))
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.SHIPPING_CANCEL_ORDER_FAILURE_MSG,
					BlInventoryScanLoggingConstants.PACKAGE_CANCEL_ORDER_FAILURE);
		}
		else if (ConsignmentStatus.SHIPPING_MANUAL_REVIEW.equals(selectedConsignment.getStatus()))
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.SHIPPING_MANUAL_REVIEW_FAILURE_MSG,
					BlInventoryScanLoggingConstants.PACKAGE_MANUAL_REVIEW_FAILURE);
		}
		else
		{
			createResponseMegForScan(getBlInventoryScanToolService().checkLocationWithType(barcodes,
					BlInventoryScanUtility.getShippingWorkstationInitial(), BlInventoryScanUtility.getShippingAllowedLocations()),
					barcodes);
		}

	}

	/**
	 * method will use to validate package bin location
	 */
	private void validatePackageBinLocation()
	{
		if (OrderStatus.CANCELLED.equals(selectedConsignment.getOrder().getStatus()))
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.SHIPPING_CANCEL_ORDER_FAILURE_MSG,
					BlInventoryScanLoggingConstants.PACKAGE_CANCEL_ORDER_FAILURE);
		}
		else if (ConsignmentStatus.SHIPPING_MANUAL_REVIEW.equals(selectedConsignment.getStatus()))
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.SHIPPING_MANUAL_REVIEW_FAILURE_MSG,
					BlInventoryScanLoggingConstants.PACKAGE_MANUAL_REVIEW_FAILURE);
		}
		else
		{
			final List<String> barcodes = shippingScanToolData.getBarcodeInputField();
			createResponseForPackageScan(barcodes);
		}
	}

	/**
	 * method will used to validate package scan
	 *
	 * @param barcodes
	 */
	private void validatePackageScan(final List<String> barcodes)
	{
		if (OrderStatus.CANCELLED.equals(selectedConsignment.getOrder().getStatus()))
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.SHIPPING_CANCEL_ORDER_FAILURE_MSG,
					BlInventoryScanLoggingConstants.PACKAGE_CANCEL_ORDER_FAILURE);
		}
		else if (ConsignmentStatus.SHIPPING_MANUAL_REVIEW.equals(selectedConsignment.getStatus()))
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.SHIPPING_MANUAL_REVIEW_FAILURE_MSG,
					BlInventoryScanLoggingConstants.PACKAGE_MANUAL_REVIEW_FAILURE);
		}

		else
		{
			final String lastScannedItem = (barcodes.get(barcodes.size() - BlInventoryScanLoggingConstants.ONE));
			createReponseMsgForPackageScan(getBlInventoryScanToolService().checkValidTrackingId(lastScannedItem), barcodes);
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
		if (barcodeSize == BlInventoryScanLoggingConstants.TWO
				&& barcodeSize <= Config.getInt("blbackoffice.max.product.scan", BlInventoryScanLoggingConstants.HUNDERED))
		{
			createResponseMsgForPackageScan(getBlInventoryScanToolService().checkLocationWithType(barcodes,
					BlInventoryScanUtility.getShippingWorkstationInitial(), BlInventoryScanUtility.getShippingAllowedLocations()),
					barcodes);
		}
		else
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.TWO_BARCODE_SCAN_ERROR_MSG,
					BlInventoryScanLoggingConstants.TWO_BARCODE_SCAN_ERROR_KEY);
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
				final Map<Integer, List<String>> failedBinBarcodeMap = getBlInventoryScanToolService()
						.getFailedBinBarcodeList(barcodes);
				createSuccessResponse(failedBinBarcodeMap);
				break;

			case BlInventoryScanLoggingConstants.TWO:
				notifyErrorMessage(BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE_KEY);
				break;

			case BlInventoryScanLoggingConstants.THREE:
				notifyErrorMessage(BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.LAST_SCAN_LOCATION_ERROR_FAILURE_KEY);
				break;

			default:
				notifyErrorMessage(BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_KEY);
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
				final Map<Integer, List<String>> failedPackageBarcodeList = getBlInventoryScanToolService()
						.getFailedPackageBarcodeList(barcodes);
				if (MapUtils.isNotEmpty(failedPackageBarcodeList))
				{
					createSuccessResponse(failedPackageBarcodeList);
				}
				break;

			case BlInventoryScanLoggingConstants.TWO:
				notifyErrorMessage(BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.LAST_LOCATION_VALID_TRACKING_FAILURE);
				break;

			default:
				notifyErrorMessage(BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_KEY);
		}
	}

	/**
	 * This method is used to create success response
	 *
	 * @param failedBinBarcodeList
	 */
	private void createSuccessResponse(final Map<Integer, List<String>> failedBinBarcodeMap)
	{
		if (failedBinBarcodeMap.containsKey(BlInventoryScanLoggingConstants.ZERO))
		{
			BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG);
			Messagebox.show(BlInventoryScanLoggingConstants.SCANNING_SUCCESS_MSG);
			this.scanningArea.setValue(BlInventoryScanLoggingConstants.EMPTY_STRING);
		}

		else if (failedBinBarcodeMap.containsKey(BlInventoryScanLoggingConstants.ONE))
		{
			notifyInvalidScan(BlInventoryScanLoggingConstants.INVALID_BARCODE_SCANNED_MSG,
					BlInventoryScanLoggingConstants.INVALID_BARCODE_SCANNED_ERROR, failedBinBarcodeMap);
		}

		else if (failedBinBarcodeMap.containsKey(BlInventoryScanLoggingConstants.TWO))
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.SERIAL_MISMATCH_SCANNED_MSG,
					BlInventoryScanLoggingConstants.SERIAL_MISMATCH_SCANNED_ERROR);
		}

		else if (failedBinBarcodeMap.containsKey(BlInventoryScanLoggingConstants.THREE))
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.INVALID_SCAN_MSG,
					BlInventoryScanLoggingConstants.INVALID_SCAN_ERROR_KEY);
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

					BlLogger.logMessage(LOG, Level.INFO, BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG);
					Messagebox.show(BlInventoryScanLoggingConstants.SCANNING_SUCCESS_MSG);
					this.scanningArea.setValue(BlInventoryScanLoggingConstants.EMPTY_STRING);
				}
				break;

			case BlInventoryScanLoggingConstants.TWO:
				notifyErrorMessage(BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE_KEY);
				break;

			case BlInventoryScanLoggingConstants.THREE:
				notifyErrorMessage(BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.LAST_SCAN_LOCATION_ERROR_FAILURE_KEY);
				break;

			default:
				notifyErrorMessage(BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_KEY);
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

