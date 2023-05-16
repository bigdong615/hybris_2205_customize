package com.bl.backoffice.widget.controller;

import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.util.Config;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.annotation.Resource;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.WrongValueException;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zul.Checkbox;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Textbox;

import com.bl.backoffice.wizards.util.WebScanToolData;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.inventory.scan.dao.BlInventoryScanToolDao;
import com.bl.core.inventory.scan.service.BlInventoryScanToolService;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
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

	@Wire
	Textbox scanningArea;

	@Wire
	private Checkbox doNotAutoClose;

	private transient WebScanToolData shippingScanToolData;

	@Resource(name = "blInventoryScanToolService")
	private BlInventoryScanToolService blInventoryScanToolService;

	@Resource(name = "blInventoryScanToolDao")
	BlInventoryScanToolDao blInventoryScanToolDao;

	@Resource(name = "modelService")
	ModelService modelService;

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
			validateShippingBinLocation();
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
			notifyErrorMessage(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
					BlInventoryScanLoggingConstants.SHIPPING_NO_ITEM_SCAN_KEY);
		}
		else
		{
			validateShippingScan();
		}
	}

	/**
	 * method will used to validate shipping scan
	 */
	private void validateShippingScan()
	{
		final List<String> barcodes = shippingScanToolData.getBarcodeInputField();

		if (OrderStatus.CANCELLED.equals(selectedConsignment.getOrder().getStatus()))
		{
			final Map<String, String> errorMap = createErrorMap(barcodes);
			barcodes.forEach(serialBarcode -> createErrorResponseForCancel(errorMap, serialBarcode));

		}
		else if (ConsignmentStatus.SHIPPING_MANUAL_REVIEW.equals(selectedConsignment.getStatus()))
		{
			final Map<String, String> errorMap = createErrorMap(barcodes);
			barcodes.forEach(serialBarcode -> createErrorResponseForManualReview(errorMap, serialBarcode));
		}
		else
		{
			verifyShippingScan(barcodes);
		}
	}

	/**
	 * method will be used to create error response for order in MANUAL REVIEW state
	 *
	 * @param errorMap
	 * @param serialBarcode
	 */
	private void createErrorResponseForManualReview(final Map<String, String> errorMap, final String serialBarcode)
	{
		if (errorMap.containsKey(serialBarcode))
		{

			notifyInvalidScan(BlInventoryScanLoggingConstants.SHIPPING_MANUAL_REVIEW_FAILURE_MSG,
					BlInventoryScanLoggingConstants.SHIPPING_MANUAL_REVIEW_FAILURE, serialBarcode, errorMap.get(serialBarcode));

		}
		else
		{
			notifyInvalidScan(BlInventoryScanLoggingConstants.SHIPPING_MANUAL_REVIEW_FAILURE_MSG,
					BlInventoryScanLoggingConstants.SHIPPING_MANUAL_REVIEW_FAILURE, serialBarcode, StringUtils.EMPTY);

		}
	}

	/**
	 * method will be used to create error response for order in CANCEL state
	 *
	 * @param errorMap
	 * @param serialBarcode
	 */
	private void createErrorResponseForCancel(final Map<String, String> errorMap, final String serialBarcode)
	{
		if (errorMap.containsKey(serialBarcode))
		{

			notifyInvalidScan(BlInventoryScanLoggingConstants.SHIPPING_CANCEL_ORDER_FAILURE_MSG,
					BlInventoryScanLoggingConstants.SHIPPING_CANCEL_ORDER_FAILURE, serialBarcode, errorMap.get(serialBarcode));

		}
		else
		{
			notifyInvalidScan(BlInventoryScanLoggingConstants.SHIPPING_CANCEL_ORDER_FAILURE_MSG,
					BlInventoryScanLoggingConstants.SHIPPING_CANCEL_ORDER_FAILURE, serialBarcode, StringUtils.EMPTY);

		}
	}

	/**
	 * method wil be used to create error Map for boxing response
	 *
	 * @param barcodes
	 * @return
	 */
	private Map<String, String> createErrorMap(final List<String> barcodes)
	{
		final Collection<BlSerialProductModel> serialProductsByBarcode = getBlInventoryScanToolDao()
				.getSerialProductsByBarcode(barcodes);

		return serialProductsByBarcode.stream()
				.collect(Collectors.toMap(BlSerialProductModel::getBarcode, serial -> serial.getBlProduct().getName()));
	}

	/**
	 * method will verify shipping scan
	 *
	 * @param barcodes
	 */
	private void verifyShippingScan(final List<String> barcodes)
	{
		final int barcodeSize = barcodes.size();
		if (barcodeSize >= BlInventoryScanLoggingConstants.ONE)
		{
			final Map<String, List<BlProductModel>> scannedBarcodeMap = getBlInventoryScanToolService().verifyShippingScan(barcodes,
					selectedConsignment);
			createResponseMsg(scannedBarcodeMap);
		}
		else
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
					BlInventoryScanLoggingConstants.SHIPPING_INVALID_SCAN_ERROR);
		}
	}

	/**
	 * method will shipping validate bin location
	 */
	private void validateShippingBinLocation()
	{
		if (OrderStatus.CANCELLED.equals(selectedConsignment.getOrder().getStatus()))
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.SHIPPING_SCAN_CANCEL_ORDER_FAILURE_MSG,
					BlInventoryScanLoggingConstants.SHIPPING_SCAN_CANCEL_ORDER_FAILURE);
		}
		else if (ConsignmentStatus.SHIPPING_MANUAL_REVIEW.equals(selectedConsignment.getStatus()))
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.SHIPPING_SCAN_MANUAL_REVIEW_FAILURE_MSG,
					BlInventoryScanLoggingConstants.SHIPPING_SCAN_MANUAL_REVIEW_FAILURE);
		}
		else
		{
			final List<String> barcodes = shippingScanToolData.getBarcodeInputField();
			createResponseForShippingScan(barcodes);
		}
	}

	/**
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
					getErrorMsgForMissingOnConsignment(scannedBarcodeMap);
				}

				if (scannedBarcodeMap.containsKey(BlInventoryScanLoggingConstants.MISSING_IN_SCAN))
				{
					final List<BlProductModel> serialCode = scannedBarcodeMap.get(BlInventoryScanLoggingConstants.MISSING_IN_SCAN);
					final List<String> serialProductCode = createErrorMsgForScan(serialCode);

					notifyInvalidScan(BlInventoryScanLoggingConstants.SERIAL_MISSING_ON_SCAN_MSG,
							BlInventoryScanLoggingConstants.SHIPPING_SERIAL_MISSING_ON_SCAN_KEY, serialProductCode,
							selectedConsignment.getOrder().getCode());
				}
				if (scannedBarcodeMap.containsKey(BlInventoryScanLoggingConstants.MISSING_SCAN_BARCODE))
				{
					getErrorMsgForMissingScanBarcode(scannedBarcodeMap);
				}
			}
		}
	}

	/**
	 * Create response message for shipping scan
	 *
	 * @param scannedBarcodeMap
	 */
	private void createResponseMsg(final Map<String, List<BlProductModel>> scannedBarcodeMap) {

		if (scannedBarcodeMap.containsKey(BlInventoryScanLoggingConstants.SUCCESS_SERIAL)
				&& !scannedBarcodeMap.containsKey(BlInventoryScanLoggingConstants.INCLUDED_SERIAL)
				&& !scannedBarcodeMap.containsKey(BlInventoryScanLoggingConstants.OUTSIDER_BARCODE)) {
			BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG);
			Messagebox.show(BlInventoryScanLoggingConstants.SCANNING_SUCCESS_MSG);
			if (!isDoNotAutoClose())
			{
				this.sendOutput(OUT_CONFIRM, COMPLETE);
			}
			this.scanningArea.setValue(BlInventoryScanLoggingConstants.EMPTY_STRING);
		} else {
			final StringBuffer message = new StringBuffer(
					"Scan result for " + selectedConsignment.getOrder().getCode() + " order");
			if (scannedBarcodeMap.containsKey(BlInventoryScanLoggingConstants.SUCCESS_SERIAL)) {
				message.append("\nAll successful scan barcode :");
				createMessage(message, scannedBarcodeMap, BlInventoryScanLoggingConstants.SUCCESS_SERIAL);
				if (!isDoNotAutoClose())
				{
					this.sendOutput(OUT_CONFIRM, COMPLETE);
				}
			}
			if (scannedBarcodeMap.containsKey(BlInventoryScanLoggingConstants.INCLUDED_SERIAL)) {
				message.append("\nAlready included barcode :");
				createMessage(message, scannedBarcodeMap, BlInventoryScanLoggingConstants.INCLUDED_SERIAL);
			}
			if (scannedBarcodeMap.containsKey(BlInventoryScanLoggingConstants.OUTSIDER_BARCODE)) {
				message.append("\nThis barcode not belongs to current consignment :");
				createMessage(message, scannedBarcodeMap, BlInventoryScanLoggingConstants.OUTSIDER_BARCODE);
			}
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, message.toString());
			Messagebox.show(message.toString());
		}

	}

	private void createMessage(StringBuffer message,
			final Map<String, List<BlProductModel>> scannedBarcodeMap, String key) {

		List<BlProductModel> blProduct = scannedBarcodeMap.get(key);
		blProduct.forEach(blProductModel -> {
			if (blProductModel instanceof BlSerialProductModel) {
				message.append(((BlSerialProductModel) blProductModel).getBarcode()).append(", ");
			}
		});
		message.deleteCharAt(message.length() - 1);
		message.deleteCharAt(message.length() - 1);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, message.toString(), blProduct);
	}
	/**
	 * @param scannedBarcodeMap
	 */
	private void getErrorMsgForMissingOnConsignment(final Map<String, List<BlProductModel>> scannedBarcodeMap)
	{
		final List<BlProductModel> serialCode = scannedBarcodeMap.get(BlInventoryScanLoggingConstants.MISSING_IN_CONSIGNMENT);
		final List<String> serialProductCode = createErrorMsgForScan(serialCode);

		notifyInvalidScan(BlInventoryScanLoggingConstants.SERIAL_MISSING_ON_CONSIGNMENT_MSG,
				BlInventoryScanLoggingConstants.SHIPPING_SERIAL_MISSING_ON_CONSIGNMENT_KEY, serialProductCode,
				selectedConsignment.getOrder().getCode());
	}

	/**
	 * @param scannedBarcodeMap
	 */
	private void getErrorMsgForMissingScanBarcode(final Map<String, List<BlProductModel>> scannedBarcodeMap)
	{
		final List<BlProductModel> serialCode = scannedBarcodeMap.get(BlInventoryScanLoggingConstants.MISSING_SCAN_BARCODE);
		final List<String> serialProductCode = createErrorMsgForScan(serialCode);

		notifyInvalidScan(BlInventoryScanLoggingConstants.SERIAL_MISSING_ON_CONSIGNMENT_MSG,
				BlInventoryScanLoggingConstants.SHIPPING_SERIAL_MISSING_ON_CONSIGNMENT_KEY, serialProductCode,
				selectedConsignment.getOrder().getCode());
	}

	/**
	 * @param serialCode
	 * @return
	 */
	private List<String> createErrorMsgForScan(final List<BlProductModel> serialCode)
	{
		final List<String> errorList = new ArrayList<>();
		serialCode.forEach(serial -> {
			if (serial instanceof BlSerialProductModel)
			{
				final BlSerialProductModel blSerialProduct = (BlSerialProductModel) serial;
				errorList.add(blSerialProduct.getBarcode().concat(BlInventoryScanLoggingConstants.PRODUCT_TEXT)
						.concat(blSerialProduct.getBlProduct().getName()));
			}
		});
		return errorList;
	}

	/**
	 * This method is used to create shipping scan response
	 *
	 * @param barcodes
	 */
	private void createResponseForShippingScan(final List<String> barcodes)
	{
		final int barcodeSize = barcodes.size();

		if (barcodeSize == BlInventoryScanLoggingConstants.TWO
				&& barcodeSize <= Config.getInt("blbackoffice.max.product.scan", BlInventoryScanLoggingConstants.HUNDERED))
		{
			createResponseMegForShippingScan(getBlInventoryScanToolService().checkLocationWithType(barcodes,
					BlInventoryScanUtility.getShippingWorkstationInitial(), BlInventoryScanUtility.getShippingAllowedLocations()),
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
	private void createResponseMegForShippingScan(final int result, final List<String> barcodes)
	{
		switch (result)
		{
			case BlInventoryScanLoggingConstants.ONE:
				final Map<Integer, List<String>> failedBinBarcodeMap = getBlInventoryScanToolService()
						.getFailedBinBarcodeList(barcodes);
				if (MapUtils.isNotEmpty(failedBinBarcodeMap))
				{
					createResponseForScan(failedBinBarcodeMap, barcodes);
				}
				break;

			case BlInventoryScanLoggingConstants.TWO:
				notifyErrorMessage(BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.SHIPPING_LAST_LOCATION_ERROR_KEY);
				break;

			case BlInventoryScanLoggingConstants.THREE:
				notifyErrorMessage(BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.SHIPPING_LAST_INVALID_LOCATION_ERROR);
				break;

			default:
				notifyErrorMessage(BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.SHIPPING_MANY_LOCATION_ERROR);
		}

	}

	/**
	 * This method is used to create success response for shipping scan
	 *
	 * @param barcodes
	 * @param failedBinBarcodeMap
	 */
	private void createResponseForScan(final Map<Integer, List<String>> failedBinBarcodeMap, final List<String> barcodes)
	{
		final String lastScannedItem = (barcodes.get(barcodes.size() - BlInventoryScanLoggingConstants.ONE));
		if (failedBinBarcodeMap.containsKey(BlInventoryScanLoggingConstants.ZERO))
		{
			getBlInventoryScanToolService().updateSerialLastScanLocation(selectedConsignment, lastScannedItem);
			BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.SCAN_BARCODE_SUCCESS_MSG);
			Messagebox.show(BlInventoryScanLoggingConstants.SCANNING_SUCCESS_MSG);
			if (!isDoNotAutoClose())
			{
				this.sendOutput(OUT_CONFIRM, COMPLETE);
			}
			this.scanningArea.setValue(BlInventoryScanLoggingConstants.EMPTY_STRING);
		}
		if (failedBinBarcodeMap.containsKey(BlInventoryScanLoggingConstants.ONE))
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.FIRST_SCAN_EMPTY_ERROR_FAILURE_MSG,
					BlInventoryScanLoggingConstants.SHIPPING_FIRST_SCAN_EMPTY_ERROR_FAILURE_KEY);
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
		throw new WrongValueException(this.scanningArea, this.getLabel(exceptionLabel, logParam));
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

	/**
	 * @return the blInventoryScanToolDao
	 */
	public BlInventoryScanToolDao getBlInventoryScanToolDao()
	{
		return blInventoryScanToolDao;
	}

	/**
	 * @param blInventoryScanToolDao
	 *           the blInventoryScanToolDao to set
	 */
	public void setBlInventoryScanToolDao(final BlInventoryScanToolDao blInventoryScanToolDao)
	{
		this.blInventoryScanToolDao = blInventoryScanToolDao;
	}

	private boolean isDoNotAutoClose()
	{
		return getDoNotAutoClose().isChecked();
	}

	/**
	 * @return the doNotAutoClose
	 */
	public Checkbox getDoNotAutoClose()
	{
		return doNotAutoClose;
	}

	/**
	 * @param doNotAutoClose
	 *           the doNotAutoClose to set
	 */
	public void setDoNotAutoClose(final Checkbox doNotAutoClose)
	{
		this.doNotAutoClose = doNotAutoClose;
	}


}

