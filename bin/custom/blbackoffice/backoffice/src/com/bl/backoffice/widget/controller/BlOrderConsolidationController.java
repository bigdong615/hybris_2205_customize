/**
 *
 */
package com.bl.backoffice.widget.controller;

import com.bl.constants.BlloggingConstants;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.stock.BlStockLevelDao;
import com.hybris.backoffice.widgets.notificationarea.event.NotificationEvent;
import com.hybris.cockpitng.util.notifications.NotificationService;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordersplitting.model.*;


import java.util.*;

import javax.annotation.Resource;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.WrongValueException;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zk.ui.select.annotation.WireVariable;
import org.zkoss.zul.Div;
import org.zkoss.zul.Grid;
import org.zkoss.zul.ListModelList;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Textbox;

import com.bl.backoffice.wizards.util.OrderConsolidationData;
import com.bl.backoffice.wizards.util.WebScanToolData;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.inventory.scan.dao.BlInventoryScanToolDao;
import com.bl.core.inventory.scan.service.BlInventoryScanToolService;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;


/**
 * @author Admin
 *
 */
public class BlOrderConsolidationController extends DefaultWidgetController
{
	private static final Logger LOG = Logger.getLogger(BlOrderConsolidationController.class);

	protected static final String IN_SOCKET = "nodeSelected";
	private static final String INVALID_SCAN_ERROR = "blbackoffice.order.scan.invalid.scan.error";
	private static final String HIDE_DIV = "resize:none;display:none";
	private static final String PREPPED_LOCATION_CATEGORY = "CLEAN_AND_READY_TO_SHIP";
	protected static final String OUT_CONFIRM = "confirmOutput";
	protected static final String COMPLETE = "completed";

	private static final String FIRST_ITEM_SCAN_INFO = "blbackoffice.order.consolidation.tool.wizard.notification.first.item.info";
	private static final String LAST_ITEM_SCAN_INFO = "blbackoffice.order.consolidation.tool.wizard.notification.last.item.info";

	private static final String NOT_SHIPPING_SOON = "blbackoffice.order.consolidation.tool.wizard.notification.not.shipping.soon";

	private Textbox textInput;

	@Resource(name = "blInventoryScanToolDao")
	BlInventoryScanToolDao blInventoryScanToolDao;

	@Wire
	private Grid consolidationData;

	@Wire
	private Div consolidationDataHeader;

	@Wire
	Textbox scanningArea;

	private transient WebScanToolData shippingScanToolData;
	private BlInventoryScanToolService blInventoryScanToolService;

	Object selectedConsignment = new Object();

	@Resource
	private transient NotificationService notificationService;

	@SocketEvent(socketId = IN_SOCKET)
	public void initLoadPage(final Object inputObject)
	{
		selectedConsignment = inputObject;
		shippingScanToolData = new WebScanToolData();
		this.getWidgetInstanceManager()
				.setTitle(String.valueOf(this.getWidgetInstanceManager().getLabel("blbackoffice.order.consolidation.heading")));
		this.consolidationDataHeader.setStyle(HIDE_DIV);
	}

	@ViewEvent(componentID = BlInventoryScanLoggingConstants.CANCEL_EVENT, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void cancel()
	{
		this.sendOutput(OUT_CONFIRM, COMPLETE);
	}

	@ViewEvent(componentID = BlInventoryScanLoggingConstants.CANCEL_EVENT_WEB_SCAN_CONSOL, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void cancelWebScanTool()
	{
		this.sendOutput(OUT_CONFIRM, COMPLETE);
	}


	@ViewEvent(componentID = BlInventoryScanLoggingConstants.SCAN_BAR_CODES, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void getOrderConsolidationTool()
	{
		this.consolidationDataHeader.setStyle(HIDE_DIV);
		final List<OrderConsolidationData> orderConsolidationDataList = new ArrayList<>();
		if (StringUtils.isBlank(textInput.getText()))
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
					BlInventoryScanLoggingConstants.NO_ITEM_SCAN_KEY);
			this.consolidationDataHeader.setStyle(HIDE_DIV);
		}
		else
		{
			final BlSerialProductModel serialModel = getBlInventoryScanToolDao().getBlSerialProductByBarcode(textInput.getText());

		if (serialModel != null )
			{
				try
				{
					final List<OrderEntryModel> orderEntries = getBlInventoryScanToolDao().getAllOrderEntries(serialModel.getBlProduct().getCode());
					OrderModel orderModel = null;
					for(OrderEntryModel orderEntryModel : orderEntries) {
						for (final BlProductModel pd : orderEntryModel.getSerialProducts()) {
							if (pd.getCode().equals(serialModel.getCode())) {
								 orderModel = orderEntryModel.getOrder();

								 for(ConsignmentModel consignmentModel : orderModel.getConsignments()){
									 final Date date = consignmentModel.getOptimizedShippingStartDate();
									 final Calendar calender = Calendar.getInstance();
									 calender.setTime(new Date()); // Using today's date
									 calender.add(Calendar.DATE, 5); // Adding 5 days
									 final Date fiveDaysPlus = calender.getTime();
									 calender.add(Calendar.DATE, -10);// Subtracting 5 days
									 final Date fiveDaysMinus = calender.getTime();
									 if(orderEntryModel.getConsignmentEntries().stream().anyMatch(new HashSet<>(consignmentModel.getConsignmentEntries())::contains)){
										 for (final ConsignmentEntryModel consEntry : consignmentModel.getConsignmentEntries()) {
											 if (!date.before(fiveDaysMinus) && !date.after(fiveDaysPlus)) {
												 {
													 for (final BlProductModel serial : consEntry.getSerialProducts()) {
														 final OrderConsolidationData orderConsolidationData = new OrderConsolidationData();
														 orderConsolidationData.setProductName((((BlSerialProductModel) serial).getBlProduct().getName()));
														 orderConsolidationData.setBarCode(((BlSerialProductModel) serial).getBarcode());
														 orderConsolidationData.setOrderNumber(consEntry.getConsignment().getOrder().getCode());
														 orderConsolidationData.setShippingMethod(consEntry.getConsignment().getOrder().getDeliveryMode().getCode());

														 if (((BlSerialProductModel) serial).getOcLocationDetails() != null) {
															 if (((BlSerialProductModel) serial).getOcLocationDetails().getLocationCategory().getCode()
																	 .equals(PREPPED_LOCATION_CATEGORY)) {
																 orderConsolidationData.setLocation(((BlSerialProductModel) serial).getOcLocationDetails().getName());
															 } else {
																 orderConsolidationData.setLocation(" ");
															 }
														 }
														 String parentLocation = "";
														 if (serialModel.getOcLocationDetails() != null && ((BlSerialProductModel) serial).getOcLocationDetails() !=null) {
															 parentLocation = ((BlSerialProductModel) serial).getOcLocationDetails()
																	 .getParentInventoryLocation() != null
																	 ? ((BlSerialProductModel) serial).getOcLocationDetails().getParentInventoryLocation()
																	 .getName()
																	 : " ";
															 orderConsolidationData.setParentLocation(parentLocation);
														 } else {
															 orderConsolidationData.setParentLocation(parentLocation);
														 }
														 orderConsolidationDataList.add(orderConsolidationData);
													 }
												 }
											 }
										 }

									 }
								 }


							}
						}
					}
				}
				catch (final Exception ex)
				{
					BlLogger.logFormatMessageInfo(LOG, Level.ERROR, "Unable to get Consolidation Data for barcode {} due to {}",
							textInput.getText(), ex);
				}
			}
			else
			{
				this.consolidationDataHeader.setStyle(HIDE_DIV);
				notifyErrorMessage(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG, INVALID_SCAN_ERROR);
			}
			if (orderConsolidationDataList.size() > 0)
			{
				this.consolidationDataHeader.setStyle("resize:none;display:block");
				this.getConsolidationData().setModel(new ListModelList<>(orderConsolidationDataList));

				this.getConsolidationData().renderAll();
				createErrorMessages(orderConsolidationDataList);
				scanningArea.setValue(textInput.getText());
				scanningArea.setFocus(true);
				scanningArea.fo

			}
			else{
				//Messagebox.show("order with the given barcode is not Shipping soon");
				notificationService.notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST,
						NotificationEvent.Level.INFO, this.getLabel(NOT_SHIPPING_SOON));
				textInput.setValue("");
				textInput.setFocus(true);
			}
		}


	}


	/**
	 * @param orderConsolidationDataList
	 */
	private void createErrorMessages(final List<OrderConsolidationData> orderConsolidationDataList)
	{
		if (orderConsolidationDataList.size() == 1 && StringUtils.isBlank(orderConsolidationDataList.get(0).getLocation()))
		{
			//Messagebox.show("Last item, please pass gear to shipping.");
			notificationService.notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST,
					NotificationEvent.Level.INFO, this.getLabel(FIRST_ITEM_SCAN_INFO));

		}
		else if (orderConsolidationDataList.stream().allMatch(cData -> StringUtils.isBlank(cData.getLocation())))
		{
			notificationService.notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST,
					NotificationEvent.Level.INFO, this.getLabel(LAST_ITEM_SCAN_INFO));
			//Messagebox.show("First item, please assign a location.");

		}

	}

	@ViewEvent(componentID = "webScanTool", eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void openWebScanTool()
	{

		shippingScanToolData
				.setBarcodeInputField(Arrays.asList(scanningArea.getValue().split(BlInventoryScanLoggingConstants.NEW_LINE)));
		final List<String> barcodes = shippingScanToolData.getBarcodeInputField();

		if (CollectionUtils.isNotEmpty(barcodes))
		{
			createResponseForScanResult(barcodes);
			textInput.setFocus(true);
		}
		else
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
					BlInventoryScanLoggingConstants.NO_ITEM_SCAN_KEY);
		}
	}

	/**
	 * @param barcodes
	 */
	private void createResponseForScanResult(final List<String> barcodes)
	{
		final int barcodeSize = barcodes.size();
		final String maxSequenceScan = getBlInventoryScanToolService()
				.getConfigKeyFromScanConfiguration(BlInventoryScanLoggingConstants.MAX_SEQUENCE_LIMIT_KEY);
		if (barcodeSize >= BlInventoryScanLoggingConstants.TWO && barcodeSize <= Integer.parseInt(maxSequenceScan))
		{
			itemScanSuccess(barcodes, maxSequenceScan);
		}
		else
		{
			if (barcodeSize == BlInventoryScanLoggingConstants.ZERO)
			{
				notifyErrorMessageForWebScan(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
						"blbackoffice.orderConsolidation.scan.must.have.two.error");

				BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
						BlInventoryScanLoggingConstants.SCAN_STRING + barcodes);
				scanningArea.setFocus(true);
			}
			else if (barcodeSize == BlInventoryScanLoggingConstants.ONE)
			{

				if (getBlInventoryScanToolService().checkLastBarcodeIsLocationOrNot(barcodes, maxSequenceScan, true))
				{
					notifyErrorMessageForWebScan(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
							"blbackoffice.orderConsolidation.scan.second.location.error");
					scanningArea.setFocus(true);
				}
				else
				{
					BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.MUST_TWO_BARCODE_ERROR_FAILURE_MSG,
							BlInventoryScanLoggingConstants.SCAN_STRING + barcodes);
					notifyErrorMessageForWebScan(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
							"blbackoffice.orderConsolidation.scan.must.have.two.error");
					scanningArea.setFocus(true);
				}
			}
			else
			{
				notifyErrorMessageForWebScan(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
						"blbackoffice.orderConsolidation.scan.max.barcode.error");
			}
		}


	}

	/**
	 * @param barcodes
	 * @param maxSequenceScan
	 */
	private void itemScanSuccess(final List<String> barcodes, final String maxSequenceScan)
	{
		if (getBlInventoryScanToolService().checkLastBarcodeIsLocationOrNot(barcodes, maxSequenceScan, false))
		{
			if (getBlInventoryScanToolService().checkBINOrSerialScan(barcodes))
			{
				createResponseMegForBINScan(getBlInventoryScanToolService().doBINScanFromWebScanTool(barcodes), barcodes);
			}
			else
			{
				createResponseMegForScan(getBlInventoryScanToolService().checkValidLocationInBarcodeList(barcodes,
						Lists.newArrayList(BlInventoryScanLoggingConstants.ALLOW_SCAN)), barcodes);
			}
		}
		else
		{
			notifyErrorMessageForWebScan(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
					"blbackoffice.orderConsolidation.scan.max.barcode.error");
		}

	}

	/**
	 * @param result
	 * @param barcodes
	 */
	private void createResponseMegForScan(final int result, final List<String> barcodes)
	{
		switch (result)
		{
			case BlInventoryScanLoggingConstants.ONE:
				checkSuccessForScan(barcodes);
				break;

			case BlInventoryScanLoggingConstants.TWO:
				notifyErrorMessageForWebScan(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
						"blbackoffice.orderConsolidation.scan.invalid.error");
				break;

			case BlInventoryScanLoggingConstants.THREE:
				notifyErrorMessageForWebScan(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
						"blbackoffice.orderConsolidation.last.scan.invalid.error");
				break;

			default:
				notifyErrorMessageForWebScan(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
						"blbackoffice.orderConsolidation.scan.many.location.error");
				break;
		}


	}

	/**
	 * @param barcodes
	 */
	private void checkSuccessForScan(final List<String> barcodes)
	{
		final List<String> failedBarcodeList = getBlInventoryScanToolService().getFailedBarcodeList(barcodes);
		if (CollectionUtils.isNotEmpty(failedBarcodeList))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.SCAN_BATCH_ERROR_FAILURE_MSG,
					failedBarcodeList);
			notifyErrorMessageForWebScan(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
					"blbackoffice.orderConsolidation.scan.batch.error");
		}
		else
		{
			Messagebox.show("Successfully records scanned!!");
		}
	}

	/**
	 * @param result
	 * @param barcodes
	 */
	private void createResponseMegForBINScan(final int result, final List<String> barcodes)
	{
		switch (result)
		{
			case BlInventoryScanLoggingConstants.ONE:
				Messagebox.show("Successfully records scanned!!");

				break;

			case BlInventoryScanLoggingConstants.TWO:
				notifyErrorMessageForWebScan(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
						"blbackoffice.orderConsolidation.scan.failed.bin.location.error");

				break;

			case BlInventoryScanLoggingConstants.THREE:
				notifyErrorMessageForWebScan(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
						"blbackoffice.orderConsolidation.scan.failed.parent.location.error");

				break;

			case BlInventoryScanLoggingConstants.FOUR:
				notifyErrorMessageForWebScan(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
						"blbackoffice.orderConsolidation.last.scan.invalid.error");

				break;

			default:
				notifyErrorMessageForWebScan(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
						"blbackoffice.orderConsolidation.max.limit.error");

				break;
		}


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
		throw new WrongValueException(this.textInput, this.getLabel(exceptionLabel));
	}

	private void notifyErrorMessageForWebScan(final String logMsg, final String exceptionLabel)
	{
		BlLogger.logMessage(LOG, Level.DEBUG, logMsg);
		throw new WrongValueException(this.scanningArea, this.getLabel(exceptionLabel));
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

	/**
	 * @return the consolidationData
	 */
	public Grid getConsolidationData()
	{
		return consolidationData;
	}


	/**
	 * @param consolidationData
	 *           the consolidationData to set
	 */
	public void setConsolidationData(final Grid consolidationData)
	{
		this.consolidationData = consolidationData;
	}

	public BlInventoryScanToolService getBlInventoryScanToolService()
	{
		return blInventoryScanToolService;
	}

	public void setBlInventoryScanToolService(final BlInventoryScanToolService blInventoryScanToolService)
	{
		this.blInventoryScanToolService = blInventoryScanToolService;
	}


	public NotificationService getNotificationService() {
		return notificationService;
	}
}
