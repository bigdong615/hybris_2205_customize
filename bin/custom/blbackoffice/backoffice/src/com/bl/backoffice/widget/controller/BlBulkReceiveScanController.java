/**
 *
 */
package com.bl.backoffice.widget.controller;

import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.enumeration.EnumerationService;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.annotation.Resource;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.Component;
import org.zkoss.zk.ui.WrongValueException;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zk.ui.select.annotation.WireVariable;
import org.zkoss.zul.Checkbox;
import org.zkoss.zul.Combobox;
import org.zkoss.zul.Div;
import org.zkoss.zul.Grid;
import org.zkoss.zul.Label;
import org.zkoss.zul.ListModelList;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Textbox;

import com.bl.backoffice.wizards.util.BulkReceiveRespData;
import com.bl.backoffice.wizards.util.BulkReceiveScanToolData;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.enums.ConditionRatingValueEnum;
import com.bl.core.enums.ItemStatusEnum;
import com.bl.core.enums.ItemTestingStatusEnum;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.inventory.scan.service.BlInventoryScanToolService;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.BlSubpartsModel;
import com.bl.core.product.dao.impl.DefaultBlProductDao;
import com.bl.facades.order.BlOrderFacade;
import com.bl.logging.BlLogger;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;

/**
 * @author ravinder
 *
 */
public class BlBulkReceiveScanController extends DefaultWidgetController
{
	//	protected static final String IN_SOCKET = "bulkReceiveInput";
	protected static final String IN_SOCKET = "nodeSelected";

	protected static final String OUT_CONFIRM = "bulkReceiveOutput";
	protected static final String COMPLETE = "completed";
	protected static final String ON_SELECT = "onSelect";

	private static final Logger LOG = Logger.getLogger(BlBulkReceiveScanController.class);

	@Resource(name = "blInventoryScanToolService")
	private BlInventoryScanToolService blInventoryScanToolService;

	@Wire
	private Grid productEntries;

	@Wire
	private Div productsListDiv;

	@Wire
	private Div barcodesSectionId;

	@WireVariable
	private transient EnumerationService enumerationService;

	@Wire
	private Checkbox globalDeclineEntriesSelection;

	@Resource
	private DefaultBlProductDao defaultBlProductDao;

	@Wire
	Textbox scanningArea;

	@Resource(name = "blOrderFacade")
	private BlOrderFacade blOrderFacade;

	private transient BulkReceiveScanToolData bulkScanToolData;

	@WireVariable
	private transient ModelService modelService;

	Object selectedConsignment = new Object();

	/**
	 * This method is used to load the default values at the time of opening the popup
	 *
	 * @param inputObject
	 */

	@SocketEvent(socketId = IN_SOCKET)
	public void initLoadPage(final Object inputObject)
	{
		selectedConsignment = inputObject;
		this.getWidgetInstanceManager()
				.setTitle(String.valueOf(this.getWidgetInstanceManager().getLabel("blbackoffice.bulk.scan.heading")));
		bulkScanToolData = new BulkReceiveScanToolData();
		this.productsListDiv.setStyle("resize:none;display:none");
		this.addListeners();
	}

	/**
	 * This method is used to close the Popup
	 */
	@ViewEvent(componentID = BlInventoryScanLoggingConstants.CANCEL_EVENT, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void cancel()
	{
		this.sendOutput(OUT_CONFIRM, COMPLETE);
	}

	/**
	 * This method is used to close the Popup
	 */
	@ViewEvent(componentID = BlInventoryScanLoggingConstants.CANCEL_BTN_EVENT, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void cancelBtn()
	{
		this.sendOutput(OUT_CONFIRM, COMPLETE);
	}

	/**
	 * This method is used to scan bar codes
	 */
	@ViewEvent(componentID = BlInventoryScanLoggingConstants.SCAN_BAR_CODES, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void scanBarCodes()
	{
		bulkScanToolData
				.setBarcodeInputField(Arrays.asList(scanningArea.getValue().split(BlInventoryScanLoggingConstants.NEW_LINE)));
		if (CollectionUtils.isEmpty(bulkScanToolData.getBarcodeInputField()))
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
					BlInventoryScanLoggingConstants.NO_ITEM_SCAN_KEY);
		}
		else
		{
			final List<BulkReceiveRespData> bulkReceiveRespDataList = new ArrayList<BulkReceiveRespData>();

			final Collection<BlSerialProductModel> serialProducts = getBlInventoryScanToolService()
					.getSerialProductsByBarcode(bulkScanToolData.getBarcodeInputField());

			if (CollectionUtils.isNotEmpty(serialProducts))
			{

			for (final BlSerialProductModel blSerialProductModel : serialProducts)
			{
				final List<String> testingStatusValues = new ArrayList<String>();
				this.getEnumerationService().getEnumerationValues(ItemTestingStatusEnum.class).forEach(testingStatus -> {
					testingStatusValues.add(testingStatus.getCode());
				});

				final List<String> cosmeticRatingValues = new ArrayList<String>();
				this.getEnumerationService().getEnumerationValues(ConditionRatingValueEnum.class).forEach(cosmRating -> {
					cosmeticRatingValues.add(cosmRating.getCode());
				});

				final List<String> functionalRatingValues = new ArrayList<String>();
				this.getEnumerationService().getEnumerationValues(ConditionRatingValueEnum.class).forEach(funRating -> {
					functionalRatingValues.add(funRating.getCode());
				});

				final BulkReceiveRespData bulkReceiveRespData = new BulkReceiveRespData();
				bulkReceiveRespData.setSerialProductId(blSerialProductModel.getCode());
				bulkReceiveRespData.setSerialProductName(blSerialProductModel.getBlProduct().getName());
				bulkReceiveRespData.setProductType(blSerialProductModel.getProductType().getCode());
				bulkReceiveRespData.setMainProductId(blSerialProductModel.getBlProduct().getCode());
				bulkReceiveRespData.setBarcode(blSerialProductModel.getBarcode());
				bulkReceiveRespData.setSkuFirmwareVersion(blSerialProductModel.getSkuFirmwareVersion());
				bulkReceiveRespData.setFirmwareVersion(blSerialProductModel.getBlProduct().getFirmwareVersion());
				bulkReceiveRespData.setCosmeticRatingValue(blSerialProductModel.getCosmeticRating().getCode());
				bulkReceiveRespData.setFunctionalRatingValue(blSerialProductModel.getFunctionalRating().getCode());
				bulkReceiveRespData.setTestingStatusValue(blSerialProductModel.getTestingStatus().getCode());

				bulkReceiveRespData.setCosmeticRating(new ListModelList<>(cosmeticRatingValues));
				bulkReceiveRespData.setTestingStatus(new ListModelList<>(testingStatusValues));
				bulkReceiveRespData.setFunctionalRating(new ListModelList<>(functionalRatingValues));

				bulkReceiveRespData.setOrderNumber(
						blSerialProductModel.getAssociatedOrder() != null ? blSerialProductModel.getAssociatedOrder().getCode()
								: StringUtils.EMPTY);
				String orderNotesValue = "";
				final List<String> orderNotesData = new ArrayList<String>();
				blSerialProductModel.getAssociatedOrder().getOrderNotes().forEach(notes -> {
					orderNotesData.add(notes.getNote());
				});

				for (final String orderNote : orderNotesData)
				{
					if (StringUtils.isEmpty(orderNotesValue))
					{
						orderNotesValue = orderNote;
					}
					else
					{
						orderNotesValue = orderNotesValue + ", " + orderNote;
					}
				}
				bulkReceiveRespData.setOrderNotes(orderNotesValue);

				/* Adding serial product information */
				bulkReceiveRespDataList.add(bulkReceiveRespData);

				if (CollectionUtils.isNotEmpty(blSerialProductModel.getBlProduct().getSubpartProducts()))
				{
					for (final BlSubpartsModel blSubPartModel : blSerialProductModel.getBlProduct().getSubpartProducts())
					{
						final BulkReceiveRespData bulkSubpartReceiveRespData = new BulkReceiveRespData();
						bulkSubpartReceiveRespData.setSerialProductId(blSubPartModel.getSubpartProduct().getCode());
						bulkSubpartReceiveRespData.setSerialProductName(blSubPartModel.getSubpartProduct().getName());
						bulkSubpartReceiveRespData.setProductType(blSubPartModel.getSubpartProduct().getProductType().getCode());
						bulkSubpartReceiveRespData.setMainProductId(blSerialProductModel.getBlProduct().getCode());
						bulkSubpartReceiveRespData.setBarcode(blSerialProductModel.getBarcode());
						bulkSubpartReceiveRespData.setFirmwareVersion(blSerialProductModel.getBlProduct().getFirmwareVersion());
						bulkSubpartReceiveRespData.setOrderNumber(
								blSerialProductModel.getAssociatedOrder() != null ? blSerialProductModel.getAssociatedOrder().getCode()
										: StringUtils.EMPTY);
						bulkSubpartReceiveRespData.setOrderNotes(orderNotesValue);

						/* Adding sub part product information */
						bulkReceiveRespDataList.add(bulkSubpartReceiveRespData);
					}
				}
			}

			this.productsListDiv.setStyle("resize:none;display:block");
			this.barcodesSectionId.setStyle("resize:none;display:none");

			this.getWidgetInstanceManager()
					.setTitle(String.valueOf(this.getWidgetInstanceManager().getLabel("blbackoffice.bulk.scan.heading")));

			this.getProductEntries().setModel(new ListModelList<>(bulkReceiveRespDataList));

			this.getProductEntries().renderAll();
		}
		else
		{
			notifyErrorMessage(BlInventoryScanLoggingConstants.WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG,
					BlInventoryScanLoggingConstants.NO_ITEM_SCAN_KEY);
		}
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
		throw new WrongValueException(this.scanningArea, this.getLabel(exceptionLabel));
	}


	@ViewEvent(componentID = "confirmchangestatus", eventName = "onClick")
	public void confirmChangeStatus() throws InterruptedException
	{

		final List<BulkReceiveRespData> selectedSerials = new ArrayList<BulkReceiveRespData>();
		boolean checkedEntries = false;
		if (this.globalDeclineEntriesSelection.isChecked())
		{
			this.selectAllEntries();
		}
		final Iterator<Component> var2 = this.getProductEntries().getRows().getChildren().iterator();
		while (var2.hasNext())
		{
			final Component row = var2.next();
			final Component firstComponent = row.getChildren().iterator().next();
			if (firstComponent instanceof Checkbox && ((Checkbox) firstComponent).isChecked())
			{
				final BulkReceiveRespData bulkReceiveData = new BulkReceiveRespData();
				checkedEntries = true;
				final String productCode = ((Label) row.getChildren().get(1)).getValue();
				final String orderCode = ((Label) row.getChildren().get(3)).getValue();
				final String barCode = ((Label) row.getChildren().get(4)).getValue();
				//	final String skuFirmwareVersion = ((Label) row.getChildren().get(6)).getValue();
				final String functionalRating = ((Combobox) row.getChildren().get(7)).getValue();
				final String cosmRating = ((Combobox) row.getChildren().get(8)).getValue();
				final String testingStatus = ((Combobox) row.getChildren().get(9)).getValue();

				bulkReceiveData.setSerialProductId(productCode);
				bulkReceiveData.setOrderNumber(orderCode);
				bulkReceiveData.setBarcode(barCode);
				bulkReceiveData.setFunctionalRatingValue(functionalRating);
				bulkReceiveData.setTestingStatusValue(testingStatus);
				bulkReceiveData.setCosmeticRatingValue(cosmRating);
				//bulkReceiveData.setSkuFirmwareVersion(skuFirmwareVersion);

				selectedSerials.add(bulkReceiveData);
			}
		}
		updateSerialStatus(selectedSerials);

		if (!checkedEntries)
		{
			throw new WrongValueException(this.globalDeclineEntriesSelection,
					this.getLabel("warehousingbackoffice.reassignserial.decline.validation.missing.selectedLine"));
		}
		this.sendOutput(OUT_CONFIRM, COMPLETE);
	}

	/**
	 * @param selectedSerials
	 *
	 */
	private void updateSerialStatus(final List<BulkReceiveRespData> selectedSerials)
	{

		if (CollectionUtils.isNotEmpty(selectedSerials))
		{
		Map<String, List<BulkReceiveRespData>> orderFilteredResp = new HashMap<>();

		orderFilteredResp = selectedSerials.stream().collect(Collectors.groupingBy(BulkReceiveRespData::getOrderNumber));

		Set<String> orderIds = new HashSet<String>();
		orderIds = orderFilteredResp != null && CollectionUtils.isNotEmpty(orderFilteredResp.keySet())
				? orderFilteredResp.keySet()
				: new HashSet<String>();

		final boolean entered = false;
		for (final String orderCode : orderIds)
		{
			final List<BulkReceiveRespData> orderData = orderFilteredResp.get(orderCode);

			final BlSerialProductModel serialModel = this.getDefaultBlProductDao().getSerialByBarcode(orderData.get(0).getBarcode());

			final OrderModel order = serialModel.getAssociatedOrder();

			if (null != order)
			{
			order.getConsignments().forEach(consignment -> {
				for (final ConsignmentEntryModel consignEntryModel : consignment.getConsignmentEntries())
				{
					final Map<String, ItemStatusEnum> itemsMap = new HashMap<>(consignEntryModel.getItems());
					final List<BlSerialProductModel> blSerialsModel = new ArrayList<BlSerialProductModel>();
					for (final Map.Entry<String, ItemStatusEnum> entry : itemsMap.entrySet())
					{
						final BlSerialProductModel serial = this.getDefaultBlProductDao().getSerialBySerialCode(entry.getKey());

						if (serial != null)
						{
							blSerialsModel.add(serial);

							itemsMap.put(entry.getKey(), ItemStatusEnum.RECEIVED_OR_RETURNED);
						}
					}

					for (final BlSerialProductModel productModel : blSerialsModel)
					{

						final BulkReceiveRespData	bulkResData = selectedSerials
								 .stream()
								.filter(bulkData -> bulkData.getSerialProductId().equals(productModel.getCode()))
										.findFirst()
						            .orElse(null);


						productModel.setSerialStatus(SerialStatusEnum.RECEIVED_OR_RETURNED);
						productModel.setHardAssigned(Boolean.FALSE);
						final ItemTestingStatusEnum testingStatusEnum = enumerationService
								.getEnumerationValue(ItemTestingStatusEnum.class, bulkResData.getTestingStatusValue());
						final ConditionRatingValueEnum functionalRatingEnum = enumerationService
								.getEnumerationValue(ConditionRatingValueEnum.class, bulkResData.getFunctionalRatingValue());
						final ConditionRatingValueEnum cosmeticRatingEnum = enumerationService
								.getEnumerationValue(ConditionRatingValueEnum.class, bulkResData.getCosmeticRatingValue());
						productModel.setFunctionalRating(functionalRatingEnum);
						productModel.setTestingStatus(testingStatusEnum);
						productModel.setCosmeticRating(cosmeticRatingEnum);
						getModelService().save(productModel);
					}

					consignEntryModel.setItems(itemsMap);
					getModelService().save(consignEntryModel);
					getModelService().refresh(consignEntryModel);
				}
			});
		}

		}

		Messagebox.show(BlInventoryScanLoggingConstants.BULK_SCAN_TOOL_SUCCESS_MSG);

	}
	else
	{
		throw new WrongValueException(this.globalDeclineEntriesSelection,
				BlInventoryScanLoggingConstants.BULK_SCAN_TOOL_PLS_SELECT_MSG);
	}

	}

	protected void selectAllEntries()
	{
		final Iterator<Component> var2 = this.productEntries.getRows().getChildren().iterator();
		while (var2.hasNext())
		{
			final Component row = var2.next();
			final Component firstComponent = row.getChildren().iterator().next();
			if (firstComponent instanceof Checkbox)
			{
				((Checkbox) firstComponent).setChecked(this.globalDeclineEntriesSelection.isChecked());
			}
		}
	}


	protected void addListeners()
	{
		final List<Component> rows = this.getProductEntries().getRows().getChildren();
		final Iterator<Component> var3 = rows.iterator();

		while (var3.hasNext())
		{
			final Component row = var3.next();
			final Iterator<Component> var5 = row.getChildren().iterator();

			while (var5.hasNext())
			{
				final Component myComponent = var5.next();
				if (myComponent instanceof Checkbox)
				{
					//myComponent.addEventListener("onCheck", event -> this.handleRow((Row) event.getTarget().getParent()));
				}
			}
		}

		this.globalDeclineEntriesSelection.addEventListener("onCheck", event -> this.selectAllEntries());
	}


	/**
	 * @return the modelService
	 */
	public ModelService getModelService()
	{
		return modelService;
	}

	/**
	 * @param modelService
	 *           the modelService to set
	 */
	public void setModelService(final ModelService modelService)
	{
		this.modelService = modelService;
	}

	/**
	 * @return the productEntries
	 */
	public Grid getProductEntries()
	{
		return productEntries;
	}

	/**
	 * @param productEntries
	 *           the productEntries to set
	 */
	public void setProductEntries(final Grid productEntries)
	{
		this.productEntries = productEntries;
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
	 * @return the enumerationService
	 */
	public EnumerationService getEnumerationService()
	{
		return enumerationService;
	}

	/**
	 * @param enumerationService
	 *           the enumerationService to set
	 */
	public void setEnumerationService(final EnumerationService enumerationService)
	{
		this.enumerationService = enumerationService;
	}

	/**
	 * @return the defaultBlProductDao
	 */
	public DefaultBlProductDao getDefaultBlProductDao()
	{
		return defaultBlProductDao;
	}

	/**
	 * @param defaultBlProductDao
	 *           the defaultBlProductDao to set
	 */
	public void setDefaultBlProductDao(final DefaultBlProductDao defaultBlProductDao)
	{
		this.defaultBlProductDao = defaultBlProductDao;
	}
}
