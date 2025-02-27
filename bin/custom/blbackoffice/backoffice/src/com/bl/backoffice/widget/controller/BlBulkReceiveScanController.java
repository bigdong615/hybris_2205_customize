/**
 *
 */
package com.bl.backoffice.widget.controller;

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
import com.bl.core.dao.warehouse.BlConsignmentDao;
import com.bl.core.enums.ConditionRatingValueEnum;
import com.bl.core.enums.ItemStatusEnum;
import com.bl.core.enums.ItemTestingStatusEnum;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.inventory.scan.service.BlInventoryScanToolService;
import com.bl.core.model.BlProductModel;
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

	List<String> submittedSerials = new ArrayList<>();

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

	@Resource
	private BlConsignmentDao blConsignmentDao;

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

			final Collection<BlSerialProductModel> enteredSerialProductsModel = getBlInventoryScanToolService()
					.getSerialProductsByBarcode(bulkScanToolData.getBarcodeInputField());

			submittedSerials.clear();
			if (CollectionUtils.isNotEmpty(enteredSerialProductsModel))
			{
				for (final BlSerialProductModel blSerialProductModel : enteredSerialProductsModel)
				{
					// Pulling all consignment entries, which is having this serial
					final List<ConsignmentEntryModel> consEntry = blConsignmentDao
							.getConsignmentEntriesForSerialCode(blSerialProductModel);


					if (CollectionUtils.isNotEmpty(consEntry))
					{
						for (final ConsignmentEntryModel consignEntryModel : consEntry)

						{
							//We need to exclude RECEIVED_OR_RETURNED & NOT_INCLUDED
							if (consignEntryModel.getItems().get(blSerialProductModel.getCode()) != null ? !(consignEntryModel.getItems()
									.get(blSerialProductModel.getCode()).getCode().equals("RECEIVED_OR_RETURNED"))
									&& !(consignEntryModel.getItems().get(blSerialProductModel.getCode()).getCode().equals("NOT_INCLUDED"))
									: Boolean.FALSE)
							{

								consignEntryModel.getSerialProducts().forEach(serialProduct -> {
									//We need only serial product of main product & barcoded subparts
									if (serialProduct instanceof BlSerialProductModel
											&& serialProduct.getCode().equals(blSerialProductModel.getCode()))
									{
										BulkReceiveRespData serialMatchBulkReceiveRespData = new BulkReceiveRespData();



										final BlSerialProductModel serialProductModel = (BlSerialProductModel) serialProduct;
										final List<String> testingStatusValues = new ArrayList<String>();
										this.getEnumerationService().getEnumerationValues(ItemTestingStatusEnum.class)
												.forEach(testingStatus -> {
													testingStatusValues.add(testingStatus.getCode());
												});

										final List<String> cosmeticRatingValues = new ArrayList<String>();
										this.getEnumerationService().getEnumerationValues(ConditionRatingValueEnum.class)
												.forEach(cosmRating -> {
													cosmeticRatingValues.add(cosmRating.getCode());
												});

										final List<String> functionalRatingValues = new ArrayList<String>();
										this.getEnumerationService().getEnumerationValues(ConditionRatingValueEnum.class)
												.forEach(funRating -> {
													functionalRatingValues.add(funRating.getCode());
												});

										final BulkReceiveRespData bulkReceiveRespData = new BulkReceiveRespData();


										serialMatchBulkReceiveRespData = bulkReceiveRespDataList.stream()
												.filter(serialId -> serialId.getSerialProductId().equals(blSerialProductModel.getCode()))
												.findFirst().orElse(null);

										//Checking, if the selected serial, already saved, then need to disable
										if (serialMatchBulkReceiveRespData != null)
										{
											bulkReceiveRespData.setDisableFlag(true);
										}
										else
										{
											bulkReceiveRespData.setDisableFlag(false);
										}



										bulkReceiveRespData.setIsSubPart(Boolean.FALSE);
										bulkReceiveRespData.setSerialProductId(serialProductModel.getCode());
										bulkReceiveRespData.setSerialProductName(serialProductModel.getBlProduct().getName());
										bulkReceiveRespData.setProductType(serialProductModel.getProductType().getCode());
										bulkReceiveRespData.setMainProductId(serialProductModel.getBlProduct().getCode());
										bulkReceiveRespData.setBarcode(serialProductModel.getBarcode());
										bulkReceiveRespData.setSkuFirmwareVersion(serialProductModel.getSkuFirmwareVersion());
										bulkReceiveRespData.setFirmwareVersion(
												serialProductModel.getFirmwareVersion() != null ? serialProductModel.getFirmwareVersion()
														: StringUtils.EMPTY);
										bulkReceiveRespData.setCosmeticRatingValue(serialProductModel.getCosmeticRating().getCode());
										bulkReceiveRespData.setFunctionalRatingValue(serialProductModel.getFunctionalRating().getCode());
										bulkReceiveRespData.setTestingStatusValue(serialProductModel.getTestingStatus().getCode());

										bulkReceiveRespData.setCosmeticRating(new ListModelList<>(cosmeticRatingValues));
										bulkReceiveRespData.setTestingStatus(new ListModelList<>(testingStatusValues));
										bulkReceiveRespData.setFunctionalRating(new ListModelList<>(functionalRatingValues));
										bulkReceiveRespData.setConsignmentEntry("" + consignEntryModel.getPk());

										bulkReceiveRespData.setOrderNumber(consignEntryModel.getOrderEntry() != null
												? consignEntryModel.getOrderEntry().getOrder().getCode()
												: StringUtils.EMPTY);
										String orderNotesValue = "";
										final List<String> orderNotesData = new ArrayList<String>();
										if (consignEntryModel.getOrderEntry() != null)
										{
											consignEntryModel.getOrderEntry().getOrder().getOrderNotes().forEach(notes -> {
												orderNotesData.add(notes.getNote());
											});
										}

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

										bulkReceiveRespDataList.add(bulkReceiveRespData);

										// we need to skip, barcoded subparts
										if (CollectionUtils.isNotEmpty(serialProductModel.getBlProduct().getSubpartProducts())
												&& !serialProductModel.getProductType().getCode().equals("SUBPARTS"))
										{
											consignEntryModel.getSerialProducts().forEach(product -> {
												LOG.info("Loop1---->" + product.getCode());
												// Trying to pull only non barcoded subparts
												if ((CollectionUtils.isEmpty(product.getSerialProducts()))
														&& product.getProductType().getCode().equals("SUBPARTS")
														&& product.getNumberSystem().getCode().equals("NONE"))
												{
													LOG.info("Loop2---->" + product.getCode());
													if (CollectionUtils.isEmpty(product.getSerialProducts()))
													{
														LOG.info("Loop3---->" + product.getCode());
														final BulkReceiveRespData bulkSubpartReceiveRespData = new BulkReceiveRespData();
														bulkSubpartReceiveRespData.setIsSubPart(Boolean.TRUE);
														//To disable few options, for non barcoded subparts
														bulkSubpartReceiveRespData.setDisableFlag(true);
														bulkSubpartReceiveRespData.setSerialProductId(product.getCode());
														bulkSubpartReceiveRespData.setSerialProductName(product.getName());
														bulkSubpartReceiveRespData.setProductType(product.getProductType().getCode());
														bulkSubpartReceiveRespData
																.setMainProductId(serialProductModel.getBlProduct().getCode());
														bulkSubpartReceiveRespData.setBarcode("");
														bulkSubpartReceiveRespData.setConsignmentEntry("" + consignEntryModel.getPk());
														bulkSubpartReceiveRespData.setOrderNumber(consignEntryModel.getOrderEntry() != null
																? consignEntryModel.getOrderEntry().getOrder().getCode()
																: StringUtils.EMPTY);

														final BulkReceiveRespData duplicateBulkReceiveRespData = bulkReceiveRespDataList
																.stream()
																.filter(bulRespData -> (bulRespData.getSerialProductId()
																		.equals(bulkSubpartReceiveRespData.getSerialProductId()))
																		&& (bulRespData.getOrderNumber()
																				.equals(bulkSubpartReceiveRespData.getOrderNumber())))
																.findFirst().orElse(null);
														//if non barcoded subparts available already for this order, no need to add again
														if (null == duplicateBulkReceiveRespData)
														{
															/* Adding sub part product information */
															bulkReceiveRespDataList.add(bulkSubpartReceiveRespData);
														}
													}
												}
											});
										}
									}
								});
							}
						}

					}
				}

				if (bulkReceiveRespDataList.size() > 0)
				{
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
							BlInventoryScanLoggingConstants.NO_ORDER_FOR_SERIAL_SCAN_KEY);
				}
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

		final Map<String, BlSerialProductModel> blSerialList = new HashMap<String, BlSerialProductModel>();
		final List<BulkReceiveRespData> selectedSerials = new ArrayList<BulkReceiveRespData>();
		final Set<BlProductModel> blProductModels = new HashSet<BlProductModel>();

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
				final String serialProductCode = ((Label) row.getChildren().get(1)).getValue();
				final String serialProductName = ((Label) row.getChildren().get(2)).getValue();
				final String orderCode = ((Label) row.getChildren().get(3)).getValue();
				final String barCode = ((Label) row.getChildren().get(4)).getValue();
				final String firmwareVersion = ((Textbox) row.getChildren().get(5)).getValue();
				final String functionalRating = ((Combobox) row.getChildren().get(7)).getValue();
				final String cosmRating = ((Combobox) row.getChildren().get(8)).getValue();
				final String testingStatus = ((Combobox) row.getChildren().get(9)).getValue();
				final String consignmentEntryPK = ((Label) row.getChildren().get(11)).getValue();

				bulkReceiveData.setSerialProductId(serialProductCode);
				bulkReceiveData.setSerialProductName(serialProductName);
				bulkReceiveData.setOrderNumber(orderCode);
				bulkReceiveData.setBarcode(barCode);
				bulkReceiveData.setFunctionalRatingValue(functionalRating);
				bulkReceiveData.setTestingStatusValue(testingStatus);
				bulkReceiveData.setCosmeticRatingValue(cosmRating);
				bulkReceiveData.setFirmwareVersion(firmwareVersion);
				bulkReceiveData.setConsignmentEntry(consignmentEntryPK);
				//				final BlSerialProductModel blSerialProductModel = this.getDefaultBlProductDao().getSerialBySerialCode(bulkReceiveData.getSerialProductId());
				//
				//				if(Objects.nonNull(blSerialProductModel))
				//				{
				//					blSerialList.put(serialProductCode,blSerialProductModel);
				//					if(Objects.nonNull(blSerialProductModel.getBlProduct()) && !blProductModels.contains(blSerialProductModel.getBlProduct()))
				//					{
				//						createDataForNonBarcodedSubparts(selectedSerials, blSerialProductModel);
				//						blProductModels.add(blSerialProductModel.getBlProduct());
				//					}
				//				}

				//We will add, only serials, not the non barcoded subparts
				selectedSerials.add(bulkReceiveData);

				LOG.info("selected serial code --->" + bulkReceiveData.getSerialProductId());
			}
		}
		LOG.info("Total entries selected --->" + selectedSerials.size());
		//	updateSerialStatus(selectedSerials, blSerialList);
		updateSerialStatus(selectedSerials);

		if (!checkedEntries)
		{
			throw new WrongValueException(this.globalDeclineEntriesSelection,
					this.getLabel("warehousingbackoffice.reassignserial.decline.validation.missing.selectedLine"));
		}
		//this.sendOutput(OUT_CONFIRM, COMPLETE);
	}

	//	private void createDataForNonBarcodedSubparts(final List<BulkReceiveRespData> selectedSerials, final BlSerialProductModel blSerialProductModel)
	//	{
	//		for (final BlSubpartsModel blSubPartModel : blSerialProductModel.getBlProduct().getSubpartProducts())
	//		{
	//			if (CollectionUtils.isEmpty(blSubPartModel.getSubpartProduct().getSerialProducts()))
	//			{
	//				final BulkReceiveRespData bulkSubpartReceiveRespData = new BulkReceiveRespData();
	//				bulkSubpartReceiveRespData.setSerialProductId(blSubPartModel.getSubpartProduct().getCode());
	//				bulkSubpartReceiveRespData.setSerialProductName(blSubPartModel.getSubpartProduct().getName());
	//				bulkSubpartReceiveRespData.setProductType(blSubPartModel.getSubpartProduct().getProductType().getCode());
	//				bulkSubpartReceiveRespData.setMainProductId(blSerialProductModel.getBlProduct().getCode());
	//				bulkSubpartReceiveRespData.setBarcode(blSerialProductModel.getBarcode());
	//				bulkSubpartReceiveRespData.setOrderNumber(
	//						blSerialProductModel.getAssociatedShippedConsignment() != null
	//								? blSerialProductModel.getAssociatedShippedConsignment().getOrder().getCode()
	//								: StringUtils.EMPTY);
	//				bulkSubpartReceiveRespData.setFunctionalRatingValue(StringUtils.EMPTY);
	//				bulkSubpartReceiveRespData.setTestingStatusValue(StringUtils.EMPTY);
	//				bulkSubpartReceiveRespData.setCosmeticRatingValue(StringUtils.EMPTY);
	//				bulkSubpartReceiveRespData.setFirmwareVersion(StringUtils.EMPTY);
	//				bulkSubpartReceiveRespData.setIsSubPart(Boolean.TRUE);
	//				/* Adding sub part product information */
	//				selectedSerials.add(bulkSubpartReceiveRespData);
	//			}
	//		}
	//	}




	/**
	 * @param selectedSerials
	 *
	 */
	private void updateSerialStatus(final List<BulkReceiveRespData> selectedSerials)
	{

		final Map<String, List<BulkReceiveRespData>> groupedSerialsByConsignmentEntry = selectedSerials.stream()
				.collect(Collectors.groupingBy(BulkReceiveRespData::getConsignmentEntry));


		groupedSerialsByConsignmentEntry.forEach((consignmentEntry, groupedSerials) -> {

			if (CollectionUtils.isNotEmpty(groupedSerials))
			{
				for (final BulkReceiveRespData bulkRespDataSerial : groupedSerials)
				{
					//Updating status in serial model
					if (bulkRespDataSerial.getBarcode() != null && !bulkRespDataSerial.getBarcode().isEmpty())
					{
						final BlSerialProductModel blSerialProductModel = this.getDefaultBlProductDao()
								.getSerialBySerialCode(bulkRespDataSerial.getSerialProductId());

						final BlSerialProductModel serialModel = blSerialProductModel;

						if (null != serialModel && !submittedSerials.contains(serialModel.getCode()))
						{
							submittedSerials.add(serialModel.getCode());
							serialModel.setSerialStatus(SerialStatusEnum.RECEIVED_OR_RETURNED);
							serialModel.setHardAssigned(Boolean.FALSE);
							final ItemTestingStatusEnum testingStatusEnum = enumerationService
									.getEnumerationValue(ItemTestingStatusEnum.class, bulkRespDataSerial.getTestingStatusValue());
							final ConditionRatingValueEnum functionalRatingEnum = enumerationService
									.getEnumerationValue(ConditionRatingValueEnum.class, bulkRespDataSerial.getFunctionalRatingValue());
							final ConditionRatingValueEnum cosmeticRatingEnum = enumerationService
									.getEnumerationValue(ConditionRatingValueEnum.class, bulkRespDataSerial.getCosmeticRatingValue());
							serialModel.setFunctionalRating(functionalRatingEnum);
							serialModel.setTestingStatus(testingStatusEnum);
							serialModel.setCosmeticRating(cosmeticRatingEnum);
							serialModel.setFirmwareVersion(bulkRespDataSerial.getFirmwareVersion());
							getModelService().save(serialModel);

						}
					}
				}
			}

			//Updating consignment entry items
			final ConsignmentEntryModel consignEntry = blConsignmentDao.getConsignmentEntryByPk(consignmentEntry);
			if (consignEntry != null)
			{
				final Map<String, ItemStatusEnum> itemsMap = new HashMap<>(consignEntry.getItems());

				consignEntry.getSerialProducts().forEach(serialProduct -> {


					final BulkReceiveRespData blReceiveData = groupedSerials.stream()
							.filter(serial -> serial.getSerialProductId().equals(serialProduct.getCode())).findFirst().orElse(null);

					if (null != blReceiveData)
				{
						//Updating serial
						itemsMap.put(serialProduct.getCode(), ItemStatusEnum.RECEIVED_OR_RETURNED);


						//updating, non barcoded subparts
						if (serialProduct instanceof BlSerialProductModel)
						{

							final BlSerialProductModel blSerialModel = (BlSerialProductModel) serialProduct;
							if (!blSerialModel.getProductType().equals("SUBPARTS"))
						{
								for (final BlSubpartsModel blSubPartModel : blSerialModel.getBlProduct().getSubpartProducts())
							{
									if (CollectionUtils.isEmpty(blSubPartModel.getSubpartProduct().getSerialProducts()))
								{
										consignEntry.getItems().forEach((key, value) -> {

											if (key.contains(blSubPartModel.getSubpartProduct().getName()))
											{
												// We need to skip the non barcoded subparts, which status is missing
												if (!(value.equals("MISSING")))
												{
													itemsMap.put(key, ItemStatusEnum.RECEIVED_OR_RETURNED);
												}
											}

										});

									}
								}
							}
						}
					}

				});

				consignEntry.setItems(itemsMap);
				getModelService().save(consignEntry);
				getModelService().refresh(consignEntry);
			}


		});






		if (CollectionUtils.isNotEmpty(selectedSerials))
		{
			//			for (final BulkReceiveRespData bulkRespDataSerial : selectedSerials)
			//			{
			//				//Updating status in serial model
			//				if (bulkRespDataSerial.getBarcode() != null && !bulkRespDataSerial.getBarcode().isEmpty())
			//				{
			//					final BlSerialProductModel blSerialProductModel = this.getDefaultBlProductDao()
			//							.getSerialBySerialCode(bulkRespDataSerial.getSerialProductId());
			//
			//					final BlSerialProductModel serialModel = blSerialProductModel;
			//
			//					if (null != serialModel && !submittedSerials.contains(serialModel.getCode()))
			//					{
			//						submittedSerials.add(serialModel.getCode());
			//						serialModel.setSerialStatus(SerialStatusEnum.RECEIVED_OR_RETURNED);
			//						serialModel.setHardAssigned(Boolean.FALSE);
			//						final ItemTestingStatusEnum testingStatusEnum = enumerationService
			//								.getEnumerationValue(ItemTestingStatusEnum.class, bulkRespDataSerial.getTestingStatusValue());
			//						final ConditionRatingValueEnum functionalRatingEnum = enumerationService
			//								.getEnumerationValue(ConditionRatingValueEnum.class, bulkRespDataSerial.getFunctionalRatingValue());
			//						final ConditionRatingValueEnum cosmeticRatingEnum = enumerationService
			//								.getEnumerationValue(ConditionRatingValueEnum.class, bulkRespDataSerial.getCosmeticRatingValue());
			//						serialModel.setFunctionalRating(functionalRatingEnum);
			//						serialModel.setTestingStatus(testingStatusEnum);
			//						serialModel.setCosmeticRating(cosmeticRatingEnum);
			//						serialModel.setFirmwareVersion(bulkRespDataSerial.getFirmwareVersion());
			//						getModelService().save(serialModel);
			//
			//					}
			//				}
			//
			//				//Updating consignment entry items
			//				final ConsignmentEntryModel consignEntry = blConsignmentDao
			//						.getConsignmentEntryByPk(bulkRespDataSerial.getConsignmentEntry());
			//				if (consignEntry != null)
			//				{
			//					final Map<String, ItemStatusEnum> itemsMap = new HashMap<>(consignEntry.getItems());
			//
			//					consignEntry.getSerialProducts().forEach(serialProduct -> {
			//
			//						if (serialProduct.getCode().equals(bulkRespDataSerial.getSerialProductId()))
//						{
			//							//Updating serial
			//							itemsMap.put(bulkRespDataSerial.getSerialProductId(), ItemStatusEnum.RECEIVED_OR_RETURNED);
			//
			//
			//							//updating, non barcoded subparts
			//							if (serialProduct instanceof BlSerialProductModel)
//							{
			//
			//								final BlSerialProductModel blSerialModel = (BlSerialProductModel) serialProduct;
			//								if (!blSerialModel.getProductType().equals("SUBPARTS"))
			//								{
			//									for (final BlSubpartsModel blSubPartModel : blSerialModel.getBlProduct().getSubpartProducts())
			//									{
			//										if (CollectionUtils.isEmpty(blSubPartModel.getSubpartProduct().getSerialProducts()))
			//										{
			//											consignEntry.getItems().forEach((key, value) -> {
			//
			//												if (key.contains(blSubPartModel.getSubpartProduct().getName()))
			//													{
			//													// We need to skip the non barcoded subparts, which status is missing
			//													if (!(value.equals("MISSING")))
			//														{
			//														itemsMap.put(key, ItemStatusEnum.RECEIVED_OR_RETURNED);
			//														}
			//													}
			//
			//												});
			//
			//											}
			//										}
			//									}
			//								}
			//
//							}
			////						else if ((CollectionUtils.isEmpty(product.getSerialProducts()))
			////								&& product.getProductType().getCode().equals("SUBPARTS")
			////								&& product.getNumberSystem().getCode().equals("NONE"))
			////						{
			////							// We need to skip the subparts, which status is missing
			////							if (itemsMap.get(product.getName()) != null ? !itemsMap.get(product.getName()).equals("MISSING")
			////									: Boolean.TRUE)
			////							{
			////							itemsMap.put(product.getName(), ItemStatusEnum.RECEIVED_OR_RETURNED);
			////							}
			////						}
			//
			//
			//					});
			//
			//					consignEntry.setItems(itemsMap);
			//					getModelService().save(consignEntry);
			//					getModelService().refresh(consignEntry);
			//				}
			//
			//			}

			this.productsListDiv.setStyle("resize:none;display:none");
			this.barcodesSectionId.setStyle("resize:none;display:block");
			this.getWidgetInstanceManager()
					.setTitle(String.valueOf(this.getWidgetInstanceManager().getLabel("blbackoffice.bulk.scan.heading")));
			bulkScanToolData = new BulkReceiveScanToolData();
			scanningArea.setValue("");
			this.globalDeclineEntriesSelection.setChecked(false);

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
			if (firstComponent instanceof Checkbox && !((Checkbox) firstComponent).isDisabled())
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
