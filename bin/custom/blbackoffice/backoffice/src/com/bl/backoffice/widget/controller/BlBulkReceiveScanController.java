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
			final boolean noOrderExistFlag = false;
			final List<BulkReceiveRespData> bulkReceiveRespDataList = new ArrayList<BulkReceiveRespData>();

			final Collection<BlSerialProductModel> serialProducts = getBlInventoryScanToolService()
					.getSerialProductsByBarcode(bulkScanToolData.getBarcodeInputField());

			submittedSerials.clear();
			if (CollectionUtils.isNotEmpty(serialProducts))
			{
				for (final BlSerialProductModel blSerialProductModel : serialProducts)
				{
					// Pulling all consignment entries, which is having this serial
					final List<ConsignmentEntryModel> consEntry = blConsignmentDao
							.getConsignmentEntriesForSerialCode(blSerialProductModel);
					if (CollectionUtils.isNotEmpty(consEntry))
					{
						for (final ConsignmentEntryModel consignEntryModel : consEntry)

						{
							//We need to exclude RECEIVED_OR_RETURNED & NOT_INCLUDED
							if (consignEntryModel.getItems().get(blSerialProductModel.getCode()) != null
									? !(consignEntryModel.getItems().get(blSerialProductModel.getCode()).getCode()
											.equals("RECEIVED_OR_RETURNEDl"))
											&& !(consignEntryModel.getItems().get(blSerialProductModel.getCode()).getCode()
													.equals("NOT_INCLUDED"))
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

										bulkReceiveRespData.setOrderNumber(consignEntryModel.getOrderEntry().getOrder() != null
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
												&& !serialProductModel.getProductType().equals("SUBPARTS"))
										{
											consignEntryModel.getSerialProducts().forEach(product -> {
												LOG.info("Loop1---->" + product.getCode());
												// Trying to pull only non barcoded subparts
												//												if (!(product instanceof BlSerialProductModel)
												//														&& !(product.getCode().equals(serialProductModel.getCode()))
												//														&& product.getProductType().equals("SUBPARTS"))
												if ((CollectionUtils.isEmpty(product.getSerialProducts()))
														&& product.getProductType().equals("SUBPARTS")
														&& product.getNumberSystem().equals("NONE"))
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
														bulkSubpartReceiveRespData
																.setOrderNumber(consignEntryModel.getOrderEntry().getOrder() != null
																		? consignEntryModel.getOrderEntry().getOrder().getCode()
																		: StringUtils.EMPTY);

														/* Adding sub part product information */
														bulkReceiveRespDataList.add(bulkSubpartReceiveRespData);
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
				selectedSerials.add(bulkReceiveData);
			}
		}
		//	updateSerialStatus(selectedSerials, blSerialList);
		updateSerialStatusNew(selectedSerials);

		if (!checkedEntries)
		{
			throw new WrongValueException(this.globalDeclineEntriesSelection,
					this.getLabel("warehousingbackoffice.reassignserial.decline.validation.missing.selectedLine"));
		}
		this.sendOutput(OUT_CONFIRM, COMPLETE);
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
	//		private void updateSerialStatus(final List<BulkReceiveRespData> selectedSerials, final Map<String, BlSerialProductModel> blSerialList)
	//		{
	//
	//			if (CollectionUtils.isNotEmpty(selectedSerials))
	//			{
	//				Map<String, List<BulkReceiveRespData>> selectedRespBasedOnBarCode = new HashMap<>();
	//
	//				selectedRespBasedOnBarCode = selectedSerials.stream().collect(Collectors.groupingBy(BulkReceiveRespData::getBarcode));
	//
	//
	//				for (final Entry<String, List<BulkReceiveRespData>> dataBasedOnBarcode : selectedRespBasedOnBarCode.entrySet())
	//				{
	//
	//					for (final BulkReceiveRespData bulkRespData : dataBasedOnBarcode.getValue())
	//					{
	//						//Skipping subparts, only serial products need to update
	//						if (bulkRespData != null && bulkRespData.getTestingStatusValue() != null
	//								&& !bulkRespData.getTestingStatusValue().isEmpty())
	//						{
	//							final BlSerialProductModel serialModel = blSerialList.get(bulkRespData.getSerialProductId());
	//							/*
	//							 * this.getDefaultBlProductDao() .getSerialBySerialCode(bulkRespData.getSerialProductId());
	//							 * //.getSerialByBarcode(bulkRespData.getBarcode());
	//							 */
	//
	//							if (null != serialModel)
	//							{
	//								serialModel.setSerialStatus(SerialStatusEnum.RECEIVED_OR_RETURNED);
	//								serialModel.setHardAssigned(Boolean.FALSE);
	//								final ItemTestingStatusEnum testingStatusEnum = enumerationService
	//										.getEnumerationValue(ItemTestingStatusEnum.class, bulkRespData.getTestingStatusValue());
	//								final ConditionRatingValueEnum functionalRatingEnum = enumerationService
	//										.getEnumerationValue(ConditionRatingValueEnum.class, bulkRespData.getFunctionalRatingValue());
	//								final ConditionRatingValueEnum cosmeticRatingEnum = enumerationService
	//										.getEnumerationValue(ConditionRatingValueEnum.class, bulkRespData.getCosmeticRatingValue());
	//								serialModel.setFunctionalRating(functionalRatingEnum);
	//								serialModel.setTestingStatus(testingStatusEnum);
	//								serialModel.setCosmeticRating(cosmeticRatingEnum);
	//								serialModel.setFirmwareVersion(bulkRespData.getFirmwareVersion());
	//								getModelService().save(serialModel);
	//
	//								Map<String, ItemStatusEnum> itemsMap;
	//
	//								// Taking the consignments, which are older & new, belongs to serial
	//								final List<ConsignmentEntryModel> consEntry = blConsignmentDao
	//										.getConsignmentEntriesForSerialCode(serialModel);
	//								if (CollectionUtils.isNotEmpty(consEntry))
	//								{
	//									for (final ConsignmentEntryModel consignEntryModel : consEntry)
	//
	//									{
	//
	//										itemsMap = new HashMap<>(consignEntryModel.getItems());
	//
	//										if (itemsMap.get(serialModel.getCode()) != null)
	//										{
	//											for (final Map.Entry<String, ItemStatusEnum> entry : itemsMap.entrySet())
	//											{
	//												//Checking Items, productID for serialProduct, productName for subParts, to update only selected items
	//												final BulkReceiveRespData consignmentEntryItems = dataBasedOnBarcode.getValue().stream()
	//														.filter(data -> (!data.getTestingStatusValue().isEmpty()
	//																? data.getSerialProductId().equals(entry.getKey())
	//																: data.getSerialProductName().equals(entry.getKey().split("--")[0])))
	//														.findFirst().orElse(null);
	//												if (consignmentEntryItems != null)
	//												{
	//													itemsMap.put(entry.getKey(), ItemStatusEnum.RECEIVED_OR_RETURNED);
	//												}
	//											}
	//										}
	//
	//										consignEntryModel.setItems(itemsMap);
	//										getModelService().save(consignEntryModel);
	//										getModelService().refresh(consignEntryModel);
	//									}
	//								}
	//							}
	//						}
	//					}
	//				}
	//
	//				Messagebox.show(BlInventoryScanLoggingConstants.BULK_SCAN_TOOL_SUCCESS_MSG);
	//			}
	//
	//			else
	//			{
	//				throw new WrongValueException(this.globalDeclineEntriesSelection,
	//						BlInventoryScanLoggingConstants.BULK_SCAN_TOOL_PLS_SELECT_MSG);
	//			}
	//
	//		}



	/**
	 * @param selectedSerials
	 *
	 */
	private void updateSerialStatusNew(final List<BulkReceiveRespData> selectedSerials)
			{

		if (CollectionUtils.isNotEmpty(selectedSerials))
				{
			for (final BulkReceiveRespData bulkRespData : selectedSerials)
						{
							if (bulkRespData.getBarcode() != null && !bulkRespData.getBarcode().isEmpty())
				{
					final BlSerialProductModel blSerialProductModel = this.getDefaultBlProductDao()
							.getSerialBySerialCode(bulkRespData.getSerialProductId());

					final BlSerialProductModel serialModel = blSerialProductModel;

					if (null != serialModel && !submittedSerials.contains(serialModel.getCode()))
								{
						submittedSerials.add(serialModel.getCode());
						serialModel.setSerialStatus(SerialStatusEnum.RECEIVED_OR_RETURNED);
						serialModel.setHardAssigned(Boolean.FALSE);
						final ItemTestingStatusEnum testingStatusEnum = enumerationService
								.getEnumerationValue(ItemTestingStatusEnum.class, bulkRespData.getTestingStatusValue());
						final ConditionRatingValueEnum functionalRatingEnum = enumerationService
								.getEnumerationValue(ConditionRatingValueEnum.class, bulkRespData.getFunctionalRatingValue());
						final ConditionRatingValueEnum cosmeticRatingEnum = enumerationService
								.getEnumerationValue(ConditionRatingValueEnum.class, bulkRespData.getCosmeticRatingValue());
						serialModel.setFunctionalRating(functionalRatingEnum);
						serialModel.setTestingStatus(testingStatusEnum);
						serialModel.setCosmeticRating(cosmeticRatingEnum);
						serialModel.setFirmwareVersion(bulkRespData.getFirmwareVersion());
						getModelService().save(serialModel);



						// Taking the consignments, which are older & new, belongs to serial
						//									final List<ConsignmentEntryModel> consEntry = blConsignmentDao
						//											.getConsignmentEntriesForSerialCode(serialModel)
						//									if (CollectionUtils.isNotEmpty(consEntry))
						//									{
						//										for (final ConsignmentEntryModel consignEntryModel : consEntry)
						//
						//										{
						//
						//											itemsMap = new HashMap<>(consignEntryModel.getItems());
						//
						//											if (itemsMap.get(serialModel.getCode()) != null)
						//											{
						//												for (final Map.Entry<String, ItemStatusEnum> entry : itemsMap.entrySet())
						//												{
						//													//Checking Items, productID for serialProduct, productName for subParts, to update only selected items
						//													final BulkReceiveRespData consignmentEntryItems = dataBasedOnBarcode.getValue().stream()
						//															.filter(data -> (!data.getTestingStatusValue().isEmpty()
						//																	? data.getSerialProductId().equals(entry.getKey())
						//																	: data.getSerialProductName().equals(entry.getKey().split("--")[0])))
						//															.findFirst().orElse(null);
						//													if (consignmentEntryItems != null)
						//													{
						//														itemsMap.put(entry.getKey(), ItemStatusEnum.RECEIVED_OR_RETURNED);
						//													}
						//												}
						//											}
						//
						//											consignEntryModel.setItems(itemsMap);
						//											getModelService().save(consignEntryModel);
						//											getModelService().refresh(consignEntryModel);
						//										}
						//									}
								}

							}

							final ConsignmentEntryModel consignEntry = blConsignmentDao
									.getConsignmentEntryByPk(bulkRespData.getConsignmentEntry());
							if (consignEntry != null)
							{
								final Map<String, ItemStatusEnum> itemsMap = new HashMap<>(consignEntry.getItems());

								itemsMap.put(bulkRespData.getSerialProductId(), ItemStatusEnum.RECEIVED_OR_RETURNED);
								consignEntry.setItems(itemsMap);
								getModelService().save(consignEntry);
								getModelService().refresh(consignEntry);
							}

						}
						//}

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
