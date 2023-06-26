package com.bl.backoffice.widget.controller;

import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.enums.BlackoutDateTypeEnum;
import com.bl.core.enums.CarrierEnum;
import com.bl.core.model.OptimizedShippingMethodModel;
import com.bl.core.model.ShippingOptimizationModel;
import com.bl.core.shipping.service.BlDeliveryModeService;
import com.bl.core.utils.BlDateTimeUtils;
import com.google.common.collect.Maps;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.internal.dao.GenericDao;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.io.IOException;
import java.util.*;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import javax.annotation.Resource;

import org.apache.commons.collections.map.HashedMap;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zul.Combobox;
import org.zkoss.zul.ListModelList;
import org.zkoss.zul.Messagebox;

import com.bl.Ordermanagement.filters.BlDeliveryStateSourcingLocationFilter;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.integration.constants.BlintegrationConstants;
import com.bl.integration.facades.BlCreateShipmentFacade;
import com.bl.integration.services.impl.DefaultBLShipmentCreationService;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;


/**
 * This class is responsible to create a return shipment package
 *
 * @author Aditi Sharma
 */

public class BlCreateReturnShipmentController extends DefaultWidgetController
{
	@Wire
	private Combobox warehouseCombobox;
	@Wire
	private Combobox shippingTypeComboBox;
	@Wire
	private Combobox optimizedShippingMethodComboBox;

	protected static final String OUT_CONFIRM = "confirmOutput";
	protected static final String COMPLETE = "completed";

	@Resource(name = "blCreateShipmentFacade")
	private BlCreateShipmentFacade blCreateShipmentFacade;

	@Resource(name = "blWarehouseGenericDao")
	private GenericDao<WarehouseModel> blWarehouseGenericDao;

	@Resource(name = "blDeliveryStateSourcingLocationFilter")
	private BlDeliveryStateSourcingLocationFilter blDeliveryStateSourcingLocationFilter;
	
	@Resource(name = "blShipmentCreationService")
	private DefaultBLShipmentCreationService blShipmentCreationService;

	@Resource(name = "modelService")
	private ModelService modelService;

	@Resource(name = "zoneDeliveryModeService")
	private BlDeliveryModeService zoneDeliveryModeService;

	@Resource(name = "blDatePickerService")
	private BlDatePickerService blDatePickerService;

	@Resource(name = "blOptimizedShippingMethodGenericDao")
	private GenericDao<OptimizedShippingMethodModel> blOptimizedShippingMethodGenericDao;

	private ListModelList<String> warehouseList = new ListModelList<>();
	private ListModelList<String> shippingTypeList = new ListModelList<>();
	private ListModelList<String> optimizedShippingMethodList = new ListModelList<>();
	Map<String, OptimizedShippingMethodModel> optimizedMethodList = Maps.newHashMap();
	ConsignmentModel selectedConsignment = new ConsignmentModel();

	private static final Logger LOG = Logger.getLogger(BlCreateReturnShipmentController.class);

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
				.setTitle(String.valueOf(this.getWidgetInstanceManager().getLabel("blbackoffice.inbound.label.heading")));
		warehouseList = new ListModelList<>(getWarehouseList());
		warehouseList.addToSelection(BlintegrationConstants.DEFAULT_WAREHOUSE_CODE);
		warehouseCombobox.setModel(warehouseList);
		shippingTypeList = new ListModelList<>(getShippingTypeList());
		shippingTypeList.addToSelection(CarrierEnum.UPS.getCode());
		shippingTypeComboBox.setModel(shippingTypeList);
		setOptimizedShippingMethodComboBox(CarrierEnum.UPS.getCode());
	}

	/**
	 * This method is used to close the Return Shipment Popup
	 */
	@ViewEvent(componentID = BlInventoryScanLoggingConstants.CANCEL_EVENT, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void cancel()
	{
		this.sendOutput(OUT_CONFIRM, COMPLETE);
	}

	/**
	 * this method will be used to generate return shipment
	 * @throws IOException 
	 */
	@ViewEvent(componentID = BlInventoryScanLoggingConstants.GENERATE_INBOUND_LABEL, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void generateInboundLabel() throws IOException
	{
		final List<PackagingInfoModel> packages = selectedConsignment.getPackaginginfos();


		///BLS-371:start
		final String selectedShippingType = getSelectedShippingType();
		CarrierEnum carrier = null;
		final String selectedOptimizedShippingMethod = getSelectedOptimizedShippingMethod();
		OptimizedShippingMethodModel selectedOptimizedShippingMethodModel = null;
		if (StringUtils.isNotBlank(selectedShippingType)
				&& BooleanUtils.isFalse(selectedShippingType.equals(BlintegrationConstants.DEFAULT_SHIPPING_CODE)))
		{
			for (final Map.Entry<String, OptimizedShippingMethodModel> entry : optimizedMethodList.entrySet())
			{
				if (entry.getKey().equals(selectedOptimizedShippingMethod))
				{
					selectedOptimizedShippingMethodModel = entry.getValue();
					break;
				}
			}
			carrier = selectedShippingType.equals(CarrierEnum.UPS.getCode()) ? CarrierEnum.UPS : CarrierEnum.FEDEX;
		}

		final boolean isOptimizedShippingMethodChanged = StringUtils.isNotBlank(selectedShippingType)
				&& BooleanUtils.isFalse(selectedShippingType.equals(BlintegrationConstants.DEFAULT_SHIPPING_CODE))
				&& Objects.nonNull(carrier) && Objects.nonNull(selectedOptimizedShippingMethodModel);

		//BLS-371: end

		final List<WarehouseModel> activeWarehouseList = blWarehouseGenericDao.find();
		final List<String> errorPackages = Lists.newArrayList();
		if (!BlintegrationConstants.DEFAULT_WAREHOUSE_CODE.equalsIgnoreCase(this.warehouseCombobox.getSelectedItem().getValue()))
		{
			errorPackages.addAll(createShipmentForSelectedWarehouse(packages, activeWarehouseList, carrier, selectedOptimizedShippingMethodModel,
			isOptimizedShippingMethodChanged));
		}
		else
		{
			errorPackages.addAll(createShipmentForOptimizedWarehouse(packages, carrier, selectedOptimizedShippingMethodModel, isOptimizedShippingMethodChanged));
		}
		if (CollectionUtils.isNotEmpty(errorPackages))
		{
			Messagebox.show("Error while generating Inbound label for packages with ID : " + String.join(", ", errorPackages),
					BlCoreConstants.ERROR_TITLE, Messagebox.OK, Messagebox.ERROR);
		}
		else
		{
			Messagebox.show("Inbound Label Generated Successfully", BlintegrationConstants.POPUP_TEXT, Messagebox.OK, "icon");
		}
		this.sendOutput(OUT_CONFIRM, COMPLETE);
	}

	/**
	 * method will be called when any value change for warehouse combobox
	 */
	@ViewEvent(componentID = BlInventoryScanLoggingConstants.WAREHOUSE_COMBOBOX, eventName = BlInventoryScanLoggingConstants.ON_CHANGE_EVENT)
	public void changeWarehouse()
	{
		final String selectedWarehouse = this.warehouseCombobox.getSelectedItem().getValue();
		warehouseList.addToSelection(selectedWarehouse);

	}

	/**
	 * method will be called when any value change for shipping type combobox
	 */
	@ViewEvent(componentID = BlInventoryScanLoggingConstants.SHIPPING_TYPE_COMBOBOX, eventName = BlInventoryScanLoggingConstants.ON_CHANGE_EVENT)
	public void changeOptimizedShippingType()
	{
		final String selectedShippingType = this.shippingTypeComboBox.getSelectedItem().getValue();
		shippingTypeList.addToSelection(selectedShippingType);
		setOptimizedShippingMethodComboBox(selectedShippingType);
	}

	@ViewEvent(componentID = "optimizedShippingMethodComboBox", eventName = BlInventoryScanLoggingConstants.ON_CHANGE_EVENT)
	public void changeOptimizedShippingMethod()
	{
		final String selectedOptimizedShippingMethod = this.optimizedShippingMethodComboBox.getSelectedItem().getValue();
		optimizedShippingMethodList.addToSelection(selectedOptimizedShippingMethod);
	}

	/**
	 * this method will be used to create shipment for optimized warehouse
	 *
	 * @param packages
	 * @throws IOException 
	 */
	private List<String> createShipmentForOptimizedWarehouse(final List<PackagingInfoModel> packages, final CarrierEnum carrier,
	 		final OptimizedShippingMethodModel selectedOptimizedShippingMethodModel, final boolean isOptimizedShippingMethodChanged) throws IOException
	{
		final WarehouseModel stateWarehouse = getBlDeliveryStateSourcingLocationFilter()
				.applyFilter(selectedConsignment.getOrder());
		final List<String> errorPackages = Lists.newArrayList();
		
//    BLS-39 : Need to set warehouse on the basis of delivery postalCode
//		if (stateWarehouse.equals(selectedConsignment.getWarehouse()))
//		{
//			errorPackages.addAll(startShipmentCreationProcess(packages, stateWarehouse));
//		}
//		else
//		{
//			errorPackages.addAll(startShipmentCreationProcess(packages, selectedConsignment.getWarehouse()));
//		}
		
		errorPackages.addAll(startShipmentCreationProcess(packages, stateWarehouse, carrier, selectedOptimizedShippingMethodModel, isOptimizedShippingMethodChanged));
		return errorPackages;
	}

	/**
	 * this method will be used to create shipment for selected warehouse
	 *
	 * @param packages
	 * @param activeWarehouseList
	 */
	private List<String> createShipmentForSelectedWarehouse(final List<PackagingInfoModel> packages,
			final List<WarehouseModel> activeWarehouseList, CarrierEnum carrier, OptimizedShippingMethodModel selectedOptimizedShippingMethodModel, boolean isOptimizedShippingMethodChanged)
	{
		final Optional<WarehouseModel> selectedWarehouse = activeWarehouseList.stream().filter(warehouse -> this.warehouseCombobox.getSelectedItem().getValue().equals(warehouse.getCode())).findAny();
		final List<String> errorPackages = Lists.newArrayList();
		if(selectedWarehouse.isPresent())
		{
			try
			{
				errorPackages.addAll(startShipmentCreationProcess(packages, selectedWarehouse.get(), carrier, selectedOptimizedShippingMethodModel, isOptimizedShippingMethodChanged));
			}
			catch (final IOException ioException)
			{
				BlLogger.logMessage(LOG, Level.ERROR, "An exception occurred while generating return label", ioException);
			}
		}
		return errorPackages;
	}

	/**
	 * this method will be used to start shipment process for return shipment
	 *
	 * @param packages
	 * @param stateWarehouse
	 * @throws IOException 
	 */
	private List<String> startShipmentCreationProcess(final List<PackagingInfoModel> packages, final WarehouseModel stateWarehouse, final CarrierEnum carrier,
			final OptimizedShippingMethodModel selectedOptimizedShippingMethodModel, final boolean isOptimizedShippingMethodChanged) throws IOException
	{
		final Map<String, Integer> sequenceMap = new HashedMap();
		final int packageCount = packages.size();
		final Map<String, Integer> sequenceNumber = getBlShipmentCreationService().getSequenceNumber(sequenceMap, packages, packageCount);
		final List<String> errorPackages = Lists.newArrayList();
		for (final PackagingInfoModel packagingInfoModel : packages)
		{
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Creating shipment package for {}", packagingInfoModel);
			final boolean isLabelGenerateSuccess = getBlCreateShipmentFacade().createBlReturnShipmentPackages(packagingInfoModel, stateWarehouse,packageCount, sequenceNumber, carrier, selectedOptimizedShippingMethodModel, isOptimizedShippingMethodChanged);
			if(BooleanUtils.isFalse(isLabelGenerateSuccess))
			{
				errorPackages.add(packagingInfoModel.getPackageId());
			}
			if(BooleanUtils.isTrue(isLabelGenerateSuccess)){
				updateOptimizedEndDateOnConsignment(packagingInfoModel, stateWarehouse);
				packagingInfoModel.setInboundWarehouse(stateWarehouse);
				getModelService().save(packagingInfoModel);
				getModelService().refresh(packagingInfoModel);
				BlLogger.logFormatMessageInfo(LOG, Level.INFO, "InboundWarehouse {} updated for package {}", stateWarehouse.getCode(),packagingInfoModel);

			}
		}
		return errorPackages;
	}

	private void updateOptimizedEndDateOnConsignment(PackagingInfoModel packagingInfoModel, WarehouseModel stateWarehouse) {

		ConsignmentModel consignmentModel = packagingInfoModel.getConsignment();
		if(null != consignmentModel && !(stateWarehouse.getCode().equalsIgnoreCase(consignmentModel.getWarehouse().getCode())))
		{
			String postalCode = consignmentModel.getOrder().getDeliveryAddress().getPostalcode().toString();
			String carrierID = consignmentModel.getDeliveryMode().getCode();
			int carrier = carrierID.contains(BlInventoryScanLoggingConstants.UPS) ? BlInventoryScanLoggingConstants.TWO : BlInventoryScanLoggingConstants.ONE;
			String homeBaseID = stateWarehouse.getName();
			int homeBase = homeBaseID.equalsIgnoreCase(BlInventoryScanLoggingConstants.MA) ? BlInventoryScanLoggingConstants.TWO : BlInventoryScanLoggingConstants.ONE;

			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "carrier : "+carrier+ " postalCode : "+postalCode+ " homeBase : "+homeBase);

			List<ShippingOptimizationModel> shippingOptimizationModels = getZoneDeliveryModeService().getOptimizedShippingRecords(carrier, homeBase, postalCode);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "shippingOptimizationModels DAO : "+shippingOptimizationModels);

			if(CollectionUtils.isNotEmpty(shippingOptimizationModels))
			{
				shippingOptimizationModels = shippingOptimizationModels.stream().filter(som -> som.getInbound().intValue() == 1).collect(Collectors.toList());
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "shippingOptimizationModels som : "+shippingOptimizationModels);

				// Business logic to filter warehouseModel from list of warehouse model.
				if(CollectionUtils.isNotEmpty(shippingOptimizationModels) && shippingOptimizationModels.size() > BlInventoryScanLoggingConstants.ONE) {
					shippingOptimizationModels = shippingOptimizationModels.stream().collect(minList(Comparator.comparing(ShippingOptimizationModel::getServiceDays)));
					BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "shippingOptimizationModels minList : "+shippingOptimizationModels);
				}

				if(CollectionUtils.isNotEmpty(shippingOptimizationModels))
				{
					int inboundServiceDays = shippingOptimizationModels.get(0).getServiceDays();
					final String rentalEndDate = BlDateTimeUtils.getDateInStringFormat(consignmentModel.getOrder().getRentalEndDate());
					final List<Date> blackOutDates = getBlDatePickerService().getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY);
					Date optimizedShippingEndDate = BlDateTimeUtils.addDaysInRentalDates(inboundServiceDays, rentalEndDate, blackOutDates);
					consignmentModel.setOptimizedShippingEndDate(optimizedShippingEndDate);
					getModelService().save(consignmentModel);
					getModelService().refresh(consignmentModel);

					if(null != consignmentModel.getOrder()){
						consignmentModel.getOrder().setActualRentalEndDate(optimizedShippingEndDate);
						getModelService().save(consignmentModel.getOrder());
						getModelService().refresh(consignmentModel.getOrder());
					}
					BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "updateOptimizedEndDateOnConsignment done.");
				}
			}

		}

	}

	static <T> Collector<T, ?, List<T>> minList(Comparator<? super T> comp) {
		return Collector.of(ArrayList::new, (list, t) -> {
			int c;
			if (list.isEmpty() || (c = comp.compare(t, list.get(0))) == 0)
				list.add(t);
			else if (c < 0) {
				/*
				 * We have found a smaller element than what we already have. Clear the list and
				 * add this smallest element to it.
				 */
				list.clear();
				list.add(t);
			}
		}, (list1, list2) -> {
			if (comp.compare(list1.get(0), list2.get(0)) < 0)
				return list1;
			else if (comp.compare(list1.get(0), list2.get(0)) > 0)
				return list2;
			else {
				list1.addAll(list2);
				return list1;
			}
		});
	}

	/**
	 * this method will be used to get the value which we will display on warehouse combobox
	 *
	 * @return valueList
	 */
	private List<String> getWarehouseList()
	{
		final List<String> valueList = new ArrayList<>();
		valueList.add(BlintegrationConstants.DEFAULT_WAREHOUSE_CODE);
		valueList.add(BlintegrationConstants.WAREHOUSE_MA);
		valueList.add(BlintegrationConstants.WAREHOUSE_CA);
		return valueList;
	}


	/**
	 * @return the blCreateShipmentFacade
	 */
	public BlCreateShipmentFacade getBlCreateShipmentFacade()
	{
		return blCreateShipmentFacade;
	}


	/**
	 * @param blCreateShipmentFacade
	 *           the blCreateShipmentFacade to set
	 */
	public void setBlCreateShipmentFacade(final BlCreateShipmentFacade blCreateShipmentFacade)
	{
		this.blCreateShipmentFacade = blCreateShipmentFacade;
	}


	/**
	 * @return the blDeliveryStateSourcingLocationFilter
	 */
	public BlDeliveryStateSourcingLocationFilter getBlDeliveryStateSourcingLocationFilter()
	{
		return blDeliveryStateSourcingLocationFilter;
	}


	/**
	 * @param blDeliveryStateSourcingLocationFilter
	 *           the blDeliveryStateSourcingLocationFilter to set
	 */
	public void setBlDeliveryStateSourcingLocationFilter(
			final BlDeliveryStateSourcingLocationFilter blDeliveryStateSourcingLocationFilter)
	{
		this.blDeliveryStateSourcingLocationFilter = blDeliveryStateSourcingLocationFilter;
	}

	public DefaultBLShipmentCreationService getBlShipmentCreationService()
	{
		return blShipmentCreationService;
	}

	public void setBlShipmentCreationService(DefaultBLShipmentCreationService blShipmentCreationService)
	{
		this.blShipmentCreationService = blShipmentCreationService;
	}

	public ModelService getModelService() {
		return modelService;
	}

	public void setModelService(ModelService modelService) {
		this.modelService = modelService;
	}

	public BlDeliveryModeService getZoneDeliveryModeService() {
		return zoneDeliveryModeService;
	}

	public void setZoneDeliveryModeService(BlDeliveryModeService zoneDeliveryModeService) {
		this.zoneDeliveryModeService = zoneDeliveryModeService;
	}

	public BlDatePickerService getBlDatePickerService() {
		return blDatePickerService;
	}

	public void setBlDatePickerService(BlDatePickerService blDatePickerService) {
		this.blDatePickerService = blDatePickerService;
	}

	private List<String> getShippingTypeList()
	{
		final List<String> shippingTypesList = org.assertj.core.util.Lists.newArrayList();
		//shippingTypesList.add(BlintegrationConstants.DEFAULT_SHIPPING_CODE);
		shippingTypesList.add(CarrierEnum.UPS.getCode());
		shippingTypesList.add(CarrierEnum.FEDEX.getCode());
		return shippingTypesList;
	}

	private void setOptimizedShippingMethodComboBox(final String selectedShippingType)
	{
		final List<String> carrierBasedOptimizedShippingMethodList = org.assertj.core.util.Lists.newArrayList();
		final List<OptimizedShippingMethodModel> allOptimizedShippingMethodList = getBlOptimizedShippingMethodGenericDao().find();
		allOptimizedShippingMethodList.forEach(
				optimizedShippingMethod -> optimizedMethodList.put(optimizedShippingMethod.getName(), optimizedShippingMethod));
		if (selectedShippingType.equalsIgnoreCase(CarrierEnum.UPS.getCode()))
		{
			optimizedShippingMethodList.clearSelection();
			for (final OptimizedShippingMethodModel optimizedShippingMethod : allOptimizedShippingMethodList)
			{
				if (!optimizedShippingMethod.getCode().toLowerCase().contains(CarrierEnum.FEDEX.getCode().toLowerCase()))
				{
					carrierBasedOptimizedShippingMethodList.add(optimizedShippingMethod.getName());
				}

			}
		}
		else if (selectedShippingType.equalsIgnoreCase(CarrierEnum.FEDEX.getCode()))
		{
			optimizedShippingMethodList.clearSelection();
			for (final OptimizedShippingMethodModel optimizedShippingMethod : allOptimizedShippingMethodList)
			{
				if (optimizedShippingMethod.getCode().toLowerCase().contains(CarrierEnum.FEDEX.getCode().toLowerCase()))
				{
					carrierBasedOptimizedShippingMethodList.add(optimizedShippingMethod.getName());
				}

			}
		}
		else
		{
			carrierBasedOptimizedShippingMethodList.clear();
			optimizedShippingMethodList.clearSelection();
		}
		optimizedShippingMethodList = new ListModelList<>(carrierBasedOptimizedShippingMethodList);
		optimizedShippingMethodComboBox.setModel(optimizedShippingMethodList);

	}

	private String getSelectedShippingType()
	{
		return Objects.nonNull(this.shippingTypeComboBox.getSelectedItem()) ? this.shippingTypeComboBox.getSelectedItem().getValue()
				: StringUtils.EMPTY;
	}

	private String getSelectedOptimizedShippingMethod()
	{
		return Objects.nonNull(this.optimizedShippingMethodComboBox.getSelectedItem())
				? this.optimizedShippingMethodComboBox.getSelectedItem().getValue()
				: StringUtils.EMPTY;
	}

	public GenericDao<OptimizedShippingMethodModel> getBlOptimizedShippingMethodGenericDao() {
		return blOptimizedShippingMethodGenericDao;
	}

	public void setBlOptimizedShippingMethodGenericDao(GenericDao<OptimizedShippingMethodModel> blOptimizedShippingMethodGenericDao) {
		this.blOptimizedShippingMethodGenericDao = blOptimizedShippingMethodGenericDao;
	}
}

