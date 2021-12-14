package com.bl.backoffice.widget.controller;

import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.internal.dao.GenericDao;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.annotation.Resource;

import org.apache.commons.collections.map.HashedMap;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zul.Combobox;
import org.zkoss.zul.ListModelList;

import com.bl.Ordermanagement.filters.BlDeliveryStateSourcingLocationFilter;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.integration.constants.BlintegrationConstants;
import com.bl.integration.facades.BlCreateShipmentFacade;
import com.bl.integration.services.impl.DefaultBLShipmentCreationService;
import com.bl.logging.BlLogger;
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

	private ListModelList<String> warehouseList = new ListModelList<>();

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
	 */
	@ViewEvent(componentID = BlInventoryScanLoggingConstants.GENERATE_INBOUND_LABEL, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void generateInboundLabel()
	{
		final List<PackagingInfoModel> packages = selectedConsignment.getPackaginginfos();

		final List<WarehouseModel> activeWarehouseList = blWarehouseGenericDao.find();

		if (!BlintegrationConstants.DEFAULT_WAREHOUSE_CODE.equalsIgnoreCase(this.warehouseCombobox.getSelectedItem().getValue()))
		{
			createShipmentForSelectedWarehouse(packages, activeWarehouseList);
		}
		else
		{
			createShipmentForOptimizedWarehouse(packages);
		}

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
	 * this method will be used to create shipment for optimized warehouse
	 *
	 * @param packages
	 */
	private void createShipmentForOptimizedWarehouse(final List<PackagingInfoModel> packages)
	{
		final WarehouseModel stateWarehouse = getBlDeliveryStateSourcingLocationFilter()
				.applyFilter(selectedConsignment.getOrder());

		if (stateWarehouse.equals(selectedConsignment.getWarehouse()))
		{
			startShipmentCreationProcess(packages, stateWarehouse);
		}
		else
		{
			startShipmentCreationProcess(packages, selectedConsignment.getWarehouse());
		}
	}

	/**
	 * this method will be used to create shipment for selected warehouse
	 *
	 * @param packages
	 * @param activeWarehouseList
	 */
	private void createShipmentForSelectedWarehouse(final List<PackagingInfoModel> packages,
			final List<WarehouseModel> activeWarehouseList)
	{
		activeWarehouseList.forEach(warehouse -> {
			if (this.warehouseCombobox.getSelectedItem().getValue().equals(warehouse.getCode()))
			{
				startShipmentCreationProcess(packages, warehouse);
			}
		});
	}

	/**
	 * this method will be used to start shipment process for return shipment
	 *
	 * @param packages
	 * @param stateWarehouse
	 */
	private void startShipmentCreationProcess(final List<PackagingInfoModel> packages, final WarehouseModel stateWarehouse)
	{
		final Map<String, Integer> sequenceMap = new HashedMap();
		final int packageCount = packages.size();
		final Map<String, Integer> sequenceNumber = getBlShipmentCreationService().getSequenceNumber(sequenceMap, packages, packageCount);
		
		for (final PackagingInfoModel packagingInfoModel : packages)
		{
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Creating shipment package for {}", packagingInfoModel);
			getBlCreateShipmentFacade().createBlReturnShipmentPackages(packagingInfoModel, stateWarehouse,packageCount, sequenceNumber);
		}
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

}

