package com.bl.backoffice.widget.controller;

import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import javax.annotation.Resource;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zul.Combobox;
import org.zkoss.zul.Div;
import org.zkoss.zul.ListModelList;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Textbox;

import com.bl.backoffice.wizards.util.WebScanToolData;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.dao.warehouse.BlConsignmentDao;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;


public class BlPickerScanController extends DefaultWidgetController
{
	private static final Logger LOG = Logger.getLogger(BlPickerScanController.class);

	protected static final String IN_SOCKET = "nodeSelected";
	protected static final String OUT_CONFIRM = "confirmOutput";
	protected static final String COMPLETE = "completed";

	@Resource(name = "baseStoreService")
	private BaseStoreService baseStoreService;

	@Resource(name = "blConsignmentDao")
	BlConsignmentDao blConsignmentDao;

	@Resource(name = "modelService")
	private ModelService modelService;
	@Resource(name="userService")
	private UserService userService;

	@Wire
	private Div pickerDataHeader;

	@Wire
	Textbox scanningArea;

	@Wire
	private Combobox warehousesCombox;

	UserModel user;
	List<ConsignmentModel> allConsignments;
	List<ConsignmentModel> remainingConsignments;

	@SocketEvent(socketId = IN_SOCKET)
	public void initLoadPage(final Object inputObject)
	{
		this.getWidgetInstanceManager()
				.setTitle(String.valueOf(this.getWidgetInstanceManager().getLabel("blbackoffice.picker.scan.heading")));
		final BaseStoreModel baseStoreModel = baseStoreService.getBaseStoreForUid(BlCoreConstants.BASE_STORE_ID);
		//Get all warehouses
		final List<WarehouseModel> warehouses = baseStoreModel.getWarehouses();
		warehousesCombox.setModel(new ListModelList<>(warehouses));
		user=userService.getCurrentUser();
	}

	@ViewEvent(componentID = "scanOrder", eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void scanOrder()
	{
		if (StringUtils.isEmpty(scanningArea.getText()))
		{
			Messagebox.show("Please enter atleast one Order");
		}
		else if (this.warehousesCombox.getSelectedItem()==null || this.warehousesCombox.getSelectedItem().getValue() == null)
		{
			Messagebox.show("Please select warehouse");
		}
		else
		{
			LOG.info("Processing order for picker scan");
			final Object value = this.warehousesCombox.getSelectedItem().getValue();
			if (value instanceof WarehouseModel)
			{
				final WarehouseModel warehouseModel = (WarehouseModel) value;
				final String orders = scanningArea.getText();
				final String[] orderList = orders.split("\n");
				try {
					getConsignment(warehouseModel, orderList);
				}catch (Exception e){
					Messagebox.show("Some error occur while processing consignment");
					BlLogger.logMessage(LOG,Level.ERROR,"Some error occur while processing consignment",e);

				}
			}

		}
	}

	private void getConsignment(final WarehouseModel selectedWarehouse, final String[] orderList)
	{
		allConsignments = blConsignmentDao.getConsignmentByOrderAndWarehouseCode(selectedWarehouse, orderList);
		if (user != null)
		{
			if (CollectionUtils.isNotEmpty(allConsignments)) {
				final List<ConsignmentModel> associatedOtherUserConsignments = allConsignments.stream()
						.filter(consignment -> StringUtils.isNotBlank(consignment.getPicker())
								&& !consignment.getPicker().equals(user.getUid()))
						.collect(Collectors.toList());
				if (CollectionUtils.isEmpty(associatedOtherUserConsignments)) {
					updatePickerAndSaveConsignment(allConsignments);
				} else {
					remainingConsignments = new ArrayList<>(allConsignments);
					remainingConsignments.removeAll(associatedOtherUserConsignments);
					BlLogger.logFormatMessageInfo(LOG,Level.INFO,"Number of associated order consignment {}",associatedOtherUserConsignments.size());
					BlLogger.logFormatMessageInfo(LOG,Level.INFO,"Number of remaining order consignment {} ", remainingConsignments.size());
					this.sendOutput(OUT_CONFIRM, COMPLETE);

					Messagebox.show("Some order already associated with other user. Do you want to assign you?", "Picker Confirmation", new Messagebox.Button[]
							{Messagebox.Button.YES, Messagebox.Button.NO},null, Messagebox.QUESTION, null, clickEvent -> {
						if (Messagebox.Button.YES == clickEvent.getButton())
						{
							updateCurrentUserToAllConsignment();
						}else{
							updateCurrentUserToRemainingConsignment();
						}
					}, null);

				}
			}
		}
	}
	public void updateCurrentUserToAllConsignment()
	{
		updatePickerAndSaveConsignment(allConsignments);
	}

	public void updateCurrentUserToRemainingConsignment()
	{
		updatePickerAndSaveConsignment(remainingConsignments);
	}

	private void updatePickerAndSaveConsignment(List<ConsignmentModel> consingmentList){
		if (CollectionUtils.isNotEmpty(consingmentList))
		{
			consingmentList.forEach(consignment -> consignment.setPicker(user.getUid()));
			modelService.saveAll(consingmentList);
			LOG.info("All the consignment for given warehouse updated with user :"+user.getUid());
			Messagebox.show("All the Consignment updated with user :"+user.getUid());
		}else {
			Messagebox.show("Consignment list are Empty");
		}
	}

	@ViewEvent(componentID = BlInventoryScanLoggingConstants.CANCEL_EVENT, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void cancel()
	{
		this.sendOutput(OUT_CONFIRM, COMPLETE);
	}

}




