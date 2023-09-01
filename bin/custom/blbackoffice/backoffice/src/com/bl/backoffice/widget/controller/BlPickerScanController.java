package com.bl.backoffice.widget.controller;

import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;

import java.util.List;
import java.util.stream.Collectors;

import javax.annotation.Resource;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
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

//import net.sf.cglib.core.CollectionUtils;


public class BlPickerScanController extends DefaultWidgetController
{
	private static final Logger LOG = Logger.getLogger(BlPickerScanController.class);

	protected static final String IN_SOCKET = "nodeSelected";
	protected static final String OUT_CONFIRM = "confirmOutput";
	protected static final String COMPLETE = "completed";
	private static final String HIDE_DIV = "resize:none;display:none";

	@Resource(name = "baseStoreService")
	private BaseStoreService baseStoreService;

	@Resource(name = "blConsignmentDao")
	BlConsignmentDao blConsignmentDao;

	@Resource(name = "modelService")
	private ModelService modelService;


	@Wire
	private Div pickerDataHeader;

	@Wire
	Textbox scanningArea;

	/*
	 * @Wire Textbox textInput;
	 */
	@Wire
	private Combobox warehousesCombox;

	List<ConsignmentModel> consignmentModels;
	List<ConsignmentModel> filterConsignmentModels;


	private transient WebScanToolData shippingScanToolData;

	@SocketEvent(socketId = IN_SOCKET)
	public void initLoadPage(final Object inputObject)
	{
		//  selectedConsignment = inputObject;
		shippingScanToolData = new WebScanToolData();
		this.getWidgetInstanceManager()
				.setTitle(String.valueOf(this.getWidgetInstanceManager().getLabel("blbackoffice.picker.scan.heading")));
		// this.pickerDataHeader.setStyle(HIDE_DIV);
		final BaseStoreModel baseStoreModel = baseStoreService.getBaseStoreForUid(BlCoreConstants.BASE_STORE_ID);
		//Get all warehouses
		final List<WarehouseModel> warehouses = baseStoreModel.getWarehouses();

		warehousesCombox.setModel(new ListModelList<>(warehouses));
	}

	@ViewEvent(componentID = BlInventoryScanLoggingConstants.CANCEL_EVENT, eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void cancel()
	{
		this.sendOutput(OUT_CONFIRM, COMPLETE);
	}

	@ViewEvent(componentID = "scanOrder", eventName = BlInventoryScanLoggingConstants.ON_CLICK_EVENT)
	public void scanOrder()
	{
		if (StringUtils.isEmpty(scanningArea.getText()))
		{
			LOG.info("plese enter atleast one order");
			//  System.out.println(textInput.getText());

		}
		else if (this.warehousesCombox.getSelectedItem().getValue() == null)
		{
			LOG.info("please select warehouse");
		}
		else
		{
			LOG.info("going for order processing");
			//   System.out.println(textInput.getText());
			final Object value = this.warehousesCombox.getSelectedItem().getValue();
			if (value instanceof WarehouseModel)
			{
				final WarehouseModel warehouseModel = (WarehouseModel) value;
				final String orders = scanningArea.getText();
				final String[] orderList = orders.split("\n");

				getConsignment(warehouseModel, orderList);
			}

		}
	}

	private UserService userService;

	private void getConsignment(final WarehouseModel selectedWarehouse, final String[] orderList)
	{
		consignmentModels = blConsignmentDao.getConsignmentByOrderAndWarehouseCode(selectedWarehouse, orderList);
		final UserModel user = userService.getCurrentUser();
		if (user != null)
		{

			// EmployeeModel employeeModel = (EmployeeModel)user;
			if (CollectionUtils.isNotEmpty(consignmentModels))
			{
				final List<ConsignmentModel> filterConsignmentModels = consignmentModels.stream()
						.filter(consignment -> StringUtils.isNotBlank(consignment.getPicker())
								&& !consignment.getPicker().equals(user.getUid()))
						.collect(Collectors.toList());
				if (CollectionUtils.isEmpty(filterConsignmentModels))
				{
					consignmentModels.forEach(consignment -> consignment.setPicker(user.getUid()));
					modelService.saveAll(consignmentModels);
					LOG.info("All the consignment for given warehouse updated with");
					Messagebox.show("All the order updated");
				}
				else
				{


					Messagebox.show("userID is already assigned to some order number");

				}
			}

		}
	}

}




