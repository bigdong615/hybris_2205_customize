/**
 *@author Keyur Patel
 */
package com.bl.backoffice.widget.controller;

import de.hybris.platform.enumeration.EnumerationService;
import de.hybris.platform.ordersplitting.WarehouseService;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.search.restriction.SearchRestrictionService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.warehousing.model.PackagingInfoModel;
import de.hybris.platform.warehousingfacades.order.data.PackagingInfoData;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.annotation.Resource;

import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.zkoss.zk.ui.Component;
import org.zkoss.zk.ui.event.Event;
import org.zkoss.zk.ui.event.Events;
import org.zkoss.zk.ui.select.annotation.Listen;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zk.ui.select.annotation.WireVariable;
import org.zkoss.zul.Checkbox;
import org.zkoss.zul.Combobox;
import org.zkoss.zul.Grid;
import org.zkoss.zul.ListModelList;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Row;
import org.zkoss.zul.Textbox;

import com.bl.blbackoffice.dto.SerialProductDTO;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.dao.BlProductDao;
import com.bl.facades.warehouse.BLWarehousingConsignmentFacade;
import com.hybris.backoffice.i18n.BackofficeLocaleService;
import com.hybris.backoffice.widgets.notificationarea.NotificationService;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.core.events.CockpitEventQueue;
import com.hybris.cockpitng.util.DefaultWidgetController;


public class CreatePackageController extends DefaultWidgetController
{
	private static final long serialVersionUID = 1L;
	protected static final String IN_SOCKET = "consignmentInput";
	protected static final String OUT_CONFIRM = "confirmOutput";
	protected static final Object COMPLETED = "completed";
	protected static final String CONSIGNMENT_ACTION_EVENT_NAME = "ConsignmentActionEvent";
	protected static final String REALLOCATE_CONSIGNMENT_CHOICE = "reallocateConsignment";
	protected static final String DECLINE_ENTRIES = "declineEntries";
	protected static final int COLUMN_INDEX_REALLOCATION_QUANTITY = 4;
	protected static final int COLUMN_INDEX_REALLOCATION_REASON = 5;
	protected static final int COLUMN_INDEX_REALLOCATION_LOCATION = 6;
	protected static final int COLUMN_INDEX_REALLOCATION_COMMENT = 7;
	private ConsignmentModel consignment;
	@Wire
	private Textbox consignmentCode;
	@Wire
	private Textbox customerName;

	@Wire
	private Textbox selectedProduct;
	//	@Wire
	//	private Combobox globalDeclineReasons;
	//	@Wire
	//	private Textbox globalDeclineComment;
	//	@Wire
	private Grid serialEntries;


	@Wire
	private Checkbox serialEntry;

	@WireVariable
	private transient WarehouseService warehouseService;
	@WireVariable
	private transient EnumerationService enumerationService;

	@WireVariable
	private transient ModelService modelService;
	@WireVariable
	private transient BackofficeLocaleService cockpitLocaleService;
	@WireVariable
	private transient CockpitEventQueue cockpitEventQueue;
	@WireVariable
	private transient NotificationService notificationService;

	@Resource(name = "blWarehousingConsignmentFacade")
	private BLWarehousingConsignmentFacade blWarehousingConsignmentFacade;
	@Wire
	private Textbox packageWeight;

	@Wire
	private Textbox packageHeight;

	@Wire
	private Textbox packageLength;

	@Wire
	private Textbox packageWidth;

	@Wire
	private Textbox totalWeight;

	@Wire
	private Combobox boxes;

	ListModelList<PackagingInfoData> packages = null;
	List<PackagingInfoData> packingInfo = null;
	double weight = 0.0;
	Set<BlSerialProductModel> allSerialProducts = new HashSet<BlSerialProductModel>();
	Set<BlSerialProductModel> selectedSerialProducts = new HashSet<BlSerialProductModel>();

	@Resource(name = "productDao")
	private BlProductDao productDao;

	@Autowired
	private SearchRestrictionService searchRestrictionService;
	@Autowired
	private SessionService sessionService;

	@SocketEvent(socketId = "inputObject")
	public void initReallocationConsignmentForm(final ConsignmentModel consignment)
	{
		setConsignment(consignment);
		this.consignmentCode.setValue(consignment.getCode());
		this.customerName.setValue(consignment.getOrder().getUser().getUid());
		this.serialEntry.setChecked(false);
		if (!CollectionUtils.isEmpty(consignment.getConsignmentEntries()))
		{
			getListOfPackedSerials(consignment);

			if (CollectionUtils.isNotEmpty(this.allSerialProducts))
			{
				setWidgetTitle("Create Package");
				final List<SerialProductDTO> serials = new ArrayList<SerialProductDTO>();
				for (final BlSerialProductModel blSerialProductModel : allSerialProducts)
				{
					final SerialProductDTO serialProduct = new SerialProductDTO();
					serialProduct.setSerialProduct(blSerialProductModel);
					this.weight = blSerialProductModel.getBlProduct().getWeight().doubleValue() + this.weight;
					serials.add(serialProduct);
				}
				this.serialEntries.setModel(new ListModelList<SerialProductDTO>(serials));
				final ListModelList<PackagingInfoData> packages = new ListModelList<PackagingInfoData>(createPackageCombobox());
				final PackagingInfoData packagingInfoData = packages.get(0);
				packages.addToSelection(packagingInfoData);
				this.boxes.setModel(packages);
				this.totalWeight.setValue(calculateTotalWeight(this.weight));
			}
			else
			{
				this.sendOutput("confirmOutput", "Completed");
				Messagebox.show("All Serials are assigned to the package, " + "hence no Serials are available", "Info", Messagebox.OK,
						"icon");
			}
		}
	}


	/**
	 * @param consignment
	 */
	private void getListOfPackedSerials(final ConsignmentModel consignment)
	{
		getModelService().refresh(consignment);
		for (final ConsignmentEntryModel consignmentEntryModel : consignment.getConsignmentEntries())
		{
			allSerialProducts.addAll(consignmentEntryModel.getSerialProducts());
		}
		final List<PackagingInfoModel> packages = consignment.getPackaginginfos();

		for (final PackagingInfoModel packagingInfoModel : packages)
		{
			final Set<BlSerialProductModel> packageSerails = packagingInfoModel.getSerialProducts();
			if (CollectionUtils.isNotEmpty(packageSerails))
			{
				this.allSerialProducts.removeAll(packageSerails);
			}
		}
	}


	@ViewEvent(componentID = "boxes", eventName = "onChange")
	public void packageChange()
	{
		if (CollectionUtils.isNotEmpty(this.packingInfo))
		{
			for (final PackagingInfoData packagingInfoData : this.packingInfo)
			{
				if (packagingInfoData.getDimension().equals(this.boxes.getValue().trim()))
				{
					this.packageHeight.setValue(packagingInfoData.getHeight());
					this.packageWeight.setValue(packagingInfoData.getGrossWeight());
					this.packageWidth.setValue(packagingInfoData.getWidth());
					this.packageLength.setValue(packagingInfoData.getLength());
				}
			}
			this.totalWeight.setValue(calculateTotalWeight(this.weight));
		}
	}

	@ViewEvent(componentID = "selectedProduct", eventName = "onChange")
	public void packageSelectedProduct()
	{
		System.out.println(this.selectedProduct.getValue());
		if (CollectionUtils.isNotEmpty(this.allSerialProducts) && this.selectedProduct != null
				&& this.selectedProduct.getValue().contains("_"))
		{
			final String[] selctedCheckBoxArray = this.selectedProduct.getValue().split("_");
			for (final BlSerialProductModel serialProduct : this.allSerialProducts)
			{
				if (serialProduct.getCode().equals(selctedCheckBoxArray[0]))
				{
					if (selctedCheckBoxArray[1].equals("true"))
					{
						this.selectedSerialProducts.add(serialProduct);
					}
					else
					{
						this.selectedSerialProducts.remove(serialProduct);
					}
					break;
				}
			}
		}
	}

	private List<PackagingInfoData> createPackageCombobox()
	{
		this.packages = new ListModelList<PackagingInfoData>();
		this.packingInfo = getBlWarehousingConsignmentFacade().getAllPackagingDimensions();
		this.packageHeight.setValue(packingInfo.get(0).getHeight());
		this.packageWeight.setValue(packingInfo.get(0).getGrossWeight());
		this.packageWidth.setValue(packingInfo.get(0).getWidth());
		this.packageLength.setValue(packingInfo.get(0).getLength());
		return packingInfo;
	}

	@ViewEvent(componentID = "undochanges", eventName = "onClick")
	public void reset()
	{
		this.sendOutput("confirmOutput", "Completed");
	}

	@ViewEvent(componentID = "createPackaging", eventName = "onClick")
	public void confirmOrder()
	{
		if (CollectionUtils.isEmpty(this.selectedSerialProducts))
		{
			Messagebox.show("Atleast Select one Serial Product", "Error Occured", Messagebox.CANCEL, "icon");
		}
		else
		{
			final PackagingInfoData packagingData = new PackagingInfoData();
			packagingData.setHeight(this.packageHeight.getValue());
			packagingData.setLength(this.packageLength.getValue());
			packagingData.setWidth(this.packageWidth.getValue());
			packagingData.setGrossWeight(this.totalWeight.getValue());
			packagingData.setDimensionUnit("In");
			packagingData.setWeightUnit("lb");

			final PackagingInfoModel packagingInfo = getBlWarehousingConsignmentFacade()
					.createPackagingInformationOnConsignment(this.consignment.getCode(), packagingData);

			if (null != packagingInfo)
			{
				packagingInfo.setSerialProducts(this.selectedSerialProducts);
				modelService.save(packagingInfo);
				modelService.refresh(packagingInfo);
				this.showMessageBox();
			}
		}
	}

	protected void showMessageBox()
	{
		Messagebox.show("Details Updated Successfully");
		this.sendOutput("confirmOutput", "Completed");
	}

	@Listen(Events.ON_CHECK)
	public void onValueCheck(final Event event)
	{
		System.out.println("event");
	}

	protected List<Component> getOrderEntriesGridRows()
	{
		return this.getSerialEntries().getRows().getChildren();
	}

	protected ConsignmentModel getConsignment()
	{
		return this.consignment;
	}

	private String calculateTotalWeight(double weight)
	{
		weight = weight + Double.valueOf(this.packageWeight.getValue());
		return String.valueOf(weight);
	}


	public void setConsignment(final ConsignmentModel consignment)
	{
		this.consignment = consignment;
	}


	protected EnumerationService getEnumerationService()
	{
		return this.enumerationService;
	}


	protected Grid getSerialEntries()
	{
		return this.serialEntries;
	}

	protected ModelService getModelService()
	{
		return this.modelService;
	}


	protected BackofficeLocaleService getCockpitLocaleService()
	{
		return this.cockpitLocaleService;
	}


	protected CockpitEventQueue getCockpitEventQueue()
	{
		return this.cockpitEventQueue;
	}


	protected void handleRow(final Row row)
	{
		System.out.println("inside row");
		final SerialProductDTO myEntry = (SerialProductDTO) row.getValue();
		if (!((Checkbox) row.getChildren().iterator().next()).isChecked())
		{
			System.out.println("Check Box false" + myEntry.getSerialProduct().getCode());
		}
		else
		{
			System.out.println("Check box true" + myEntry.getSerialProduct().getCode());
		}
	}


	/**
	 * @return the searchRestrictionService
	 */
	public SearchRestrictionService getSearchRestrictionService()
	{
		return searchRestrictionService;
	}

	/**
	 * @param searchRestrictionService
	 *           the searchRestrictionService to set
	 */
	public void setSearchRestrictionService(final SearchRestrictionService searchRestrictionService)
	{
		this.searchRestrictionService = searchRestrictionService;
	}

	/**
	 * @return the sessionService
	 */
	public SessionService getSessionService()
	{
		return sessionService;
	}

	/**
	 * @param sessionService
	 *           the sessionService to set
	 */
	public void setSessionService(final SessionService sessionService)
	{
		this.sessionService = sessionService;
	}

	/**
	 * @return the productDao
	 */
	public BlProductDao getProductDao()
	{
		return productDao;
	}

	/**
	 * @param productDao
	 *           the productDao to set
	 */
	public void setProductDao(final BlProductDao productDao)
	{
		this.productDao = productDao;
	}


	/**
	 * @return the blWarehousingConsignmentFacade
	 */
	public BLWarehousingConsignmentFacade getBlWarehousingConsignmentFacade()
	{
		return blWarehousingConsignmentFacade;
	}


	/**
	 * @param blWarehousingConsignmentFacade
	 *           the blWarehousingConsignmentFacade to set
	 */
	public void setBlWarehousingConsignmentFacade(final BLWarehousingConsignmentFacade blWarehousingConsignmentFacade)
	{
		this.blWarehousingConsignmentFacade = blWarehousingConsignmentFacade;
	}

}

