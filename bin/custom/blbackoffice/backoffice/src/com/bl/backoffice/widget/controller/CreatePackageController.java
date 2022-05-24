/**
 *@author Keyur Patel
 */
package com.bl.backoffice.widget.controller;

import com.bl.blbackoffice.dto.SerialProductDTO;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.enums.ItemStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.dao.BlProductDao;
import com.bl.facades.warehouse.BLWarehousingConsignmentFacade;
import com.bl.integration.constants.BlintegrationConstants;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
import com.hybris.backoffice.i18n.BackofficeLocaleService;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.core.events.CockpitEventQueue;
import com.hybris.cockpitng.util.DefaultWidgetController;

import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.enumeration.EnumerationService;
import de.hybris.platform.ordersplitting.WarehouseService;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.warehousing.model.PackagingInfoModel;
import de.hybris.platform.warehousingfacades.order.data.PackagingInfoData;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.Component;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zk.ui.select.annotation.WireVariable;
import org.zkoss.zul.Checkbox;
import org.zkoss.zul.Combobox;
import org.zkoss.zul.Grid;
import org.zkoss.zul.ListModelList;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Textbox;


public class CreatePackageController extends DefaultWidgetController
{
	 private static final Logger LOG = Logger.getLogger(CreatePackageController.class);
	
	private static final long serialVersionUID = 1L;
	protected static final String IN_SOCKET = "consignmentInput";
	protected static final String OUT_CONFIRM = "confirmOutput";
	protected static final String COMPLETE = "completed";
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

	public ListModelList<PackagingInfoData> packages = null; //NOSONAR
	private List<PackagingInfoData> packingInfo = null;

	double weight = 0.0;
	private final Set<BlProductModel> allSerialProducts = new HashSet<>();
	private final Set<BlProductModel> selectedSerialProducts = new HashSet<>();

	@Resource(name = "productDao")
	private BlProductDao productDao;

	@SocketEvent(socketId = "inputObject")
	public void initReallocationConsignmentForm(final ConsignmentModel consignment)
	{
		setConsignment(consignment);
		try
		{
			if (!ConsignmentStatus.SHIPPING_MANUAL_REVIEW.equals(consignment.getStatus()))
			{
				this.consignmentCode.setValue(consignment.getCode());
				this.customerName.setValue(consignment.getOrder().getUser().getUid());
				this.serialEntry.setChecked(false);
				if (!CollectionUtils.isEmpty(consignment.getConsignmentEntries()))
				{
					getListOfPackedSerials(consignment);

					if (CollectionUtils.isNotEmpty(this.allSerialProducts))
					{
						createShipmentPackage();
					}
					else
					{
						this.sendOutput(OUT_CONFIRM, COMPLETE);
						Messagebox.show("All Serials are assigned to the package, " + "hence no Serials are available", "Info",
								Messagebox.OK, "icon");
					}
				}
			}
		}
		catch (final Exception e)
		{
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Exception occour for {} ", e);
		}
	}


	/**
	 * This method is used to create shipment packages
	 */
	private void createShipmentPackage()
	{
		setWidgetTitle("Create Package");
		final List<SerialProductDTO> serials = new ArrayList<>();
		for (final BlProductModel blProductModel : allSerialProducts)
		{
			if(blProductModel instanceof BlSerialProductModel) {
				final BlSerialProductModel blSerialProductModel = (BlSerialProductModel) blProductModel;
				final SerialProductDTO serialProduct = new SerialProductDTO();
				serialProduct.setSerialProduct(blSerialProductModel);
				this.weight = blSerialProductModel.getBlProduct().getWeight().doubleValue() + this.weight;
				serials.add(serialProduct);
			}
		}
		this.serialEntries.setModel(new ListModelList<>(serials));
		final ListModelList<PackagingInfoData> packageBox = new ListModelList<>(createPackageCombobox());
		final PackagingInfoData packagingInfoData = packageBox.get(0);
		packageBox.addToSelection(packagingInfoData);
		this.boxes.setModel(packageBox);
		this.totalWeight.setValue(calculateTotalWeight(this.weight));
	}


	/**
	 * @param consignment
	 *           all the list of already packed serials
	 */
	private void getListOfPackedSerials(final ConsignmentModel consignment)
	{
		getModelService().refresh(consignment);
		for (final ConsignmentEntryModel consignmentEntryModel : consignment.getConsignmentEntries())
		{
			final Map<String, ItemStatusEnum> itemsMap = consignmentEntryModel.getItems();

			consignmentEntryModel.getSerialProducts().stream().filter(serialProduct -> itemsMap.containsKey(serialProduct.getCode())
					&& ItemStatusEnum.INCLUDED.equals(itemsMap.get(serialProduct.getCode()))).forEach(allSerialProducts::add);
		}
		final List<PackagingInfoModel> packageInfo = consignment.getPackaginginfos();

		for (final PackagingInfoModel packagingInfoModel : packageInfo)
		{
			final List<BlProductModel> packageSerails = packagingInfoModel.getSerialProducts();
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
		
		if (CollectionUtils.isNotEmpty(this.allSerialProducts) && this.selectedProduct != null
				&& this.selectedProduct.getValue().contains(BlInventoryScanLoggingConstants.SPLIT_STRING))
		{
			final String[] selctedCheckBoxArray = this.selectedProduct.getValue().split(BlInventoryScanLoggingConstants.SPLIT_STRING);
			for (final BlProductModel serialProduct : this.allSerialProducts)
			{
				BlLogger.logMessage(LOG, Level.INFO, "************ Selected Serial Product ***********" + serialProduct);

				if (serialProduct.getCode().equals(selctedCheckBoxArray[0]))
				{
					BlLogger.logMessage(LOG, Level.INFO, "************ Inside if Condition ***********" + serialProduct);
					
					addOrRemoveSelectedSerialProduct(selctedCheckBoxArray, serialProduct);
					break;
				}
			}
		}
	}


	/**
	 * method will use to add or remove selected serial product
	 * @param selctedCheckBoxArray
	 * @param serialProduct
	 */
	private void addOrRemoveSelectedSerialProduct(final String[] selctedCheckBoxArray, final BlProductModel serialProduct)
	{
		if (BlInventoryScanLoggingConstants.TRUE_STRING.equals(selctedCheckBoxArray[1]))
		{
			this.selectedSerialProducts.add(serialProduct);
			BlLogger.logMessage(LOG, Level.INFO, "************ Serial Product Added***********" + serialProduct);
		}
		else
		{
			this.selectedSerialProducts.remove(serialProduct);
			BlLogger.logMessage(LOG, Level.INFO, "************ Serial Product Removed***********" + serialProduct);
		}
	}

	private List<PackagingInfoData> createPackageCombobox()
	{
		this.packages = new ListModelList<>();
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
		this.sendOutput(OUT_CONFIRM, COMPLETE);
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
			packagingData.setDimensionUnit(BlintegrationConstants.DIMENSION_UNIT);
			packagingData.setWeightUnit(BlintegrationConstants.WEIGHT_UNIT);

			final PackagingInfoModel packagingInfo = getBlWarehousingConsignmentFacade()
					.createPackagingInformationOnConsignment(this.consignment.getCode(), packagingData);

			if (null != packagingInfo)
			{
				packagingInfo.setSerialProducts(Lists.newArrayList(this.selectedSerialProducts));
				modelService.save(packagingInfo);
				modelService.refresh(packagingInfo);
				this.showMessageBox();
			}
		}
	}

	protected void showMessageBox()
	{
		Messagebox.show("Details Updated Successfully");
		this.sendOutput(OUT_CONFIRM, COMPLETE);
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

