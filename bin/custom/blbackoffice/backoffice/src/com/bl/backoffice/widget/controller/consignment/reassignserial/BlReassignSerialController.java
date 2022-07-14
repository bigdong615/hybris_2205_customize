package com.bl.backoffice.widget.controller.consignment.reassignserial;

import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.warehousing.data.ConsignmentEntriesData;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.annotation.Resource;

import org.zkoss.zk.ui.Component;
import org.zkoss.zk.ui.WrongValueException;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zk.ui.select.annotation.WireVariable;
import org.zkoss.zul.Checkbox;
import org.zkoss.zul.Grid;
import org.zkoss.zul.Label;
import org.zkoss.zul.ListModelList;
import org.zkoss.zul.Textbox;

import com.bl.Ordermanagement.reallocation.BlReallocationService;
import com.bl.core.enums.ItemStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.dao.impl.DefaultBlProductDao;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.core.events.CockpitEventQueue;
import com.hybris.cockpitng.util.DefaultWidgetController;

public class BlReassignSerialController  extends DefaultWidgetController {

	protected static final String IN_SOCKET = "consignmentSerialInput";
	protected static final String OUT_CONFIRM = "confirmSerialOutput";
  protected static final Object COMPLETED = "completed";
  protected static final String ON_SELECT = "onSelect";
  protected static final String CONSIGNMENT_ACTION_EVENT_NAME = "ConsignmentActionEvent";
  protected static final String REALLOCATE_CONSIGNMENT_CHOICE = "reallocateConsignment";

  protected static final String COMPLETE = "completed";
  private ConsignmentModel consignment;
  @Wire
  private Textbox consignmentCode;
  @Wire
  private Textbox customerName;
  @Wire
  private Grid consignmentEntries;
  @Wire
  private Checkbox globalDeclineEntriesSelection;

  @WireVariable
  private transient ModelService modelService;

  @WireVariable
  private transient CockpitEventQueue cockpitEventQueue;

  @WireVariable
  private transient BlReallocationService blReallocationService;

  @Resource
  private DefaultBlProductDao defaultBlProductDao;

  public BlReassignSerialController()
  {
    // constructor
  }

  @SocketEvent(
		  socketId = IN_SOCKET
  )
  public void initReallocationConsignmentForm(final ConsignmentModel inputObject) {

    this.setConsignment(inputObject);
    this.getWidgetInstanceManager().setTitle(this.getWidgetInstanceManager()
			 .getLabel("warehousingbackoffice.reassignserial.title") + " " + this
        .getConsignment().getCode());
    this.consignmentCode.setValue(this.getConsignment().getCode());
    this.customerName.setValue(this.getConsignment().getOrder().getUser().getDisplayName());
	 final List<ConsignmentEntriesData> entriesList = new ArrayList<ConsignmentEntriesData>();
	 for (final ConsignmentEntryModel entry : inputObject.getConsignmentEntries())
	 {
		 for (final BlProductModel product : entry.getSerialProducts())
		 {
			 if (product instanceof BlSerialProductModel)
			 {
				 final ConsignmentEntriesData dataEntry = new ConsignmentEntriesData();
				 dataEntry.setCurrentSerial(product.getCode());
				 dataEntry.setEntryNumber(entry.getOrderEntry().getEntryNumber());
				 dataEntry.setProductCode(entry.getOrderEntry().getProduct().getCode());
				 dataEntry.setProductName(entry.getOrderEntry().getProduct().getName());
				 entriesList.add(dataEntry);
			 }
		 }
	 }
	 this.getConsignmentEntries().setModel(new ListModelList<>(entriesList));
    this.getConsignmentEntries().renderAll();
  }

  @ViewEvent(
		  componentID = "confirmreassignserial",
      eventName = "onClick"
  )
  public void confirmReallocation() throws InterruptedException {

	 final ConsignmentModel consignment = this.consignment;
	 boolean checkedEntries = false;
	 if (this.globalDeclineEntriesSelection.isChecked())
	 {
		 this.selectAllEntries();
	 }
	 final Iterator<Component> var2 = this.getConsignmentEntries().getRows().getChildren().iterator();
	 while (var2.hasNext())
	 {
      final Component row = var2.next();
      final Component firstComponent = row.getChildren().iterator().next();
      if (firstComponent instanceof Checkbox && ((Checkbox) firstComponent).isChecked()) {
			checkedEntries = true;
			if (((Textbox) row.getChildren().get(4)).getValue() == null || ((Textbox) row.getChildren().get(4)).getValue().isBlank())
			{
				throw new WrongValueException((row.getChildren().get(4)),
						this.getLabel("warehousingbackoffice.reassignserial.validation.missing.barcode"));
			}
      	final String productCode = ((Label) row.getChildren().get(1)).getValue();
			final String oldSerialCode = ((Label) row.getChildren().get(3)).getValue();
			for (final ConsignmentEntryModel consign : consignment.getConsignmentEntries())
			{
				if (consign.getOrderEntry().getProduct().getCode().equals(productCode))
				{
					applyToRow(consign, ((Textbox) row.getChildren().get(4)).getValue(), oldSerialCode, row);
				}
			}
      }
    }
	 if (!checkedEntries)
	{
		throw new WrongValueException(this.globalDeclineEntriesSelection,
				this.getLabel("warehousingbackoffice.reassignserial.decline.validation.missing.selectedLine"));
	}
	this.sendOutput(OUT_CONFIRM, COMPLETED);
  }

  protected void applyToRow(final ConsignmentEntryModel entry, final String barCode, final String oldSerialCode,
		  final Component row)
  {
	  final BlSerialProductModel serial = this.getDefaultBlProductDao().getSerialByBarcode(barCode);
	  final List<BlProductModel> productEntries = new ArrayList<BlProductModel>();
	  productEntries.addAll(entry.getSerialProducts());
	  final Map<String, ItemStatusEnum> newItems = new HashMap<String, ItemStatusEnum>();
	  newItems.putAll(entry.getItems());
	  for (final BlProductModel productEntry : entry.getSerialProducts())
	  {
		  if (productEntry instanceof BlSerialProductModel && productEntry.getCode().equals(oldSerialCode))
		  {
			  final BlSerialProductModel serialProduct = (BlSerialProductModel) productEntry;
			  if (serial != null && serialProduct.getBlProduct().equals(serial.getBlProduct()))
			  {
				  if (entry.getItems().containsKey(serialProduct.getCode()))
				  {
					  newItems.remove(serialProduct.getCode());
					  newItems.put(serial.getCode(), entry.getItems().get(serialProduct.getCode()));
				  }
				  productEntries.remove(productEntry);
				  productEntries.add(serial);
			  }
			  else
			  {
				  throw new WrongValueException((row.getChildren().get(4)),
						  this.getLabel("warehousingbackoffice.reassignserial.validation.incorrect.barcode"));
			  }
			  entry.getOrderEntry().setSerialProducts(productEntries);
			  entry.setSerialProducts(productEntries);
			  entry.setItems(newItems);
			  getModelService().save(entry.getOrderEntry());
			  getModelService().save(entry);
		  }
	  }
  }

  protected void selectAllEntries()
  {
	  final Iterator<Component> var2 = this.consignmentEntries.getRows().getChildren().iterator();
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

  @ViewEvent(
		  componentID = "undoreassignserial",
      eventName = "onClick"
  )
  public void reset() {
    this.initReallocationConsignmentForm(this.getConsignment());
  }

  protected ConsignmentModel getConsignment() {
    return this.consignment;
  }

  public void setConsignment(final ConsignmentModel consignment) {
    this.consignment = consignment;
  }

  protected Grid getConsignmentEntries() {
    return this.consignmentEntries;
  }

  protected ModelService getModelService() {
    return this.modelService;
  }

  protected CockpitEventQueue getCockpitEventQueue() {
    return this.cockpitEventQueue;
  }

  public BlReallocationService getBlReallocationService() {
    return blReallocationService;
  }

  public void setBlReallocationService(
      final BlReallocationService blReallocationService) {
    this.blReallocationService = blReallocationService;
  }

  public DefaultBlProductDao getDefaultBlProductDao()
  {
	  return defaultBlProductDao;
  }

  public void setDefaultBlProductDao(final DefaultBlProductDao defaultBlProductDao)
  {
	  this.defaultBlProductDao = defaultBlProductDao;
  }

}
