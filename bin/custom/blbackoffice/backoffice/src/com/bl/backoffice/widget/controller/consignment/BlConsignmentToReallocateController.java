package com.bl.backoffice.widget.controller.consignment;

import com.bl.Ordermanagement.reallocation.BlReallocationService;
import com.bl.core.enums.ConsignmentEntryStatusEnum;
import com.bl.core.enums.ItemStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.utils.BlDateTimeUtils;
import com.google.common.collect.Sets;
import com.hybris.backoffice.i18n.BackofficeLocaleService;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.core.events.CockpitEventQueue;
import com.hybris.cockpitng.util.DefaultWidgetController;
import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.enumeration.EnumerationService;
import de.hybris.platform.ordersplitting.WarehouseService;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.ConsignmentProcessModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.processengine.model.BusinessProcessParameterModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.util.localization.Localization;
import de.hybris.platform.warehousing.data.allocation.DeclineEntries;
import de.hybris.platform.warehousing.data.allocation.DeclineEntry;
import de.hybris.platform.warehousing.data.sourcing.SourcingContext;
import de.hybris.platform.warehousing.enums.DeclineReason;
import de.hybris.platform.warehousing.process.WarehousingBusinessProcessService;
import de.hybris.platform.warehousingbackoffice.dtos.ConsignmentEntryToReallocateDto;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.zkoss.zk.ui.Component;
import org.zkoss.zk.ui.WrongValueException;
import org.zkoss.zk.ui.event.Event;
import org.zkoss.zk.ui.event.Events;
import org.zkoss.zk.ui.event.InputEvent;
import org.zkoss.zk.ui.event.SelectEvent;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zk.ui.select.annotation.WireVariable;
import org.zkoss.zk.ui.util.Clients;
import org.zkoss.zul.Checkbox;
import org.zkoss.zul.Combobox;
import org.zkoss.zul.Comboitem;
import org.zkoss.zul.Grid;
import org.zkoss.zul.Intbox;
import org.zkoss.zul.Label;
import org.zkoss.zul.ListModelArray;
import org.zkoss.zul.ListModelList;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Row;
import org.zkoss.zul.Textbox;
import org.zkoss.zul.impl.InputElement;

public class BlConsignmentToReallocateController  extends DefaultWidgetController {

  protected static final String IN_SOCKET = "consignmentInput";
  protected static final String OUT_CONFIRM = "confirmOutput";
  protected static final Object COMPLETED = "completed";
  protected static final String ON_SELECT = "onSelect";
  protected static final String CONSIGNMENT_ACTION_EVENT_NAME = "ConsignmentActionEvent";
  protected static final String REALLOCATE_CONSIGNMENT_CHOICE = "reallocateConsignment";

  protected static final String COMPLETE = "completed";
  private static final String MESSAGE_BOX_TITLE = "consignment.reallocate.message.box.title";
  private static final String ERR_MESG_FOR_CONSIGNMENT_SHIPPED = "error.message.consignment.reallocate.shipped";

  protected static final String DECLINE_ENTRIES = "declineEntries";
  protected static final int COLUMN_INDEX_REALLOCATION_QUANTITY = 4;
  protected static final int COLUMN_INDEX_REALLOCATION_REASON = 5;
  protected static final int COLUMN_INDEX_REALLOCATION_LOCATION = 6;
  protected static final int COLUMN_INDEX_REALLOCATION_COMMENT = 7;
  private transient Set<ConsignmentEntryToReallocateDto> consignmentsEntriesToReallocate;
  private ConsignmentModel consignment;
  @Wire
  private Textbox consignmentCode;
  @Wire
  private Textbox customerName;
  @Wire
  private Combobox globalDeclineReasons;
  @Wire
  private Textbox globalDeclineComment;
  @Wire
  private Grid consignmentEntries;
  @Wire
  private Combobox globalPossibleLocations;
  @Wire
  private Checkbox globalDeclineEntriesSelection;
  private final List<String> declineReasons = new ArrayList<>();

  private final Set<WarehouseModel> locations = Sets.newHashSet();

  @WireVariable
  private transient WarehouseService warehouseService;
  @WireVariable
  private transient EnumerationService enumerationService;
  @WireVariable
  private transient WarehousingBusinessProcessService<ConsignmentModel> consignmentBusinessProcessService;
  @WireVariable
  private transient ModelService modelService;
  @WireVariable
  private transient BackofficeLocaleService cockpitLocaleService;
  @WireVariable
  private transient CockpitEventQueue cockpitEventQueue;

  @WireVariable
  private transient BlReallocationService blReallocationService;

  @WireVariable
  private transient BlCommerceStockService blCommerceStockService;

  public BlConsignmentToReallocateController()
  {
    // constructor
  }

  @SocketEvent(
      socketId = "consignmentInput"
  )
  public void initReallocationConsignmentForm(final ConsignmentModel inputObject) {

    this.declineReasons.clear();
    this.locations.clear();
    this.globalDeclineEntriesSelection.setChecked(false);
    this.setConsignment(inputObject);
    this.getWidgetInstanceManager().setTitle(this.getWidgetInstanceManager()
        .getLabel("warehousingbackoffice.reallocationconsignment.title") + " " + this
        .getConsignment().getCode());
    this.consignmentCode.setValue(this.getConsignment().getCode());
    this.customerName.setValue(this.getConsignment().getOrder().getUser().getDisplayName());
    final Locale locale = this.getCockpitLocaleService().getCurrentLocale();
    this.getEnumerationService().getEnumerationValues(DeclineReason.class).stream()
        .filter(reason -> !reason.equals(DeclineReason.ASNCANCELLATION)).forEach(reason ->
        this.declineReasons.add(this.getEnumerationService().getEnumerationName(reason, locale))
    );

    this.locations.addAll(inputObject.getOrder().getStore().getWarehouses());
    if (this.locations.contains(this.getConsignment().getWarehouse())) {
      this.locations.remove(this.getConsignment().getWarehouse());
    }

    this.globalDeclineReasons.setModel(new ListModelArray<>(this.declineReasons));
    this.globalPossibleLocations.setModel(new ListModelArray<>(this.locations.toArray()));
    this.consignmentsEntriesToReallocate = new HashSet<>();
    if(!this.getConsignment().getStatus().equals(ConsignmentStatus.BL_SHIPPED)) {
      this.getConsignment().getConsignmentEntries().stream()
          .filter(entry -> entry.getQuantityPending() > 0L).forEach(entry ->
          this.consignmentsEntriesToReallocate
              .add(new ConsignmentEntryToReallocateDto(entry, this.declineReasons, this.locations)));
    }
    this.getConsignmentEntries().setModel(new ListModelList<>(this.consignmentsEntriesToReallocate));
    this.getConsignmentEntries().renderAll();
    this.addListeners();
  }

  @ViewEvent(
      componentID = "confirmreallocation",
      eventName = "onClick"
  )
  public void confirmReallocation() throws InterruptedException {

    // Show error message if other warehouse consignment of the same order is already shipped.
    final Set<ConsignmentModel> otherConsignments = new HashSet<>();
    otherConsignments.addAll(this.consignment.getOrder().getConsignments());

    if (CollectionUtils.isNotEmpty(otherConsignments) && otherConsignments.size() > 1) {
      otherConsignments.remove(this.consignment);

      otherConsignments.stream().forEach(consignmentModel -> {
        if (consignmentModel.getStatus().equals(ConsignmentStatus.BL_SHIPPED)) {
          showMessageBox(Localization.getLocalizedString(ERR_MESG_FOR_CONSIGNMENT_SHIPPED), true);
          return;
        }
      });
    }

    this.validateRequest();

    final List<AbstractOrderEntryModel> orderEntries = new ArrayList<>();
    WarehouseModel selectedWH = null;
    OrderModel orderModel = null;
    final ListModelList<ConsignmentEntryToReallocateDto> allModelList = (ListModelList) this
        .getConsignmentEntries().getModel();
    final ListModelList<ConsignmentEntryToReallocateDto> selectedModelList = new ListModelList<>();

    final List<AtomicBoolean> allEntryAllQuantityReallocated = new ArrayList<>();

    for (final ConsignmentEntryToReallocateDto entryDto : allModelList) {
      if (entryDto.getQuantityToReallocate() > 0L) {

        final AbstractOrderEntryModel orderEntryModel = entryDto.getConsignmentEntry().getOrderEntry();
        orderEntryModel.setQuantity(entryDto.getQuantityToReallocate());
        orderEntryModel.setUnAllocatedQuantity(entryDto.getQuantityToReallocate());
        orderEntries.add(orderEntryModel);
        selectedWH = entryDto.getSelectedLocation();
        orderModel = (OrderModel) orderEntryModel.getOrder();
        selectedModelList.add(entryDto);

        if (entryDto.getQuantityToReallocate()
            .equals(entryDto.getConsignmentEntry().getQuantity())) {
          allEntryAllQuantityReallocated.add(new AtomicBoolean(true));
        } else {
          allEntryAllQuantityReallocated.add(new AtomicBoolean(false));
        }

      }
    }

    final Set<String> productCodes = new HashSet<>();
    orderEntries.forEach(orderEntry -> productCodes.add(orderEntry.getProduct().getCode()));

    final Collection<StockLevelModel> stockLevels = null != orderModel ? blCommerceStockService
        .getStockForProductCodesAndDate(productCodes, selectedWH, orderModel.getActualRentalStartDate(),
            orderModel.getActualRentalEndDate()) : new ArrayList<>();

    final Map<String, List<StockLevelModel>> availabilityMap = getBlCommerceStockService()
        .groupBySkuProductWithAvailability(stockLevels);

    final SourcingContext context = blReallocationService.createSourcingContext(orderEntries);
    blReallocationService.createSourcingLocation(availabilityMap, selectedWH, context);

    blReallocationService.assignSerialFromLocation(context);

    blReallocationService.createConsignment(orderModel, context, selectedWH);

    updateCurrentConsignmentStatus(this.consignment, allEntryAllQuantityReallocated, orderEntries);
    final List<ConsignmentEntryModel> entryModels = new ArrayList<>();
    orderModel.getEntries().forEach(entryModel -> {
      entryModel.getConsignmentEntries().forEach(consEntryModel -> {
        if (Objects.isNull(consEntryModel.getConsignment())) {
          entryModels.add(consEntryModel);
        }
      });
    });
    modelService.removeAll(entryModels);
    this.sendOutput(OUT_CONFIRM, COMPLETED);
  }

  /**
   * Mark the consignment status to CANCELLED, if all the entry and their quantities are
   * re-allocated
   *
   * @param consignment     the consignment
   * @param allEntryAllQuantityReallocated the boolean values for each entry
   */
   private void updateCurrentConsignmentStatus(final ConsignmentModel consignment,
   		  final List<AtomicBoolean> allEntryAllQuantityReallocated, final List<AbstractOrderEntryModel> orderEntries)
     {
		  final List<String> serialsCodesToRemove = new ArrayList<String>();
       if (allEntryAllQuantityReallocated.stream().allMatch(AtomicBoolean::get)) {
   		  if (consignment.getConsignmentEntries().size() > 1 && consignment.getConsignmentEntries().size() != orderEntries.size())
   		  {
   			  final Set<ConsignmentEntryModel> entriesList = new HashSet<ConsignmentEntryModel>();
      			entriesList.addAll(consignment.getConsignmentEntries());
					for (final ConsignmentEntryModel consignmentEntry : consignment.getConsignmentEntries())
   			  {
   				  for (final AbstractOrderEntryModel entry : orderEntries)
   				  {
						  if (consignmentEntry.getOrderEntry().equals(entry))
   					  {
							  entriesList.remove(consignmentEntry);
							  for (final BlProductModel product : consignmentEntry.getSerialProducts())
							  {
								  if (product instanceof BlSerialProductModel)
								  {
									  serialsCodesToRemove.add(product.getCode());
								  }
							  }
   					  }
   				  }
   			  }
   			  if (entriesList.isEmpty())
   			  {
   				  consignment.setStatus(ConsignmentStatus.CANCELLED);
   			  }
   			  consignment.setConsignmentEntries(entriesList);
   			  modelService.save(consignment);
   			  modelService.refresh(consignment);
   		  }
   		  else
   		  {
				  final Set<ConsignmentEntryModel> entriesList = new HashSet<ConsignmentEntryModel>();
				  entriesList.addAll(consignment.getConsignmentEntries());
				  for (final ConsignmentEntryModel consignmentEntry : consignment.getConsignmentEntries())
				  {
					  final List<BlProductModel> serialProducts = new ArrayList<BlProductModel>();
					  serialProducts.addAll(consignmentEntry.getSerialProducts());
					  for (final BlProductModel product : consignmentEntry.getSerialProducts())
					  {
						  if (product instanceof BlSerialProductModel)
						  {
							  serialProducts.remove(product);
							  serialsCodesToRemove.add(product.getCode());
						  }
					  }
					  consignmentEntry.setSerialProducts(serialProducts);
					  if (serialProducts.isEmpty()
							  || !serialProducts.stream().anyMatch(serialProduct -> serialProduct instanceof BlSerialProductModel))
					  {
						  entriesList.remove(consignmentEntry);
						  consignmentEntry.setQuantity(0l);
					  }
					  modelService.save(consignmentEntry);
				  }
   			  consignment.setStatus(ConsignmentStatus.CANCELLED);
				  consignment.setConsignmentEntries(entriesList);
   			  modelService.save(consignment);
   			  modelService.refresh(consignment);
   		  }
       }
		 else if (!orderEntries.isEmpty())
		 {
			 final List<BlProductModel> serials = new ArrayList<BlProductModel>();
			 final Map<String, ItemStatusEnum> items = new HashMap<String, ItemStatusEnum>();
			 for (final ConsignmentEntryModel consignmentEntry : consignment.getConsignmentEntries())
			 {
				 serials.addAll(consignmentEntry.getSerialProducts());
				 items.putAll(consignmentEntry.getItems());
				 for (final AbstractOrderEntryModel orderEntry : orderEntries)
				 {
					 if (consignmentEntry.getOrderEntry().equals(orderEntry))
					 {
						 consignmentEntry.setQuantity(consignmentEntry.getQuantity() - orderEntry.getQuantity());
						 int counter = 0;
						 for (final BlProductModel serial : consignmentEntry.getSerialProducts())
						 {
							 if (serial instanceof BlSerialProductModel)
							 {
								 items.remove(serial.getCode());
								 serialsCodesToRemove.add(serial.getCode());
								 serials.remove(serial);
								 counter++;
								 if (counter == orderEntry.getQuantity().intValue())
								 {
									 consignmentEntry.setItems(items);
									 consignmentEntry.setSerialProducts(serials);
									 modelService.save(consignmentEntry);
									 modelService.refresh(consignmentEntry);
									 break;
								 }
							 }
						 }
					 }
				 }
			 }
		 }
		 blReallocationService.removeReserveStocksForSerialProducts(new HashSet<>(serialsCodesToRemove),
				 consignment.getOptimizedShippingStartDate(), consignment.getOptimizedShippingEndDate(), Boolean.TRUE,
				 consignment.getWarehouse());
     }

  /**
   * Show message box.
   *
   * @param message     the message
   * @param isErrorMesg the is error mesg
   */
  protected void showMessageBox(final String message, final boolean isErrorMesg) {
    if (isErrorMesg) {
      Messagebox
          .show(message, Localization.getLocalizedString(MESSAGE_BOX_TITLE), Messagebox.OK,
              Messagebox.ERROR);
    } else {
      Messagebox
          .show(message, Localization.getLocalizedString(MESSAGE_BOX_TITLE), Messagebox.OK,
              Messagebox.INFORMATION);
    }
    this.sendOutput(OUT_CONFIRM, COMPLETE);
  }

  protected void createDeclineEntry(final Collection<DeclineEntry> entriesToReallocate, final Component component) {
    final ConsignmentEntryToReallocateDto consignmentEntryToReallocate = (ConsignmentEntryToReallocateDto)((Row)component).getValue();
    final Long qtyToReallocate = consignmentEntryToReallocate.getQuantityToReallocate();
    final Long qtyAvailableForReallocation = consignmentEntryToReallocate.getConsignmentEntry().getQuantityPending();
    if (qtyToReallocate > 0L && qtyToReallocate <= qtyAvailableForReallocation) {
      final DeclineEntry newEntry = new DeclineEntry();
      newEntry.setQuantity(qtyToReallocate);
      newEntry.setConsignmentEntry(consignmentEntryToReallocate.getConsignmentEntry());
      newEntry.setNotes(consignmentEntryToReallocate.getDeclineConsignmentEntryComment());
      newEntry.setReallocationWarehouse(consignmentEntryToReallocate.getSelectedLocation());
      newEntry.setReason(consignmentEntryToReallocate.getSelectedReason());
      entriesToReallocate.add(newEntry);
    }

  }

  @ViewEvent(
      componentID = "undoreallocation",
      eventName = "onClick"
  )
  public void reset() {
    this.globalDeclineReasons.setSelectedItem((Comboitem)null);
    this.globalPossibleLocations.setSelectedItem((Comboitem)null);
    this.globalDeclineComment.setValue("");
    this.initReallocationConsignmentForm(this.getConsignment());
  }

  protected void addListeners() {
    final List<Component> rows = this.consignmentEntries.getRows().getChildren();
    final Iterator<Component> var3 = rows.iterator();

    while(var3.hasNext()) {
      final Component row = var3.next();
      final Iterator<Component> var5 = row.getChildren().iterator();

      while(var5.hasNext()) {
        final Component myComponent = var5.next();
        if (myComponent instanceof Checkbox) {
          myComponent.addEventListener("onCheck", event ->
              this.handleRow((Row) event.getTarget().getParent()));
        } else if (myComponent instanceof Combobox) {
          myComponent.addEventListener(ON_SELECT, this::handleIndividualLocation);
          myComponent.addEventListener("onCustomChange", event ->
              Events.echoEvent("onLaterCustomChange", myComponent, event.getData()));
          myComponent.addEventListener("onLaterCustomChange", event -> {
            Clients.clearWrongValue(myComponent);
            myComponent.invalidate();
            this.handleIndividualReason(event);
          });
        } else if (myComponent instanceof Intbox) {
          myComponent.addEventListener("onChange", event -> {
            this.autoSelect(event);
            ((ConsignmentEntryToReallocateDto)((Row)event.getTarget().getParent()).getValue()).setQuantityToReallocate(Long.parseLong(((InputEvent)event).getValue()));
          });
        } else if (myComponent instanceof Textbox) {
          myComponent.addEventListener("onChanging", event -> {
            this.autoSelect(event);
            ((ConsignmentEntryToReallocateDto)((Row)event.getTarget().getParent()).getValue()).setDeclineConsignmentEntryComment(((InputEvent)event).getValue());
          });
        }
      }
    }

    this.globalDeclineReasons.addEventListener(ON_SELECT, this::handleGlobalReason);
    this.globalPossibleLocations.addEventListener(ON_SELECT, this::handleGlobalLocation);
    this.globalDeclineComment.addEventListener("onChanging", this::handleGlobalComment);
    this.globalDeclineEntriesSelection
        .addEventListener("onCheck", event -> this.selectAllEntries());
  }

  protected void applyToGrid(final Object data, final int childrenIndex) {
    this.consignmentEntries.getRows().getChildren().stream().filter(entry ->
        ((Checkbox) entry.getChildren().iterator().next()).isChecked()).forEach(entry ->
        this.applyToRow(data, childrenIndex, entry));
  }

  protected void applyToRow(final Object data, final int childrenIndex, final Component row) {
    int index = 0;

    for(final Iterator<Component> var6 = row.getChildren().iterator(); var6.hasNext(); ++index) {
      final Component myComponent = var6.next();
      if (index == childrenIndex) {
        this.applyToCheckboxRow(data, myComponent);
        this.applyToComboboxRow(data, myComponent);
        if (myComponent instanceof Intbox) {
          ((Intbox)myComponent).setValue((Integer)data);
        } else if (!(myComponent instanceof Combobox) && myComponent instanceof Textbox) {
          ((Textbox)myComponent).setValue((String)data);
        }
      }
    }

  }

  protected void applyToComboboxRow(final Object data, final Component component) {
    if (component instanceof Combobox) {
      if (data == null) {
        ((Combobox)component).setSelectedItem((Comboitem)null);
      } else {
        ((Combobox)component).setSelectedIndex((Integer)data);
      }
    }

  }

  protected void applyToCheckboxRow(final Object data, final Component component) {
    if (component instanceof Checkbox) {
      if (data == null) {
        ((Checkbox)component).setChecked(Boolean.FALSE);
      } else {
        ((Checkbox)component).setChecked((Boolean)data);
      }
    }

  }

  protected void autoSelect(final Event event) {
    ((Checkbox)event.getTarget().getParent().getChildren().iterator().next()).setChecked(true);
  }

  protected void buildDeclineParam(final ConsignmentProcessModel processModel, final Collection<DeclineEntry> entriesToReallocate) {
    this.cleanDeclineParam(processModel);
    final Collection<BusinessProcessParameterModel> contextParams = new ArrayList<>();
    contextParams.addAll(processModel.getContextParameters());
    final DeclineEntries declinedEntries = new DeclineEntries();
    declinedEntries.setEntries(entriesToReallocate);
    final BusinessProcessParameterModel declineParam = new BusinessProcessParameterModel();
    declineParam.setName(DECLINE_ENTRIES);
    declineParam.setValue(declinedEntries);
    declineParam.setProcess(processModel);
    contextParams.add(declineParam);
    processModel.setContextParameters(contextParams);
    this.getModelService().save(processModel);
  }

  protected void cleanDeclineParam(final ConsignmentProcessModel processModel) {
    final Collection<BusinessProcessParameterModel> contextParams = new ArrayList<>();
    contextParams.addAll(processModel.getContextParameters());
    if (CollectionUtils.isNotEmpty(contextParams)) {
      final Optional<BusinessProcessParameterModel> declineEntriesParamOptional = contextParams.stream()
          .filter(param -> param.getName().equals(DECLINE_ENTRIES)).findFirst();
      if (declineEntriesParamOptional.isPresent()) {
        final BusinessProcessParameterModel declineEntriesParam = declineEntriesParamOptional.get();
        contextParams.remove(declineEntriesParam);
        this.getModelService().remove(declineEntriesParam);
        processModel.setContextParameters(contextParams);
        this.getModelService().save(processModel);
      }
    }

  }

  protected int getLocationIndex(final WarehouseModel location) {
    int index = 0;

    for(final Iterator<WarehouseModel> var4 = this.locations.iterator(); var4.hasNext(); ++index) {
      final WarehouseModel warehouseModel = var4.next();
      if (location.getCode().equals(warehouseModel.getCode())) {
        break;
      }
    }

    return index;
  }

  protected int getReasonIndex(final DeclineReason declineReason) {
    int index = 0;
    final String myReason = this.getEnumerationService().getEnumerationName(declineReason, this.getCockpitLocaleService().getCurrentLocale());

    for(final Iterator<String> var5 = this.declineReasons.iterator(); var5.hasNext(); ++index) {
      final String reason = var5.next();
      if (myReason.equals(reason)) {
        break;
      }
    }

    return index;
  }

  protected Optional<DeclineReason> getSelectedDeclineReason(final Event event) {
    Optional<DeclineReason> result = Optional.empty();
    if (!((SelectEvent)event).getSelectedItems().isEmpty()) {
      final Object selectedValue = ((Comboitem)((SelectEvent)event).getSelectedItems().iterator().next()).getValue();
      result = this.matchingComboboxDeclineReason(selectedValue.toString());
    }

    return result;
  }

  protected Optional<DeclineReason> getCustomSelectedDeclineReason(final Event event) {
    Optional<DeclineReason> reason = Optional.empty();
    if (event.getTarget() instanceof Combobox) {
      final Object selectedValue = event.getData();
      reason = this.matchingComboboxDeclineReason(selectedValue.toString());
    }

    return reason;
  }

  protected WarehouseModel getSelectedLocation(final Event event) {
    WarehouseModel result = null;
    if (!((SelectEvent)event).getSelectedItems().isEmpty()) {
      result = (WarehouseModel)((Comboitem)((SelectEvent)event).getSelectedItems().iterator().next()).getValue();
    }

    return result;
  }

  protected void handleGlobalComment(final Event event) {
    this.applyToGrid(((InputEvent)event).getValue(), 7);
    this.consignmentEntries.getRows().getChildren().stream().filter(entry ->
        ((Checkbox) entry.getChildren().iterator().next()).isChecked()).forEach(entry ->
        ((ConsignmentEntryToReallocateDto) ((Row) entry).getValue())
            .setDeclineConsignmentEntryComment(((InputEvent) event).getValue()));
  }

  protected void handleGlobalLocation(final Event event) {
    final WarehouseModel selectedLocation = this.getSelectedLocation(event);
    if (selectedLocation != null) {
      this.applyToGrid(this.getLocationIndex(selectedLocation), 6);
      this.consignmentEntries.getRows().getChildren().stream().filter(entry ->
          ((Checkbox) entry.getChildren().iterator().next()).isChecked()).forEach(entry ->
          ((ConsignmentEntryToReallocateDto) ((Row) entry).getValue())
              .setSelectedLocation(selectedLocation));
    }

  }

  protected void handleGlobalReason(final Event event) {
    final Optional<DeclineReason> declineReason = this.getSelectedDeclineReason(event);
    if (declineReason.isPresent()) {
      this.applyToGrid(this.getReasonIndex(declineReason.get()), 5);
      this.consignmentEntries.getRows().getChildren().stream().filter(entry ->
          ((Checkbox) entry.getChildren().iterator().next()).isChecked()).forEach(entry ->
          ((ConsignmentEntryToReallocateDto) ((Row) entry).getValue())
              .setSelectedReason(declineReason.get()));
    }

  }

  protected void handleIndividualReason(final Event event) {
    final Optional<DeclineReason> declineReason = this.getCustomSelectedDeclineReason(event);
    if (declineReason.isPresent()) {
      this.autoSelect(event);
      ((ConsignmentEntryToReallocateDto)((Row)event.getTarget().getParent()).getValue()).setSelectedReason(declineReason.get());
    }

  }

  protected void handleIndividualLocation(final Event event) {
    if (!((SelectEvent) event).getSelectedItems().isEmpty()) {
      this.autoSelect(event);
      final Object selectedValue = ((Comboitem) ((SelectEvent) event).getSelectedItems().iterator()
          .next()).getValue();
      if (selectedValue instanceof WarehouseModel) {
        ((ConsignmentEntryToReallocateDto) ((Row) event.getTarget().getParent()).getValue())
            .setSelectedLocation((WarehouseModel) selectedValue);
      }
    }

  }

  protected void handleRow(final Row row) {
    final ConsignmentEntryToReallocateDto myEntry = (ConsignmentEntryToReallocateDto) row.getValue();
    if (row.getChildren().iterator().next() instanceof Checkbox) {
      if (!((Checkbox) row.getChildren().iterator().next()).isChecked()) {
        this.applyToRow(0, 4, row);
        this.applyToRow((Object) null, 5, row);
        this.applyToRow((Object) null, 6, row);
        this.applyToRow((Object) null, 7, row);
        myEntry.setQuantityToReallocate(0L);
        myEntry.setSelectedReason((DeclineReason) null);
        myEntry.setSelectedLocation((WarehouseModel) null);
        myEntry.setDeclineConsignmentEntryComment((String) null);
      } else {
        this.applyToRow(this.globalDeclineReasons.getSelectedIndex(), 5, row);
        this.applyToRow(this.globalPossibleLocations.getSelectedIndex(), 6, row);
        this.applyToRow(this.globalDeclineComment.getValue(), 7, row);
        final Optional<DeclineReason> reason = this.matchingComboboxDeclineReason(
            this.globalDeclineReasons.getSelectedItem() != null ? this.globalDeclineReasons
                .getSelectedItem().getLabel() : null);
        myEntry.setSelectedReason(reason.orElse(null));
        myEntry.setSelectedLocation(this.globalPossibleLocations.getSelectedItem() != null
            ? (WarehouseModel) this.globalPossibleLocations.getSelectedItem().getValue() : null);
        myEntry.setDeclineConsignmentEntryComment(this.globalDeclineComment.getValue());
      }
    }

  }

  protected boolean isDeclineProcessDone(final ConsignmentModel latestConsignmentModel,
      final Collection<DeclineEntry> entriesToReallocate) {
    return entriesToReallocate.stream().allMatch(entry ->
        this.isDeclinedQuantityCorrect(latestConsignmentModel, entry));
  }

  protected boolean isDeclinedQuantityCorrect(final ConsignmentModel latestConsignmentModel,
      final DeclineEntry declineEntry) {
    final Long expectedDeclinedQuantity =
        declineEntry.getConsignmentEntry().getQuantityDeclined() + declineEntry.getQuantity();
    return latestConsignmentModel.getConsignmentEntries().stream().anyMatch(entry ->
        entry.getPk().equals(declineEntry.getConsignmentEntry().getPk())
            && expectedDeclinedQuantity.equals(entry.getQuantityDeclined()));
  }

  protected Optional<DeclineReason> matchingComboboxDeclineReason(final String declineReasonLabel) {
    return this.getEnumerationService().getEnumerationValues(DeclineReason.class).stream()
        .filter(reason -> this.getEnumerationService()
            .getEnumerationName(reason, this.getCockpitLocaleService().getCurrentLocale())
            .equals(declineReasonLabel)).findFirst();
  }

  protected void selectAllEntries() {
    this.applyToGrid(Boolean.TRUE, 0);
    final Iterator<Component> var2 = this.consignmentEntries.getRows().getChildren().iterator();

    while (var2.hasNext()) {
      final Component row = var2.next();
      final Component firstComponent = row.getChildren().iterator().next();
      if (firstComponent instanceof Checkbox) {
        ((Checkbox) firstComponent).setChecked(this.globalDeclineEntriesSelection.isChecked());
      }

      this.handleRow((Row) row);
      if (this.globalDeclineEntriesSelection.isChecked()) {
        final int reallocatableQuantity = Integer.parseInt(((Label) row.getChildren().get(3)).getValue());
        this.applyToRow(reallocatableQuantity, 4, row);
      }
    }

    if (this.globalDeclineEntriesSelection.isChecked()) {
      this.consignmentsEntriesToReallocate.stream().forEach(entry ->
          entry.setQuantityToReallocate(entry.getConsignmentEntry().getQuantityPending()));
    }

  }

  protected Component targetFieldToApplyValidation(final String stringToValidate, final int indexLabelToCheck,
      final int indexTargetComponent) {
    final Iterator<Component> var5 = this.consignmentEntries.getRows().getChildren().iterator();

    while (var5.hasNext()) {
      final Component component = var5.next();
      final Label label = (Label) component.getChildren().get(indexLabelToCheck);
      if (label.getValue().equals(stringToValidate)) {
        return component.getChildren().get(indexTargetComponent);
      }
    }

    return null;
  }

  /**
   * It validate consignment entry which needs to be re-allocated
   * @param entry the ConsignmentEntryToReallocateDto
   */
  protected void validateConsignmentEntry(final ConsignmentEntryToReallocateDto entry) {

    InputElement quantity;
    if (entry.getQuantityToReallocate() > entry.getConsignmentEntry().getQuantityPending()) {

      quantity = (InputElement) this.targetFieldToApplyValidation(
          entry.getConsignmentEntry().getOrderEntry().getProduct().getCode(), 1, 4);
      throw new WrongValueException(quantity, this.getLabel(
          "warehousingbackoffice.reallocationconsignment.decline.validation.invalid.quantity"));
    } else if (entry.getSelectedReason() != null && entry.getQuantityToReallocate() == 0L) {

      quantity = (InputElement) this.targetFieldToApplyValidation(
          entry.getConsignmentEntry().getOrderEntry().getProduct().getCode(), 1, 4);
      throw new WrongValueException(quantity, this.getLabel(
          "warehousingbackoffice.reallocationconsignment.decline.validation.missing.quantity"));
    } else {

      Combobox location;
      if (entry.getSelectedReason() == null && entry.getQuantityToReallocate() > 0L) {

        location = (Combobox) this.targetFieldToApplyValidation(
            entry.getConsignmentEntry().getOrderEntry().getProduct().getCode(), 1, 5);
        throw new WrongValueException(location, this.getLabel(
            "warehousingbackoffice.reallocationconsignment.decline.validation.missing.reason"));
      } else if (entry.getQuantityToReallocate() > 0L && entry.getSelectedLocation() == null) {

        location = (Combobox) this.targetFieldToApplyValidation(
            entry.getConsignmentEntry().getOrderEntry().getProduct().getCode(), 1, 6);
        throw new WrongValueException(location, this.getLabel(
            "warehousingbackoffice.reallocationconsignment.decline.validation.missing.location"));
      } else if (entry.getSelectedLocation() != null && isStockNotAvailable(
          entry.getConsignmentEntry(), entry.getSelectedLocation())) {

        location = (Combobox) this.targetFieldToApplyValidation(
            entry.getConsignmentEntry().getOrderEntry().getProduct().getCode(), 1, 6);
        throw new WrongValueException(location, this.getLabel(
            "warehousingbackoffice.reallocationconsignment.decline.validation.invalid.stockLevel"));
      }
    }
  }

  /**
   * It checks the stock availability for the product
   * @param consignmentEntry the ConsignmentEntryModel
   * @param selectedLocation the WarehouseModel
   * @return true if no stock available
   */
  private boolean isStockNotAvailable(final ConsignmentEntryModel consignmentEntry,
      final WarehouseModel selectedLocation) {

    final Set<String> productCodes = new HashSet<>();
    productCodes.add(consignmentEntry.getOrderEntry().getProduct().getCode());

    final OrderModel orderModel = (OrderModel) consignmentEntry.getOrderEntry().getOrder();

    final Collection<StockLevelModel> stockLevels = blCommerceStockService
        .getStockForProductCodesAndDate(productCodes,
            selectedLocation, orderModel.getActualRentalStartDate(),
            orderModel.getActualRentalEndDate());

    if (CollectionUtils.isNotEmpty(stockLevels)) {

      final Map<Object, List<StockLevelModel>> stockLevelsDatewise = stockLevels.stream()
          .collect(Collectors.groupingBy(StockLevelModel::getDate));

      final LocalDateTime rentalStartDate = BlDateTimeUtils
          .getFormattedDateTime(orderModel.getActualRentalStartDate());
      final LocalDateTime rentalEndDate = BlDateTimeUtils
          .getFormattedDateTime(orderModel.getActualRentalEndDate());
      final long stayDuration = ChronoUnit.DAYS.between(rentalStartDate, rentalEndDate.plusDays(1));

      final Set<Object> datesPresentInStockTable = stockLevelsDatewise.keySet();
      //This is to check whether stock for any particular day is missing in inventory table

        return datesPresentInStockTable.size() != stayDuration;

    }

    return true;
  }

  protected void validateRequest() {
    final Iterator<Component> var2 = this.getConsignmentEntries().getRows().getChildren().iterator();

    while (var2.hasNext()) {
      final Component row = var2.next();
      final Component firstComponent = row.getChildren().iterator().next();
      if (firstComponent instanceof Checkbox && ((Checkbox) firstComponent).isChecked()) {
        final InputElement returnQty = (InputElement) row.getChildren().get(4);
        if (returnQty.getRawValue().equals(0)) {
          throw new WrongValueException(returnQty, this.getLabel(
              "warehousingbackoffice.reallocationconsignment.decline.validation.missing.quantity"));
        }
      }
    }

    final ListModelList<ConsignmentEntryToReallocateDto> modelList = (ListModelList) this
        .getConsignmentEntries().getModel();
    if (modelList.stream().allMatch(entry ->
       entry.getQuantityToReallocate() == 0L)) {
      throw new WrongValueException(this.globalDeclineEntriesSelection, this.getLabel(
          "warehousingbackoffice.reallocationconsignment.decline.validation.missing.selectedLine"));
    } else {
      modelList.forEach(this::validateConsignmentEntry);
    }
  }

  protected ConsignmentModel getConsignment() {
    return this.consignment;
  }

  public void setConsignment(final ConsignmentModel consignment) {
    this.consignment = consignment;
  }

  protected EnumerationService getEnumerationService() {
    return this.enumerationService;
  }

  protected Grid getConsignmentEntries() {
    return this.consignmentEntries;
  }

  protected WarehousingBusinessProcessService<ConsignmentModel> getConsignmentBusinessProcessService() {
    return this.consignmentBusinessProcessService;
  }

  protected ModelService getModelService() {
    return this.modelService;
  }

  protected BackofficeLocaleService getCockpitLocaleService() {
    return this.cockpitLocaleService;
  }

  protected CockpitEventQueue getCockpitEventQueue() {
    return this.cockpitEventQueue;
  }

  public com.bl.core.stock.BlCommerceStockService getBlCommerceStockService() {
    return blCommerceStockService;
  }

  public void setBlCommerceStockService(final BlCommerceStockService blCommerceStockService) {
    this.blCommerceStockService = blCommerceStockService;
  }

  public BlReallocationService getBlReallocationService() {
    return blReallocationService;
  }

  public void setBlReallocationService(
      final BlReallocationService blReallocationService) {
    this.blReallocationService = blReallocationService;
  }
}
