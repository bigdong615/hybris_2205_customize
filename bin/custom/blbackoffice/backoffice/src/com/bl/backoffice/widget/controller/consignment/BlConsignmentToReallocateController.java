package com.bl.backoffice.widget.controller.consignment;

import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.utils.BlDateTimeUtils;
import com.google.common.collect.Sets;
import com.hybris.backoffice.i18n.BackofficeLocaleService;
import com.hybris.backoffice.widgets.notificationarea.NotificationService;
import com.hybris.backoffice.widgets.notificationarea.event.NotificationEvent.Level;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.core.events.CockpitEventQueue;
import com.hybris.cockpitng.core.events.impl.DefaultCockpitEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
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
import de.hybris.platform.warehousing.enums.DeclineReason;
import de.hybris.platform.warehousing.process.WarehousingBusinessProcessService;
import de.hybris.platform.warehousingbackoffice.dtos.ConsignmentEntryToReallocateDto;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
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
  private final List<String> declineReasons = new ArrayList();

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
  private transient BlCommerceStockService blCommerceStockService;

  @WireVariable
  private transient NotificationService notificationService;

  public BlConsignmentToReallocateController() {
  }

  @SocketEvent(
      socketId = "consignmentInput"
  )
  public void initReallocationConsignmentForm(ConsignmentModel inputObject) {

    this.declineReasons.clear();
    this.locations.clear();
    this.globalDeclineEntriesSelection.setChecked(false);
    this.setConsignment(inputObject);
    this.getWidgetInstanceManager().setTitle(this.getWidgetInstanceManager()
        .getLabel("warehousingbackoffice.reallocationconsignment.title") + " " + this
        .getConsignment().getCode());
    this.consignmentCode.setValue(this.getConsignment().getCode());
    this.customerName.setValue(this.getConsignment().getOrder().getUser().getDisplayName());
    Locale locale = this.getCockpitLocaleService().getCurrentLocale();
    this.getEnumerationService().getEnumerationValues(DeclineReason.class).stream()
        .filter((reason) -> {
          return !reason.equals(DeclineReason.ASNCANCELLATION);
        }).forEach((reason) -> {
      this.declineReasons.add(this.getEnumerationService().getEnumerationName(reason, locale));
    });

    this.locations.addAll(inputObject.getOrder().getStore().getWarehouses());
    if (this.locations.contains(this.getConsignment().getWarehouse())) {
      this.locations.remove(this.getConsignment().getWarehouse());
    }

    this.globalDeclineReasons.setModel(new ListModelArray(this.declineReasons));
    this.globalPossibleLocations.setModel(new ListModelArray(this.locations.toArray()));
    this.consignmentsEntriesToReallocate = new HashSet();
    this.getConsignment().getConsignmentEntries().stream().filter((entry) -> {
      return entry.getQuantityPending() > 0L;
    }).forEach((entry) -> {
      this.consignmentsEntriesToReallocate
          .add(new ConsignmentEntryToReallocateDto(entry, this.declineReasons, this.locations));
    });
    this.getConsignmentEntries().setModel(new ListModelList(this.consignmentsEntriesToReallocate));
    this.getConsignmentEntries().renderAll();
    this.addListeners();
  }

  @ViewEvent(
      componentID = "confirmreallocation",
      eventName = "onClick"
  )
  public void confirmReallocation() throws InterruptedException {

    // Show error message if other consignment of the same order is already shipped.
    final Set<ConsignmentModel> allConsignments = this.consignment.getOrder().getConsignments();
    if (CollectionUtils.isNotEmpty(allConsignments) && allConsignments.size() > 1) {
      allConsignments.remove(this.consignment);

      allConsignments.stream().forEach(consignmentModel -> {
        if(consignmentModel.getStatus().equals(ConsignmentStatus.SHIPPED)) {
          showMessageBox(Localization.getLocalizedString(ERR_MESG_FOR_CONSIGNMENT_SHIPPED), true);
          return;
        }
      });
    }

    this.validateRequest();
    String consignmentProcessCode = this.consignment.getCode() + "_ordermanagement";
    Optional<ConsignmentProcessModel> myConsignmentProcess = this.consignment.getConsignmentProcesses().stream().filter((consignmentProcess) -> {
      return consignmentProcess.getCode().equals(consignmentProcessCode);
    }).findFirst();
    Collection<DeclineEntry> entriesToReallocate = new ArrayList();
    if (myConsignmentProcess.isPresent()) {
      List<Component> rows = this.consignmentEntries.getRows().getChildren();
      rows.stream().filter((entry) -> {
        return ((Checkbox)entry.getFirstChild()).isChecked();
      }).forEach((entry) -> {
        this.createDeclineEntry(entriesToReallocate, entry);
      });
    }

    if (!entriesToReallocate.isEmpty()) {
      this.buildDeclineParam((ConsignmentProcessModel)myConsignmentProcess.get(), entriesToReallocate);
      this.getConsignmentBusinessProcessService().triggerChoiceEvent(this.getConsignment(), "ConsignmentActionEvent", "reallocateConsignment");
      ConsignmentModel refreshedConsignment = (ConsignmentModel)this.getModelService().get(this.getConsignment().getPk());

      for(int iterationCount = 0; !this.isDeclineProcessDone(refreshedConsignment, entriesToReallocate) && iterationCount < 500000; ++iterationCount) {
        this.getModelService().refresh(refreshedConsignment);
      }

      refreshedConsignment.getConsignmentEntries().forEach((entry) -> {
        this.getCockpitEventQueue().publishEvent(new DefaultCockpitEvent("objectsUpdated", entry, (Object)null));
      });
      this.setConsignment(refreshedConsignment);
      this.getNotificationService().notifyUser("", "JustMessage", Level.SUCCESS, new Object[]{this.getLabel("warehousingbackoffice.reallocationconsignment.success.message")});
    } else {
      this.getNotificationService().notifyUser("", "JustMessage", Level.FAILURE, new Object[]{this.getLabel("warehousingbackoffice.reallocationconsignment.error.message")});
    }

    this.sendOutput("confirmOutput", COMPLETED);
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

  protected void createDeclineEntry(Collection<DeclineEntry> entriesToReallocate, Component component) {
    ConsignmentEntryToReallocateDto consignmentEntryToReallocate = (ConsignmentEntryToReallocateDto)((Row)component).getValue();
    Long qtyToReallocate = consignmentEntryToReallocate.getQuantityToReallocate();
    Long qtyAvailableForReallocation = consignmentEntryToReallocate.getConsignmentEntry().getQuantityPending();
    if (qtyToReallocate > 0L && qtyToReallocate <= qtyAvailableForReallocation) {
      DeclineEntry newEntry = new DeclineEntry();
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
    List<Component> rows = this.consignmentEntries.getRows().getChildren();
    Iterator var3 = rows.iterator();

    while(var3.hasNext()) {
      Component row = (Component)var3.next();
      Iterator var5 = row.getChildren().iterator();

      while(var5.hasNext()) {
        Component myComponent = (Component)var5.next();
        if (myComponent instanceof Checkbox) {
          myComponent.addEventListener("onCheck", (event) -> {
            this.handleRow((Row)event.getTarget().getParent());
          });
        } else if (myComponent instanceof Combobox) {
          myComponent.addEventListener("onSelect", this::handleIndividualLocation);
          myComponent.addEventListener("onCustomChange", (event) -> {
            Events.echoEvent("onLaterCustomChange", myComponent, event.getData());
          });
          myComponent.addEventListener("onLaterCustomChange", (event) -> {
            Clients.clearWrongValue(myComponent);
            myComponent.invalidate();
            this.handleIndividualReason(event);
          });
        } else if (myComponent instanceof Intbox) {
          myComponent.addEventListener("onChange", (event) -> {
            this.autoSelect(event);
            ((ConsignmentEntryToReallocateDto)((Row)event.getTarget().getParent()).getValue()).setQuantityToReallocate(Long.parseLong(((InputEvent)event).getValue()));
          });
        } else if (myComponent instanceof Textbox) {
          myComponent.addEventListener("onChanging", (event) -> {
            this.autoSelect(event);
            ((ConsignmentEntryToReallocateDto)((Row)event.getTarget().getParent()).getValue()).setDeclineConsignmentEntryComment(((InputEvent)event).getValue());
          });
        }
      }
    }

    this.globalDeclineReasons.addEventListener("onSelect", this::handleGlobalReason);
    this.globalPossibleLocations.addEventListener("onSelect", this::handleGlobalLocation);
    this.globalDeclineComment.addEventListener("onChanging", this::handleGlobalComment);
    this.globalDeclineEntriesSelection.addEventListener("onCheck", (event) -> {
      this.selectAllEntries();
    });
  }

  protected void applyToGrid(Object data, int childrenIndex) {
    this.consignmentEntries.getRows().getChildren().stream().filter((entry) -> {
      return ((Checkbox)entry.getChildren().iterator().next()).isChecked();
    }).forEach((entry) -> {
      this.applyToRow(data, childrenIndex, entry);
    });
  }

  protected void applyToRow(Object data, int childrenIndex, Component row) {
    int index = 0;

    for(Iterator var6 = row.getChildren().iterator(); var6.hasNext(); ++index) {
      Component myComponent = (Component)var6.next();
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

  protected void applyToComboboxRow(Object data, Component component) {
    if (component instanceof Combobox) {
      if (data == null) {
        ((Combobox)component).setSelectedItem((Comboitem)null);
      } else {
        ((Combobox)component).setSelectedIndex((Integer)data);
      }
    }

  }

  protected void applyToCheckboxRow(Object data, Component component) {
    if (component instanceof Checkbox) {
      if (data == null) {
        ((Checkbox)component).setChecked(Boolean.FALSE);
      } else {
        ((Checkbox)component).setChecked((Boolean)data);
      }
    }

  }

  protected void autoSelect(Event event) {
    ((Checkbox)event.getTarget().getParent().getChildren().iterator().next()).setChecked(true);
  }

  protected void buildDeclineParam(ConsignmentProcessModel processModel, Collection<DeclineEntry> entriesToReallocate) {
    this.cleanDeclineParam(processModel);
    Collection<BusinessProcessParameterModel> contextParams = new ArrayList();
    contextParams.addAll(processModel.getContextParameters());
    DeclineEntries declinedEntries = new DeclineEntries();
    declinedEntries.setEntries(entriesToReallocate);
    BusinessProcessParameterModel declineParam = new BusinessProcessParameterModel();
    declineParam.setName("declineEntries");
    declineParam.setValue(declinedEntries);
    declineParam.setProcess(processModel);
    contextParams.add(declineParam);
    processModel.setContextParameters(contextParams);
    this.getModelService().save(processModel);
  }

  protected void cleanDeclineParam(ConsignmentProcessModel processModel) {
    Collection<BusinessProcessParameterModel> contextParams = new ArrayList();
    contextParams.addAll(processModel.getContextParameters());
    if (CollectionUtils.isNotEmpty(contextParams)) {
      Optional<BusinessProcessParameterModel> declineEntriesParamOptional = contextParams.stream().filter((param) -> {
        return param.getName().equals("declineEntries");
      }).findFirst();
      if (declineEntriesParamOptional.isPresent()) {
        BusinessProcessParameterModel declineEntriesParam = (BusinessProcessParameterModel)declineEntriesParamOptional.get();
        contextParams.remove(declineEntriesParam);
        this.getModelService().remove(declineEntriesParam);
        processModel.setContextParameters(contextParams);
        this.getModelService().save(processModel);
      }
    }

  }

  protected int getLocationIndex(WarehouseModel location) {
    int index = 0;

    for(Iterator var4 = this.locations.iterator(); var4.hasNext(); ++index) {
      WarehouseModel warehouseModel = (WarehouseModel)var4.next();
      if (location.getCode().equals(warehouseModel.getCode())) {
        break;
      }
    }

    return index;
  }

  protected int getReasonIndex(DeclineReason declineReason) {
    int index = 0;
    String myReason = this.getEnumerationService().getEnumerationName(declineReason, this.getCockpitLocaleService().getCurrentLocale());

    for(Iterator var5 = this.declineReasons.iterator(); var5.hasNext(); ++index) {
      String reason = (String)var5.next();
      if (myReason.equals(reason)) {
        break;
      }
    }

    return index;
  }

  protected Optional<DeclineReason> getSelectedDeclineReason(Event event) {
    Optional<DeclineReason> result = Optional.empty();
    if (!((SelectEvent)event).getSelectedItems().isEmpty()) {
      Object selectedValue = ((Comboitem)((SelectEvent)event).getSelectedItems().iterator().next()).getValue();
      result = this.matchingComboboxDeclineReason(selectedValue.toString());
    }

    return result;
  }

  protected Optional<DeclineReason> getCustomSelectedDeclineReason(Event event) {
    Optional<DeclineReason> reason = Optional.empty();
    if (event.getTarget() instanceof Combobox) {
      Object selectedValue = event.getData();
      reason = this.matchingComboboxDeclineReason(selectedValue.toString());
    }

    return reason;
  }

  protected WarehouseModel getSelectedLocation(Event event) {
    WarehouseModel result = null;
    if (!((SelectEvent)event).getSelectedItems().isEmpty()) {
      result = (WarehouseModel)((Comboitem)((SelectEvent)event).getSelectedItems().iterator().next()).getValue();
    }

    return result;
  }

  protected void handleGlobalComment(Event event) {
    this.applyToGrid(((InputEvent)event).getValue(), 7);
    this.consignmentEntries.getRows().getChildren().stream().filter((entry) -> {
      return ((Checkbox)entry.getChildren().iterator().next()).isChecked();
    }).forEach((entry) -> {
      ((ConsignmentEntryToReallocateDto)((Row)entry).getValue()).setDeclineConsignmentEntryComment(((InputEvent)event).getValue());
    });
  }

  protected void handleGlobalLocation(Event event) {
    WarehouseModel selectedLocation = this.getSelectedLocation(event);
    if (selectedLocation != null) {
      this.applyToGrid(this.getLocationIndex(selectedLocation), 6);
      this.consignmentEntries.getRows().getChildren().stream().filter((entry) -> {
        return ((Checkbox)entry.getChildren().iterator().next()).isChecked();
      }).forEach((entry) -> {
        ((ConsignmentEntryToReallocateDto)((Row)entry).getValue()).setSelectedLocation(selectedLocation);
      });
    }

  }

  protected void handleGlobalReason(Event event) {
    Optional<DeclineReason> declineReason = this.getSelectedDeclineReason(event);
    if (declineReason.isPresent()) {
      this.applyToGrid(this.getReasonIndex((DeclineReason)declineReason.get()), 5);
      this.consignmentEntries.getRows().getChildren().stream().filter((entry) -> {
        return ((Checkbox)entry.getChildren().iterator().next()).isChecked();
      }).forEach((entry) -> {
        ((ConsignmentEntryToReallocateDto)((Row)entry).getValue()).setSelectedReason((DeclineReason)declineReason.get());
      });
    }

  }

  protected void handleIndividualReason(Event event) {
    Optional<DeclineReason> declineReason = this.getCustomSelectedDeclineReason(event);
    if (declineReason.isPresent()) {
      this.autoSelect(event);
      ((ConsignmentEntryToReallocateDto)((Row)event.getTarget().getParent()).getValue()).setSelectedReason((DeclineReason)declineReason.get());
    }

  }

  protected void handleIndividualLocation(Event event) {
    if (!((SelectEvent)event).getSelectedItems().isEmpty()) {
      this.autoSelect(event);
      Object selectedValue = ((Comboitem)((SelectEvent)event).getSelectedItems().iterator().next()).getValue();
      if (selectedValue instanceof WarehouseModel) {
        ((ConsignmentEntryToReallocateDto)((Row)event.getTarget().getParent()).getValue()).setSelectedLocation((WarehouseModel)selectedValue);
      }
    }

  }

  protected void handleRow(Row row) {
    ConsignmentEntryToReallocateDto myEntry = (ConsignmentEntryToReallocateDto)row.getValue();
    if (row.getChildren().iterator().next() instanceof Checkbox) {
      if (!((Checkbox)row.getChildren().iterator().next()).isChecked()) {
        this.applyToRow(0, 4, row);
        this.applyToRow((Object)null, 5, row);
        this.applyToRow((Object)null, 6, row);
        this.applyToRow((Object)null, 7, row);
        myEntry.setQuantityToReallocate(0L);
        myEntry.setSelectedReason((DeclineReason)null);
        myEntry.setSelectedLocation((WarehouseModel)null);
        myEntry.setDeclineConsignmentEntryComment((String)null);
      } else {
        this.applyToRow(this.globalDeclineReasons.getSelectedIndex(), 5, row);
        this.applyToRow(this.globalPossibleLocations.getSelectedIndex(), 6, row);
        this.applyToRow(this.globalDeclineComment.getValue(), 7, row);
        Optional<DeclineReason> reason = this.matchingComboboxDeclineReason(this.globalDeclineReasons.getSelectedItem() != null ? this.globalDeclineReasons.getSelectedItem().getLabel() : null);
        myEntry.setSelectedReason(reason.isPresent() ? (DeclineReason)reason.get() : null);
        myEntry.setSelectedLocation(this.globalPossibleLocations.getSelectedItem() != null ? (WarehouseModel)this.globalPossibleLocations.getSelectedItem().getValue() : null);
        myEntry.setDeclineConsignmentEntryComment(this.globalDeclineComment.getValue());
      }
    }

  }

  protected boolean isDeclineProcessDone(ConsignmentModel latestConsignmentModel, Collection<DeclineEntry> entriesToReallocate) {
    return entriesToReallocate.stream().allMatch((entry) -> {
      return this.isDeclinedQuantityCorrect(latestConsignmentModel, entry);
    });
  }

  protected boolean isDeclinedQuantityCorrect(ConsignmentModel latestConsignmentModel, DeclineEntry declineEntry) {
    Long expectedDeclinedQuantity = declineEntry.getConsignmentEntry().getQuantityDeclined() + declineEntry.getQuantity();
    return latestConsignmentModel.getConsignmentEntries().stream().anyMatch((entry) -> {
      return entry.getPk().equals(declineEntry.getConsignmentEntry().getPk()) && expectedDeclinedQuantity.equals(entry.getQuantityDeclined());
    });
  }

  protected Optional<DeclineReason> matchingComboboxDeclineReason(String declineReasonLabel) {
    return this.getEnumerationService().getEnumerationValues(DeclineReason.class).stream().filter((reason) -> {
      return this.getEnumerationService().getEnumerationName(reason, this.getCockpitLocaleService().getCurrentLocale()).equals(declineReasonLabel);
    }).findFirst();
  }

  protected void selectAllEntries() {
    this.applyToGrid(Boolean.TRUE, 0);
    Iterator var2 = this.consignmentEntries.getRows().getChildren().iterator();

    while(var2.hasNext()) {
      Component row = (Component)var2.next();
      Component firstComponent = (Component)row.getChildren().iterator().next();
      if (firstComponent instanceof Checkbox) {
        ((Checkbox)firstComponent).setChecked(this.globalDeclineEntriesSelection.isChecked());
      }

      this.handleRow((Row)row);
      if (this.globalDeclineEntriesSelection.isChecked()) {
        int reallocatableQuantity = Integer.parseInt(((Label)row.getChildren().get(3)).getValue());
        this.applyToRow(reallocatableQuantity, 4, row);
      }
    }

    if (this.globalDeclineEntriesSelection.isChecked()) {
      this.consignmentsEntriesToReallocate.stream().forEach((entry) -> {
        entry.setQuantityToReallocate(entry.getConsignmentEntry().getQuantityPending());
      });
    }

  }

  protected Component targetFieldToApplyValidation(String stringToValidate, int indexLabelToCheck, int indexTargetComponent) {
    Iterator var5 = this.consignmentEntries.getRows().getChildren().iterator();

    while(var5.hasNext()) {
      Component component = (Component)var5.next();
      Label label = (Label)component.getChildren().get(indexLabelToCheck);
      if (label.getValue().equals(stringToValidate)) {
        return (Component)component.getChildren().get(indexTargetComponent);
      }
    }

    return null;
  }

  protected void validateConsignmentEntry(ConsignmentEntryToReallocateDto entry) {
    InputElement quantity;
    if (entry.getQuantityToReallocate() > entry.getConsignmentEntry().getQuantityPending()) {
      quantity = (InputElement)this.targetFieldToApplyValidation(entry.getConsignmentEntry().getOrderEntry().getProduct().getCode(), 1, 4);
      throw new WrongValueException(quantity, this.getLabel("warehousingbackoffice.reallocationconsignment.decline.validation.invalid.quantity"));
    } else if (entry.getSelectedReason() != null && entry.getQuantityToReallocate() == 0L) {
      quantity = (InputElement)this.targetFieldToApplyValidation(entry.getConsignmentEntry().getOrderEntry().getProduct().getCode(), 1, 4);
      throw new WrongValueException(quantity, this.getLabel("warehousingbackoffice.reallocationconsignment.decline.validation.missing.quantity"));
    } else {
      Combobox location;
      if (entry.getSelectedReason() == null && entry.getQuantityToReallocate() > 0L) {
        location = (Combobox)this.targetFieldToApplyValidation(entry.getConsignmentEntry().getOrderEntry().getProduct().getCode(), 1, 5);
        throw new WrongValueException(location, this.getLabel("warehousingbackoffice.reallocationconsignment.decline.validation.missing.reason"));
      } else if (entry.getSelectedLocation() != null && isStockNotAvailable(entry.getConsignmentEntry(), entry.getSelectedLocation())) {
        location = (Combobox)this.targetFieldToApplyValidation(entry.getConsignmentEntry().getOrderEntry().getProduct().getCode(), 1, 6);
        throw new WrongValueException(location, this.getLabel("warehousingbackoffice.reallocationconsignment.decline.validation.invalid.stockLevel"));
      }
    }
  }

  private boolean isStockNotAvailable(final ConsignmentEntryModel consignmentEntry,
      final WarehouseModel selectedLocation) {

    final Set<String> productCodes = new HashSet<String>();
    productCodes.add(consignmentEntry.getOrderEntry().getProduct().getCode());

    final OrderModel orderModel = (OrderModel) consignmentEntry.getOrderEntry().getOrder();

    final Collection<StockLevelModel> stockLevels = blCommerceStockService
        .getStockForProductCodesAndDate(productCodes,
            selectedLocation, orderModel.getActualRentalStartDate(),
            orderModel.getActualRentalEndDate());

    if(CollectionUtils.isNotEmpty(stockLevels)) {

      final Map<Object, List<StockLevelModel>> stockLevelsDatewise = stockLevels.stream()
          .collect(Collectors.groupingBy(stockLevel -> stockLevel.getDate()));
      final LocalDateTime rentalStartDate = BlDateTimeUtils.getFormattedDateTime(orderModel.getActualRentalStartDate());
      final LocalDateTime rentalEndDate = BlDateTimeUtils.getFormattedDateTime(orderModel.getActualRentalEndDate());
      final long stayDuration = ChronoUnit.DAYS.between(rentalStartDate, rentalEndDate.plusDays(1));
      final Set<Object> datesPresentInStockTable = stockLevelsDatewise.keySet();
      //This is to check whether stock for any particular day is missing in inventory table
      if (datesPresentInStockTable.size() == stayDuration){
        return false;
      } else {
        return true;
      }

    }

    return true;
  }

  protected void validateRequest() {
    Iterator var2 = this.getConsignmentEntries().getRows().getChildren().iterator();

    while(var2.hasNext()) {
      Component row = (Component)var2.next();
      Component firstComponent = (Component)row.getChildren().iterator().next();
      if (firstComponent instanceof Checkbox && ((Checkbox)firstComponent).isChecked()) {
        InputElement returnQty = (InputElement)row.getChildren().get(4);
        if (returnQty.getRawValue().equals(0)) {
          throw new WrongValueException(returnQty, this.getLabel("warehousingbackoffice.reallocationconsignment.decline.validation.missing.quantity"));
        }
      }
    }

    ListModelList<ConsignmentEntryToReallocateDto> modelList = (ListModelList)this.getConsignmentEntries().getModel();
    if (modelList.stream().allMatch((entry) -> {
      return entry.getQuantityToReallocate() == 0L;
    })) {
      throw new WrongValueException(this.globalDeclineEntriesSelection, this.getLabel("warehousingbackoffice.reallocationconsignment.decline.validation.missing.selectedLine"));
    } else {
      modelList.forEach(this::validateConsignmentEntry);
    }
  }

  protected ConsignmentModel getConsignment() {
    return this.consignment;
  }

  public void setConsignment(ConsignmentModel consignment) {
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

  protected NotificationService getNotificationService() {
    return this.notificationService;
  }

}
