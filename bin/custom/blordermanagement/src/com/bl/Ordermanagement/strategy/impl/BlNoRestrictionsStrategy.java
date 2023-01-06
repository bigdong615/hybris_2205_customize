package com.bl.Ordermanagement.strategy.impl;

import com.bl.Ordermanagement.exceptions.BlSourcingException;
import com.bl.Ordermanagement.services.BlAssignSerialService;
import com.bl.core.constants.BlCoreConstants;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import de.hybris.platform.warehousing.data.sourcing.SourcingContext;
import de.hybris.platform.warehousing.data.sourcing.SourcingLocation;
import de.hybris.platform.warehousing.data.sourcing.SourcingResult;
import de.hybris.platform.warehousing.sourcing.strategy.AbstractSourcingStrategy;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

import org.apache.commons.lang.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * It is a strategy to source from multiple warehouses without any restriction.
 *
 * @author Sunil
 */
public class BlNoRestrictionsStrategy extends AbstractSourcingStrategy {

  private static final Logger LOG = Logger.getLogger(BlNoRestrictionsStrategy.class);

  private ModelService modelService;
  private BlAssignSerialService blAssignSerialService;

  public BlNoRestrictionsStrategy() {
    //default constructor
  }

  /**
   * This is to source the order
   *
   * @param sourcingContext the sourcingContext
   */
  public void source(final SourcingContext sourcingContext) throws BlSourcingException {

    ServicesUtil.validateParameterNotNullStandardMessage("sourcingContext", sourcingContext);

    final boolean sourcingComplete = assignSerials(sourcingContext);
    sourcingContext.getResult().setComplete(sourcingComplete);

    final boolean sourcingInComplete = updateOrderEntryUnallocatedQuantity(sourcingContext);
    if (sourcingInComplete) {

      BlLogger.logFormatMessageInfo(LOG, Level.INFO,
          "Sourcing is In-complete after tried sourcing from all possible location");
        updateOrderStatusForSourcingIncomplete(sourcingContext);

      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "All products can not be sourced. !!!");
    }
  }

  private boolean updateOrderEntryUnallocatedQuantity(final SourcingContext sourcingContext) {

    final List<AtomicBoolean> allEntrySourceInComplete = new ArrayList<>();
    sourcingContext.getOrderEntries().stream().forEach(entry -> {
      Long allResultQuantityAllocated = 0l;
      for (SourcingResult result : sourcingContext.getResult().getResults()) {
        if (null != result.getAllocation().get(entry)) {
          allResultQuantityAllocated += result.getAllocation().get(entry);
        }
      }

      if (!isAquatechProductInEntry(entry) && allResultQuantityAllocated < entry.getQuantity()) {
        allEntrySourceInComplete.add(new AtomicBoolean(true));
        entry.setUnAllocatedQuantity(entry.getQuantity() - allResultQuantityAllocated);
      }
    });

    return !allEntrySourceInComplete.isEmpty() && allEntrySourceInComplete.stream()
        .allMatch(AtomicBoolean::get);
  }

  /**
   * Check whether aquatech product is in given order entry.
   *
   * @param orderEntry
   * @return true if aquatech product is in this entry.
   */
  private boolean isAquatechProductInEntry(final AbstractOrderEntryModel orderEntry) {

    return BlCoreConstants.AQUATECH_BRAND_ID.equals(orderEntry.getProduct().getManufacturerAID());
  }

  /**
   * This method will mark the order as RECEIVED_MANUAL_REVIEW when some of the products can not be sourced.
   *
   * @param sourcingContext the sourcingContext
   */
  private void updateOrderStatusForSourcingIncomplete(final SourcingContext sourcingContext) {

    //can not be sourced all the products from all warehouses
    sourcingContext.getResult().setComplete(false);
    AbstractOrderModel order = sourcingContext.getOrderEntries().iterator().next().getOrder();
    if(Objects.isNull(sourcingContext.isIsNewOrderEntryFromBackoffice()) || BooleanUtils.isFalse(sourcingContext.isIsNewOrderEntryFromBackoffice())) {

        order.setStatus(OrderStatus.RECEIVED_MANUAL_REVIEW);
      }
    modelService.save(order);

  }

  /**
   * Source and assign serials.
   * @param context
   * @return true if sourcing complete.
   * @throws BlSourcingException
   */
  private boolean assignSerials(final SourcingContext context)  throws BlSourcingException  {

    final SourcingLocation completeSourcingLocation = context.getSourcingLocations().stream().filter(
        SourcingLocation::isCompleteSourcePossible).findAny().orElse(null);

    boolean sourcingComplete = false;
    if (null != completeSourcingLocation) { //sourcing possible from single location
      BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Complete sourcing is possible from single warehouse with code {}",
          completeSourcingLocation.getWarehouse().getCode());
      sourcingComplete = blAssignSerialService
          .assignSerialsFromLocation(context, completeSourcingLocation);
    } else {  //sourcing from multiple locations
      final SourcingLocation primarySourcingLocation = context.getPrimaryLocation();
      final List<SourcingLocation> otherLocations = context.getSourcingLocations().stream()
          .filter(sl -> !sl.getWarehouse().equals(primarySourcingLocation.getWarehouse())).collect(
              Collectors.toList());

      BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Sourcing from multiple locations, starting with primary location/warehouse {}",
          primarySourcingLocation.getWarehouse().getCode());
      if (null != primarySourcingLocation.getAvailabilityMap()) {
        sourcingComplete = blAssignSerialService
            .assignSerialsFromLocation(context, primarySourcingLocation);
      }
      if (!sourcingComplete) {

        otherLocations.forEach(otherLocation ->
            new AtomicBoolean(null != otherLocation.getAvailabilityMap() && blAssignSerialService
                .assignSerialsFromLocation(context, otherLocation)));
        sourcingComplete = blAssignSerialService.isAllQuantityFulfilled(context);

      }
    }
    return sourcingComplete;
  }


  public BlAssignSerialService getBlAssignSerialService() {
    return blAssignSerialService;
  }

  public void setBlAssignSerialService(
      final BlAssignSerialService blAssignSerialService) {
    this.blAssignSerialService = blAssignSerialService;
  }

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(final ModelService modelService) {
    this.modelService = modelService;
  }

}
