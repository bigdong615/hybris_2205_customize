package com.bl.Ordermanagement.strategy.impl;

import com.bl.Ordermanagement.exceptions.BlSourcingException;
import com.bl.Ordermanagement.services.BlAssignSerialService;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import de.hybris.platform.warehousing.data.sourcing.SourcingContext;
import de.hybris.platform.warehousing.data.sourcing.SourcingLocation;
import de.hybris.platform.warehousing.sourcing.strategy.AbstractSourcingStrategy;
import java.util.ArrayList;
import java.util.List;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
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
  public void source(final SourcingContext sourcingContext)  throws BlSourcingException {

    ServicesUtil.validateParameterNotNullStandardMessage("sourcingContext", sourcingContext);

    if (canBeSourcedCompletely(sourcingContext)) {
      final boolean sourcingComplete = assignSerials(sourcingContext);
      sourcingContext.getResult().setComplete(sourcingComplete);

      if (!sourcingComplete) {

        BlLogger.logFormatMessageInfo(LOG, Level.INFO,
            "Sourcing is In-complete after tried sourcing from all possible location");
        updateOrderStatusForSourcingIncomplete(sourcingContext);
      }
    } else {

      updateOrderStatusForSourcingIncomplete(sourcingContext);
    }
  }

  /**
   * This method will mark the order as manual review when some of the products can not be sourced.
   *
   * @param sourcingContext the sourcingContext
   */
  private void updateOrderStatusForSourcingIncomplete(final SourcingContext sourcingContext) {

    //can not be sourced all the products from all warehouses
    sourcingContext.getResult().setComplete(false);
    AbstractOrderModel order = sourcingContext.getOrderEntries().iterator().next().getOrder();
    order.setStatus(OrderStatus.MANUAL_REVIEW);
    modelService.save(order);

    throw new BlSourcingException("Some products can not be sourced.");
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
      sourcingComplete = blAssignSerialService
          .assignSerialsFromLocation(context, primarySourcingLocation);
      if (!sourcingComplete) {

        otherLocations.forEach(otherLocation ->
           new AtomicBoolean(blAssignSerialService
                .assignSerialsFromLocation(context, otherLocation)));

        sourcingComplete = blAssignSerialService.isAllQuantityFulfilled(context);
      }
    }
    return sourcingComplete;
  }

  /**
   * Check whether sourcing done completely.
   * @param sourcingContext
   * @return
   * @throws BlSourcingException
   */
  private boolean canBeSourcedCompletely(final SourcingContext sourcingContext)  throws BlSourcingException {

    boolean canBeSourcedCompletely = true;
    for (Long unAllocatedValue : sourcingContext.getUnallocatedMap().values()) {
      if (unAllocatedValue > 0) {
        // source was incomplete from all warehouses
        // mark the order status as error
        canBeSourcedCompletely = false;
        break;
      }
    }

    BlLogger
        .logFormatMessageInfo(LOG, Level.INFO, "Are all products can be sourced completely ? : {}",
            canBeSourcedCompletely);

    return canBeSourcedCompletely;
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
