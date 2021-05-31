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
import java.util.List;

import java.util.stream.Collectors;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * It is a strategy to source from multiple warehouses without any restriction.
 *
 * @author Sunil
 */
public class BlNoRestrictionsStrategy extends AbstractSourcingStrategy {
  private static final Logger LOG = Logger
      .getLogger(BlNoRestrictionsStrategy.class);

  private ModelService modelService;
  private BlAssignSerialService blAssignSerialService;

  public BlNoRestrictionsStrategy() {
  }

  /**
   * This is to source the order
   *
   * @param sourcingContext the sourcingContext
   */
  public void source(SourcingContext sourcingContext)  throws BlSourcingException {
    ServicesUtil.validateParameterNotNullStandardMessage("sourcingContext", sourcingContext);

    boolean canBeSourcedCompletely = canBeSourcedCompletely(sourcingContext);
    if (canBeSourcedCompletely) {
      boolean sourcingComplete = assignSerials(sourcingContext);
      sourcingContext.getResult().setComplete(sourcingComplete);
    } else { //can not be sourced all the products from all warehouses
      sourcingContext.getResult().setComplete(false);
    }
  }

  private boolean assignSerials(SourcingContext context)  throws BlSourcingException  {
    SourcingLocation completeSourcingLocation = context.getSourcingLocations().stream().filter(
        SourcingLocation::isCompleteSourcePossible).findAny().orElse(null);

    boolean sourcingComplete = false;
    if (null != completeSourcingLocation) { //sourcing possible from single location
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Complete sourcing is possible from warehouse {}",
          completeSourcingLocation.getWarehouse().getCode());
      sourcingComplete = blAssignSerialService
          .assignSerialsFromLocation(context, completeSourcingLocation);
    } else {  //sourcing from multiple locations
      SourcingLocation primarySourcingLocation = context.getPrimaryLocation();
      List<SourcingLocation> otherLocations = context.getSourcingLocations().stream()
          .filter(sl -> !sl.getWarehouse().equals(primarySourcingLocation.getWarehouse())).collect(
              Collectors.toList());

      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Sourcing from multiple locations, starting with primary location/warehouse {}",
          primarySourcingLocation.getWarehouse().getCode());
      sourcingComplete = blAssignSerialService
          .assignSerialsFromLocation(context, primarySourcingLocation);
      if (!sourcingComplete) {
        otherLocations.forEach(otherLocation -> blAssignSerialService
            .assignSerialsFromLocation(context, otherLocation));
      }
    }
    return sourcingComplete;
  }

  private boolean canBeSourcedCompletely(SourcingContext sourcingContext)  throws BlSourcingException {
    boolean canBeSourcedCompletely = true;
    for (Long unAllocatedValue : sourcingContext.getUnAllocatedMap().values()) {
      if (unAllocatedValue > 0) {
        // source was incomplete from all warehouses
        canBeSourcedCompletely = false;
        // mark the order status as error
        AbstractOrderModel order = sourcingContext.getOrderEntries().iterator().next().getOrder();
        order.setStatus(OrderStatus.SUSPENDED);
        modelService.save(order);
        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "All products can not be sourced. Suspending the order {}",
            order.getCode());
        throw new BlSourcingException("All products can not be sourced.");
      }
    }
    return canBeSourcedCompletely;
  }

  public BlAssignSerialService getBlAssignSerialService() {
    return blAssignSerialService;
  }

  public void setBlAssignSerialService(
      BlAssignSerialService blAssignSerialService) {
    this.blAssignSerialService = blAssignSerialService;
  }

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }

}
