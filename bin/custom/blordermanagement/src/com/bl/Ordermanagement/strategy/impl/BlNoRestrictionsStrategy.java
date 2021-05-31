package com.bl.Ordermanagement.strategy.impl;

import com.bl.Ordermanagement.exceptions.BlSourcingException;
import com.bl.Ordermanagement.services.BlAssignSerialService;
import com.bl.Ordermanagement.services.BlSourcingLocationService;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import de.hybris.platform.store.services.BaseStoreService;
import de.hybris.platform.warehousing.data.sourcing.SourcingContext;
import de.hybris.platform.warehousing.data.sourcing.SourcingLocation;
import de.hybris.platform.warehousing.sourcing.strategy.AbstractSourcingStrategy;
import java.util.List;

import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class BlNoRestrictionsStrategy extends AbstractSourcingStrategy {

  private static Logger LOGGER = LoggerFactory.getLogger(BlNoRestrictionsStrategy.class);
  private BlSourcingLocationService blSourcingLocationService;
  private BaseStoreService baseStoreService;
  private ModelService modelService;
  private BlAssignSerialService blAssignSerialService;

  public BlNoRestrictionsStrategy() {
  }

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
      sourcingComplete = blAssignSerialService
          .assignSerialsFromLocation(context, completeSourcingLocation);
    } else {  //sourcing from multiple locations
      SourcingLocation primarySourcingLocation = context.getPrimaryLocation();
      List<SourcingLocation> otherLocations = context.getSourcingLocations().stream()
          .filter(sl -> !sl.getWarehouse().equals(primarySourcingLocation.getWarehouse())).collect(
              Collectors.toList());

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
        throw new BlSourcingException("All products can not be sourced.");
      }
    }
    return canBeSourcedCompletely;
  }

  public BlSourcingLocationService getBlSourcingLocationService() {
    return blSourcingLocationService;
  }

  public void setBlSourcingLocationService(
      BlSourcingLocationService blSourcingLocationService) {
    this.blSourcingLocationService = blSourcingLocationService;
  }

  public BaseStoreService getBaseStoreService() {
    return baseStoreService;
  }

  public void setBaseStoreService(BaseStoreService baseStoreService) {
    this.baseStoreService = baseStoreService;
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
