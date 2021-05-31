package com.bl.Ordermanagement.strategy.impl;

import com.bl.Ordermanagement.services.BlSourcingLocationService;
import com.bl.core.stock.BlCommerceStockService;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;
import de.hybris.platform.warehousing.data.sourcing.SourcingContext;
import de.hybris.platform.warehousing.data.sourcing.SourcingLocation;
import de.hybris.platform.warehousing.sourcing.strategy.AbstractSourcingStrategy;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.StringUtils;

public class BlNoSplittingStrategy extends AbstractSourcingStrategy {

  private BlCommerceStockService blCommerceStockService;
  private BlSourcingLocationService blSourcingLocationService;
  private BaseStoreService baseStoreService;

  public void source(SourcingContext sourcingContext) {
    ServicesUtil.validateParameterNotNullStandardMessage("sourcingContext", sourcingContext);

    SourcingLocation sourcingLocation = sourcingContext.getSourcingLocations().iterator().next();
    sourcingContext.setPrimaryLocation(sourcingLocation);
    populateContextWithUnAllocatedMap(sourcingContext);
    if (isSourcingNoSplittingPossible(sourcingContext, sourcingLocation)) {
       sourcingLocation.setCompleteSourcePossible(true);
     // sourcingContext.getResult().setComplete(sourcingComplete);
    } else {
      final BaseStoreModel baseStore = baseStoreService.getBaseStoreForUid("bl");
     /* Collection<WarehouseModel> warehouseModels = baseStoreService.getCurrentBaseStore()
          .getWarehouses();*/
      Collection<WarehouseModel> warehouseModels = baseStore.getWarehouses();
      for (WarehouseModel warehouse : warehouseModels) {
        if (!StringUtils
            .equalsIgnoreCase(warehouse.getCode(), sourcingLocation.getWarehouse().getCode())
            && warehouse.isActive()) {
          SourcingLocation otherSourcingLocation = blSourcingLocationService
              .createSourcingLocation(sourcingContext, warehouse);
          if (isSourcingNoSplittingPossible(sourcingContext,
              otherSourcingLocation)) {
            otherSourcingLocation.setCompleteSourcePossible(true);
            break;
          }
        }
      }

    }
    // LOGGER.debug("Total order entries sourceable using No Splitting Strategy: {}", sourcingContext.getResult().getResults().size());
  }

  private void populateContextWithUnAllocatedMap(SourcingContext sourcingContext) {
    Map<String, Long> unAllocatedMap = new HashMap<>();
    if (MapUtils.isEmpty(sourcingContext.getUnAllocatedMap())) {
      sourcingContext.getOrderEntries()
          .forEach(entry -> unAllocatedMap.put(entry.getProduct().getCode() + "_" + entry.getEntryNumber(), entry.getQuantity()));
    }
    sourcingContext.setUnAllocatedMap(unAllocatedMap);
  }

  private boolean isSourcingNoSplittingPossible(SourcingContext sourcingContext,
      SourcingLocation sourcingLocation) {
    sourcingContext.getOrderEntries().stream().forEach(orderEntry -> {
      Long availableQty = getAvailabilityForProduct(orderEntry.getProduct(), sourcingLocation);
      if (availableQty.longValue() != 0l) {
        populateContextWithAllocatedQuantity(sourcingContext, sourcingLocation, orderEntry,
            availableQty);
      }
    });
    return sourcingContext.getOrderEntries().stream().allMatch((entry) -> {
      Long availableQty = getAvailabilityForProduct(entry.getProduct(), sourcingLocation);
      return ((OrderEntryModel) entry).getQuantity() <= availableQty;
    });

  }

  private void populateContextWithAllocatedQuantity(SourcingContext sourcingContext,
      SourcingLocation sourcingLocation, AbstractOrderEntryModel entry, Long availableQty) {
    Map<String, Long> unAllocatedMap = sourcingContext.getUnAllocatedMap();

    Long oldUnAllocatedQty = unAllocatedMap.get(entry.getProduct().getCode()+"_"+entry.getEntryNumber());
    Map<String, Long> allocatedMap = new HashMap<>();
    Long allocatableQty = 0L;
    allocatableQty = (oldUnAllocatedQty > 0) && (oldUnAllocatedQty > availableQty) ? availableQty : oldUnAllocatedQty;

    allocatedMap.put(entry.getProduct().getCode() + "_" + entry.getEntryNumber(), allocatableQty);
    sourcingLocation.setAllocatedMap(allocatedMap);
    unAllocatedMap.put(entry.getProduct().getCode() + "_" + entry.getEntryNumber(), oldUnAllocatedQty - allocatableQty);
    sourcingContext.setUnAllocatedMap(unAllocatedMap);
  }

  protected Long getAvailabilityForProduct(ProductModel productModel,
      SourcingLocation sourcingLocation) {
    Long stockLevel = 0L;
    if (sourcingLocation.getAvailabilityMap() != null
        && sourcingLocation.getAvailabilityMap().get(productModel.getCode()) != null) {
      stockLevel = Long
          .valueOf(sourcingLocation.getAvailabilityMap().get(productModel.getCode()).size());
    }
    return stockLevel;
  }

  public BlCommerceStockService getBlCommerceStockService() {
    return blCommerceStockService;
  }

  public void setBlCommerceStockService(BlCommerceStockService blCommerceStockService) {
    this.blCommerceStockService = blCommerceStockService;
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

}
