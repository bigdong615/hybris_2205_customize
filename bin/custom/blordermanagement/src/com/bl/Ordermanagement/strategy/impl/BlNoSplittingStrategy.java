package com.bl.Ordermanagement.strategy.impl;

import com.bl.Ordermanagement.services.BlSourcingLocationService;
import com.bl.core.product.service.BlProductService;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;
import de.hybris.platform.warehousing.data.sourcing.SourcingContext;
import de.hybris.platform.warehousing.data.sourcing.SourcingLocation;
import de.hybris.platform.warehousing.sourcing.strategy.AbstractSourcingStrategy;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * It is a strategy to find out if order splitting is possible or not, sourcing can be possible from
 * single warehouse.
 *
 * @author Sunil
 */
public class BlNoSplittingStrategy extends AbstractSourcingStrategy {

  private static final Logger LOG = Logger.getLogger(BlNoSplittingStrategy.class);
  private BlCommerceStockService blCommerceStockService;
  private BlSourcingLocationService blSourcingLocationService;
  private BaseStoreService baseStoreService;
  private BlProductService productService;

  /**
   * This is to source the order
   *
   * @param sourcingContext the sourcingContext
   */
  public void source(final SourcingContext sourcingContext) {

    ServicesUtil.validateParameterNotNullStandardMessage("sourcingContext", sourcingContext);

    final SourcingLocation sourcingLocation = sourcingContext.getSourcingLocations().iterator().next();
    sourcingContext.setPrimaryLocation(sourcingLocation);
    populateContextWithUnAllocatedMap(sourcingContext);

    if (isSourcingNoSplittingPossible(sourcingContext, sourcingLocation)) {
      
       sourcingLocation.setCompleteSourcePossible(true);
      BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Complete sourcing possible from primary warehouse {}",
          sourcingLocation.getWarehouse().getCode());
    } else {
      final BaseStoreModel baseStore = baseStoreService.getBaseStoreForUid("bl");
      final Collection<WarehouseModel> warehouseModels = baseStore.getWarehouses();
      for (WarehouseModel warehouse : warehouseModels) {
        if (!StringUtils
            .equalsIgnoreCase(warehouse.getCode(), sourcingLocation.getWarehouse().getCode())
            && warehouse.isActive()) {

         final SourcingLocation otherSourcingLocation = blSourcingLocationService
              .createSourcingLocation(sourcingContext, warehouse);
          if (isSourcingNoSplittingPossible(sourcingContext, otherSourcingLocation)) {
            
            otherSourcingLocation.setCompleteSourcePossible(true);
            BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Complete sourcing possible from other warehouse {}",
                otherSourcingLocation.getWarehouse().getCode());
            break;
          }
        }
      }

    }

  }

  /**
   * Populate context with unallocated map.
   * @param sourcingContext
   */
  private void populateContextWithUnAllocatedMap(final SourcingContext sourcingContext) {

    final Map<String, Long> unAllocatedMap = new HashMap<>();
    if (MapUtils.isEmpty(sourcingContext.getUnallocatedMap())) {
      sourcingContext.getOrderEntries()
          .forEach(entry -> unAllocatedMap.put(entry.getProduct().getCode() + "_" + entry.getEntryNumber(), BooleanUtils.isTrue(sourcingContext.isModifiedEntryFromBackoffice()) ? sourcingContext.getModifiedQuantityForEntry() : entry.getQuantity()));
    }
    sourcingContext.setUnallocatedMap(unAllocatedMap);
  }

  /**
   * Check whether sourcing is possible with no splitting.
   * @param sourcingContext
   * @param sourcingLocation
   * @return true if possible.
   */
  private boolean isSourcingNoSplittingPossible(final SourcingContext sourcingContext,
      final SourcingLocation sourcingLocation) {

    sourcingContext.getOrderEntries().stream().forEach(orderEntry -> {

      final Long availableQty = isAquatechProductInEntry(orderEntry) ? orderEntry.getQuantity()
          : getAvailabilityForProduct(orderEntry.getProduct(), sourcingLocation);

      if (availableQty.longValue() != 0l) {
        populateContextWithAllocatedQuantity(sourcingContext, sourcingLocation, orderEntry,
            availableQty);
      }

    });

    return sourcingContext.getOrderEntries().stream().allMatch(entry -> {

      final Long availableQty = isAquatechProductInEntry(entry) ? entry.getQuantity()
          : getAvailabilityForProduct(entry.getProduct(), sourcingLocation);
      final Long quantity = BooleanUtils.isTrue(sourcingContext.isModifiedEntryFromBackoffice()) ? sourcingContext.getModifiedQuantityForEntry() :((OrderEntryModel) entry).getQuantity();
      return quantity <= availableQty;
    });
  }

  /**
   * Check whether aquatech product is in given order entry.
   *
   * @param orderEntry
   * @return true if aquatech product is in this entry.
   */
  private boolean isAquatechProductInEntry(final AbstractOrderEntryModel orderEntry) {

    return productService.isAquatechProduct(orderEntry.getProduct());
  }

    /**
   * Populate context with allocatedQuantity.
   * @param sourcingContext
   * @param sourcingLocation
   * @param entry
   * @param availableQty
   */
  private void populateContextWithAllocatedQuantity(final SourcingContext sourcingContext,
      final SourcingLocation sourcingLocation, final AbstractOrderEntryModel entry, final Long availableQty) {

    final Map<String, Long> unAllocatedMap = sourcingContext.getUnallocatedMap();

    final Long oldUnAllocatedQty = unAllocatedMap.get(entry.getProduct().getCode()+"_"+entry.getEntryNumber());
    final Map<String, Long> allocatedMap = (null != sourcingLocation.getAllocatedMap()) ? sourcingLocation.getAllocatedMap() : new HashMap<>();
    final Long allocatableQty = (oldUnAllocatedQty > 0) && (oldUnAllocatedQty > availableQty) ? availableQty : oldUnAllocatedQty;

    allocatedMap.put(entry.getProduct().getCode() + "_" + entry.getEntryNumber(), allocatableQty);
    sourcingLocation.setAllocatedMap(allocatedMap);
    BlLogger.logFormatMessageInfo(LOG, Level.INFO,
        "Sourcing Location with warehouse {} has allocatedMap =  {}",
        sourcingLocation.getWarehouse().getCode(), allocatedMap.toString());

    unAllocatedMap.put(entry.getProduct().getCode() + "_" + entry.getEntryNumber(), oldUnAllocatedQty - allocatableQty);
    sourcingContext.setUnallocatedMap(unAllocatedMap);
  }

  /**
   * Get availability for product.
   * @param productModel
   * @param sourcingLocation
   * @return available quantity.
   */
  @Override
  protected Long getAvailabilityForProduct(final ProductModel productModel,
      final SourcingLocation sourcingLocation) {

    Long stockLevel = 0L;
    if (MapUtils.isNotEmpty(sourcingLocation.getAvailabilityMap())
        && sourcingLocation.getAvailabilityMap().get(productModel.getCode()) != null) {
     List<StockLevelModel> stockLevelList = sourcingLocation.getAvailabilityMap().get(productModel.getCode());
      final Set<String> serialProductCodes = stockLevelList.stream()
          .map(StockLevelModel::getSerialProductCode).collect(Collectors.toSet());

      stockLevel = Long.valueOf(serialProductCodes.size());

    }

    BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Stock size {} found for product code {} from warehouse {} ",
        stockLevel.intValue(), productModel.getCode(), sourcingLocation.getWarehouse().getCode());

    return stockLevel;
  }

  public BlCommerceStockService getBlCommerceStockService() {
    return blCommerceStockService;
  }

  public void setBlCommerceStockService(final BlCommerceStockService blCommerceStockService) {
    this.blCommerceStockService = blCommerceStockService;
  }

  public BlSourcingLocationService getBlSourcingLocationService() {
    return blSourcingLocationService;
  }

  public void setBlSourcingLocationService(
      final BlSourcingLocationService blSourcingLocationService) {
    this.blSourcingLocationService = blSourcingLocationService;
  }

  public BaseStoreService getBaseStoreService() {
    return baseStoreService;
  }

  public void setBaseStoreService(BaseStoreService baseStoreService) {
    this.baseStoreService = baseStoreService;
  }

  public BlProductService getProductService() {
    return productService;
  }

  public void setProductService(BlProductService productService) {
    this.productService = productService;
  }
}
