package com.bl.Ordermanagement.populators;

import com.bl.core.stock.BlCommerceStockService;
import com.bl.logging.BlLogger;
import com.google.common.base.Preconditions;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.warehousing.data.sourcing.SourcingLocation;
import de.hybris.platform.warehousing.sourcing.context.populator.SourcingLocationPopulator;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * It is used to populate availability map from Warehouse to SourcingLocation.
 *
 * @author Sunil
 */
public class BlAvailabilitySourcingLocationPopulator implements SourcingLocationPopulator {
  private static final Logger LOG = Logger
      .getLogger(BlAvailabilitySourcingLocationPopulator.class);
  private BlCommerceStockService blCommerceStockService;

  public BlAvailabilitySourcingLocationPopulator() {
  }

  /**
   * This is to populate availability map
   *
   * @param source the WarehouseModel
   * @param target the SourcingLocation
   */
  public void populate(WarehouseModel source, SourcingLocation target) {
    Preconditions.checkArgument(source != null, "Point of service model (source) cannot be null.");
    Preconditions.checkArgument(target != null, "Sourcing location (target) cannot be null.");
    Map<String, List<StockLevelModel>> availabilityMap;
    AbstractOrderModel order = target.getContext().getOrderEntries().iterator().next().getOrder();
    Set<String> productCodes = order.getEntries().stream()
        .map(entry -> entry.getProduct().getCode()).collect(Collectors.toSet());
    Collection<StockLevelModel> stockLevels = blCommerceStockService
        .getStockForProductCodesAndDate(productCodes,
            source, order.getRentalStartDate(), order.getRentalEndDate());
    if (CollectionUtils.isNotEmpty(stockLevels)) {
      availabilityMap = blCommerceStockService.groupByProductsAvailability(stockLevels);
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Populating availability map, serial products {} found for product codes {} from date {} to date {}",
          stockLevels.stream().map(stock-> stock.getSerialProductCode()).collect(Collectors.toList()),  productCodes, order.getRentalStartDate(), order.getRentalEndDate());
      target.setAvailabilityMap(availabilityMap);
    }

  }

  public com.bl.core.stock.BlCommerceStockService getBlCommerceStockService() {
    return blCommerceStockService;
  }

  public void setBlCommerceStockService(BlCommerceStockService blCommerceStockService) {
    this.blCommerceStockService = blCommerceStockService;
  }

}
