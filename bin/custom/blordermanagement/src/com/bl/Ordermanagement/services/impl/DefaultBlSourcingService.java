package com.bl.Ordermanagement.services.impl;

import com.bl.Ordermanagement.exceptions.BlSourcingException;
import com.bl.Ordermanagement.filters.BlDeliveryStateSourcingLocationFilter;
import com.bl.Ordermanagement.services.BlSourcingLocationService;
import com.bl.Ordermanagement.services.BlSourcingService;
import com.bl.Ordermanagement.strategy.BlSourcingStrategyService;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import com.google.common.base.Preconditions;
import com.google.common.collect.Sets;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import de.hybris.platform.warehousing.data.sourcing.SourcingContext;
import de.hybris.platform.warehousing.data.sourcing.SourcingLocation;
import de.hybris.platform.warehousing.data.sourcing.SourcingResults;
import de.hybris.platform.warehousing.sourcing.filter.SourcingFilterProcessor;
import de.hybris.platform.warehousing.sourcing.strategy.SourcingStrategy;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * SourcingService to source the order and create sourcing results.
 *
 * @author Sunil
 */
public class DefaultBlSourcingService implements BlSourcingService {

  private static final Logger LOG = Logger.getLogger(DefaultBlSourcingService.class);
  private BlSourcingStrategyService blSourcingStrategyService;
  private BlSourcingLocationService blSourcingLocationService;

  private BlDeliveryStateSourcingLocationFilter blDeliveryStateSourcingLocationFilter;

  /**
   * This is to source the order
   *
   * @param order the order
   * @return SourcingResults The SourcingResults
   */
  @Override
  public SourcingResults sourceOrder(final AbstractOrderModel order) {

    ServicesUtil.validateParameterNotNullStandardMessage("order", order);
    Preconditions.checkArgument(Objects.nonNull(order), "Parameter order cannot be null.");
    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Starting sourcing Order : {}",
        order.getCode());
    final Set<WarehouseModel> locations = Sets.newHashSet();
    blDeliveryStateSourcingLocationFilter.applyFilter(order, locations);
    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Total filtered sourcing locations found: {}",
        locations.size());

    final SourcingContext context = new SourcingContext();
    final SourcingResults result = new SourcingResults();
    result.setResults(Sets.newHashSet());
    result.setComplete(Boolean.FALSE);
    context.setResult(result);
    context.setOrderEntries(order.getEntries());

    final Set<SourcingLocation> sourcingLocations = Sets.newHashSet();
    locations.forEach(location ->
      sourcingLocations.add(blSourcingLocationService.createSourcingLocation(context, location))
    );

    final List<SourcingStrategy> strategies = this.blSourcingStrategyService.getDefaultStrategies();

    final Iterator<SourcingStrategy> strategyItr = strategies.iterator();
    try {
      while (strategyItr.hasNext()) {
        final SourcingStrategy strategy = strategyItr.next();
        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Apply sourcing strategy: {}",
            strategy.getClass().getSimpleName());

        strategy.source(context);

        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
            "Sourcing strategy '" + strategy.getClass().getSimpleName() + "' applied" + (
                context.getResult().isComplete() ? "" : " not") + " successfully");
        if (context.getResult().isComplete()) {
          break;
        }
      }
    } catch (final BlSourcingException e) {
      BlLogger.logMessage(LOG, Level.ERROR, LogErrorCodeEnum.ORDER_SOURCING_ERROR.getCode(), e);
      return null;
    }

    return context.getResult();
  }

  public BlSourcingLocationService getBlSourcingLocationService() {
    return blSourcingLocationService;
  }

  public void setBlSourcingLocationService(
      final BlSourcingLocationService blSourcingLocationService) {
    this.blSourcingLocationService = blSourcingLocationService;
  }

  public BlSourcingStrategyService getBlSourcingStrategyService() {
    return blSourcingStrategyService;
  }

  public void setBlSourcingStrategyService(
      final BlSourcingStrategyService blSourcingStrategyService) {
    this.blSourcingStrategyService = blSourcingStrategyService;
  }

  public BlDeliveryStateSourcingLocationFilter getBlDeliveryStateSourcingLocationFilter() {
    return blDeliveryStateSourcingLocationFilter;
  }

  public void setBlDeliveryStateSourcingLocationFilter(
      final BlDeliveryStateSourcingLocationFilter blDeliveryStateSourcingLocationFilter) {
    this.blDeliveryStateSourcingLocationFilter = blDeliveryStateSourcingLocationFilter;
  }
}
