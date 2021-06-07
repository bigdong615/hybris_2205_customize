package com.bl.Ordermanagement.filters;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.dao.warehouse.BlStateWarehouseMappingDao;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.warehousing.sourcing.filter.SourcingFilterResultOperator;
import de.hybris.platform.warehousing.sourcing.filter.impl.AbstractBaseSourcingLocationFilter;
import java.util.Collection;
import java.util.Set;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * Filter to find out exact warehouse matching with delivery address.
 *
 * @author Sunil
 */
public class BlDeliveryStateSourcingLocationFilter {

  private static final Logger LOG = Logger.getLogger(BlDeliveryStateSourcingLocationFilter.class);
  private BlStateWarehouseMappingDao blStateWarehouseMappingDao;

  /**
   * {@inheritDoc}
   */
  public Collection<WarehouseModel> applyFilter(final AbstractOrderModel order,
      final Set<WarehouseModel> locations) {

    String stateCode = BlCoreConstants.EMPTY_STRING;
    if (null != order.getDeliveryAddress() && null != order.getDeliveryAddress().getRegion()
        && null != order.getDeliveryAddress().getRegion().getIsocodeShort()) {
      stateCode = order.getDeliveryAddress().getRegion().getIsocodeShort();
    }

    final WarehouseModel foundLocation = blStateWarehouseMappingDao.getStateWarehouseForStateCode(stateCode).getWarehouse();

    if (null != foundLocation) {
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
          "Location found for state iso code {} is warehouse {}",
          stateCode, foundLocation.getCode());
      locations.add(foundLocation);
    }

    return locations;
  }

  public BlStateWarehouseMappingDao getBlStateWarehouseMappingDao() {
    return blStateWarehouseMappingDao;
  }

  public void setBlStateWarehouseMappingDao(final BlStateWarehouseMappingDao blStateWarehouseMappingDao) {
    this.blStateWarehouseMappingDao = blStateWarehouseMappingDao;
  }
}
