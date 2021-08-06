package com.bl.Ordermanagement.filters;

import com.bl.core.dao.warehouse.BlStateWarehouseMappingDao;
import com.bl.core.model.BlPickUpZoneDeliveryModeModel;
import com.bl.core.model.BlStateWarehouseMappingModel;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
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
  public WarehouseModel applyFilter(final AbstractOrderModel order) {

    if (order.getDeliveryMode() instanceof BlPickUpZoneDeliveryModeModel) {

      return ((ZoneDeliveryModeModel) order.getDeliveryMode()).getWarehouse();
    } else if (null != order.getDeliveryAddress() && null != order.getDeliveryAddress().getRegion()
        && null != order.getDeliveryAddress().getRegion().getIsocodeShort()) {
      final String stateCode = order.getDeliveryAddress().getRegion().getIsocodeShort();

      final BlStateWarehouseMappingModel blStateWarehouseMappingModel = blStateWarehouseMappingDao.getStateWarehouseForStateCode(stateCode);

      if (null != blStateWarehouseMappingModel) {
        final WarehouseModel foundLocation = blStateWarehouseMappingModel.getWarehouse();
        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
            "Location found for state iso code {} is warehouse {}", stateCode,
            foundLocation.getCode());

        return foundLocation;
      }
      BlLogger.logFormatMessageInfo(LOG, Level.ERROR, "Warehouse not found for state code : {}", stateCode);
      return null;
    }

    BlLogger.logFormatMessageInfo(LOG, Level.ERROR, "State code and delivery address is null");
    return null;
  }

  public BlStateWarehouseMappingDao getBlStateWarehouseMappingDao() {
    return blStateWarehouseMappingDao;
  }

  public void setBlStateWarehouseMappingDao(final BlStateWarehouseMappingDao blStateWarehouseMappingDao) {
    this.blStateWarehouseMappingDao = blStateWarehouseMappingDao;
  }
}
