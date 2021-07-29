package com.bl.core.dao.warehouse;

import de.hybris.platform.ordersplitting.model.WarehouseModel;
import java.util.Date;

/**
 * It is used to get ReadyToShipOrderItem.
 *
 * @author Sunil
 */
public interface BlReadyToShipOrderItemDao {

  /**
   * Remove ReadyToShipOrderItem
   *
   * @param shipDate
   * @param warehouse
   * @return ReadyToShipOrderItemModels
   */
  void removeReadyToShipOrderItemsForDate(final Date shipDate, final
  WarehouseModel warehouse);

}
