package com.bl.core.dao.warehouse;

import com.bl.core.model.ReadyToShipOrderItemModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import java.util.Date;
import java.util.List;

/**
 * It is used to get ReadyToShipOrderItem.
 *
 * @author Sunil
 */
public interface BlReadyToShipOrderItemDao {

  /**
   * Get ReadyToShipOrderItem
   *
   * @param shipDate
   * @param warehouse
   * @return ReadyToShipOrderItemModels
   */
  List<ReadyToShipOrderItemModel> getReadyToShipOrderItemsForDate(final Date shipDate, final
  WarehouseModel warehouse);

}
