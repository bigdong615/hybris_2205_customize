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
   * Get ReadyToShipOrderItem for date and warehouse
   *
   * @param shipDate
   * @param warehouse
   * @return ReadyToShipOrderItemModels
   */
  List<ReadyToShipOrderItemModel> getReadyToShipOrderItemsForDateAndWarehouse(final Date shipDate, final
  WarehouseModel warehouse);

  /**
   * Get ReadyToShipOrderItems for warehouse
   *
   * @param warehouse
   * @return ReadyToShipOrderItemModels
   */
  List<ReadyToShipOrderItemModel> getReadyToShipOrderItemsForWarehouse(
      final WarehouseModel warehouse);
}
