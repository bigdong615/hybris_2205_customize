package com.bl.core.inventory.order.service;

import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import java.util.List;

/**
 * Service for ReadyToShipOrderItem
 *
 * @author Sunil Sahu
 */
public interface BlReadyToShipOrderItemService {

  /**
   * This method will create and save ReadyToShipOrderItems in DB
   *
   * @param consignmentModels the consignmentModels
   * @param membersCount      the membersCount
   */
  void createReadyToShipOrderItems(final List<ConsignmentModel> consignmentModels,
      final Integer membersCount);

  /**
   * This method will remove all  ReadyToShipOrderItems for specified warehouse.
   *
   * @param warehouse the warehouse
   */
  void removeReadyToShipOrderItemsForWarehouse(final WarehouseModel warehouse);

}
