package com.bl.Ordermanagement.reallocation;

import com.bl.core.model.BlSerialProductModel;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.orderprocessing.model.OrderProcessModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.warehousing.data.sourcing.SourcingContext;
import de.hybris.platform.warehousing.data.sourcing.SourcingLocation;
import de.hybris.platform.warehousing.data.sourcing.SourcingResult;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

public interface BlReallocationService {

  /**
   * It creates sourcing context
   * @param orderEntries the order entries
   * @return sourcing context
   */
  SourcingContext createSourcingContext(final Collection<AbstractOrderEntryModel> orderEntries);

  /**
   * It creates sourcing location
   * @param availabilityMap the availability map
   * @param warehouseModel the warehouse model
   * @param context the context
   * @return Sourcing location
   */
  SourcingLocation createSourcingLocation(
      final Map<String, List<StockLevelModel>> availabilityMap,
      final WarehouseModel warehouseModel, final SourcingContext context);

  /**
   * It assigns the serial as per serial priority rules
   * @param context
   */
  void assignSerialFromLocation(final SourcingContext context);

  /**
   * It creates a consignment
   * @param order the order
   * @param context the context
   * @param warehouseModel the warehouse
   */
  void createConsignment(final AbstractOrderModel order, final SourcingContext context,
      final WarehouseModel warehouseModel);

  /**
   * It updates the consignment entry which is already created
   * @param entry the entry
   * @param result the sourcing result
   * @param orderEntry the order entry
   */
  void updateConsignmentEntry(final ConsignmentEntryModel entry,
      final SourcingResult result,
      final AbstractOrderEntryModel orderEntry);

  /**
   * It reserves the stocks for the assigned serial products
   * @param serialProductModels set of serial product model
   * @param entry the consignment entry
   */
  void reserveStocksForSerialProducts(final Set<BlSerialProductModel> serialProductModels,
      final ConsignmentEntryModel entry);

  /**
   * Create and start a consignment process for each consignment in the collection.
   *
   * @param consignments - list of consignments; never <tt>null</tt>
   * @param process      - order process model
   */
  void startConsignmentSubProcess(final Collection<ConsignmentModel> consignments,
      final OrderProcessModel process);

  void removeReserveStocksForSerialProducts(Set<String> serialProductCodes, Date startDay, Date endDay, Boolean reservedStatus,
		WarehouseModel warehouse);
}
