package com.bl.Ordermanagement.reshuffler.service;

import com.bl.core.model.BlSerialProductModel;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
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

/**
 * This class is used to optimize 'ship from' WH for the consignments
 * @author Moumita
 */
public interface BlOptimizeShippingFromWHService
{
  /**
   * It processes incomplete orders
   * @param date the present date
   */
  void optimizeShipFormWHForOrders(final Date date);

  /**
   * It gets the other warehouse which is not preferred as per state mapping of delivery address
   *
   * @param warehouses list of warehouse
   * @param preferredWH the preferred warehouse
   * @return the warehouse model
   */
  public WarehouseModel getAnotherWarehouse(final Collection<WarehouseModel> warehouses,
      final WarehouseModel preferredWH);

  /**
   * It gets the products from the consignment of the other warehouse
   * @param order the order
   * @param otherWH the other warehouse
   * @return list of product code
   */
  public Set<String> modifyProductCodes(final AbstractOrderModel order,
      final WarehouseModel otherWH);

  /**
   * It gets the stock for the products from the given warehouse
   * @param productCodes the product codes
   * @param location the warehouse
   * @param order the order
   * @return list of stock level model
   */
  public Collection<StockLevelModel> getStocks(final Set<String> productCodes,
      final WarehouseModel location,
      final AbstractOrderModel order);

  /**
   * It gets the consignment from the given warehouse
   * @param location the warehouse
   * @param order the order
   * @return the consignment model
   */
  public ConsignmentModel getConsignment(final WarehouseModel location,
      final AbstractOrderModel order);

  /**
   * It gets the availability for a product
   * @param skuProduct the product
   * @param availabilityMap the availability map
   * @return the available quantity
   */
  public Long getAvailabilityForProduct(final String skuProduct, final
  Map<String, List<StockLevelModel>> availabilityMap);

  /**
   * It gets the order entries
   * @param order the order
   * @param modifiedProductCodes the list of product code
   * @return list of order entry model
   */
  public Collection<AbstractOrderEntryModel> getOrderEntries(final AbstractOrderModel order,
      final Set<String> modifiedProductCodes);

  /**
   * It creates sourcing location
   * @param availabilityMap the availability map
   * @param warehouseModel the warehouse model
   * @param context the context
   * @return Sourcing location
   */
  public SourcingLocation createSourcingLocation(
      final Map<String, List<StockLevelModel>> availabilityMap,
      final WarehouseModel warehouseModel, final SourcingContext context);

  /**
   * It creates sourcing context
   * @param orderEntries the order entries
   * @return sourcing context
   */
  public SourcingContext createSourcingContext(
      final Collection<AbstractOrderEntryModel> orderEntries);

  /**
   * It deletes the consignment if all the products can be fulfilled from the other warehouse
   * @param order the order
   * @param anotherWH another warehouse
   */
  public void deleteOtherConsignmentIfAny(final AbstractOrderModel order,
      final WarehouseModel anotherWH);

  /**
   * It creates a consignment
   * @param order the order
   * @param context the context
   * @param warehouseModel the warehouse
   */
  public void createConsignment(final AbstractOrderModel order, final SourcingContext context,
      final WarehouseModel warehouseModel);

  /**
   * It updates the consignment entry which is already created
   * @param entry the entry
   * @param result the sourcing result
   * @param orderEntry the order entry
   */
  public void updateConsignmentEntry(final ConsignmentEntryModel entry,
      final SourcingResult result,
      final AbstractOrderEntryModel orderEntry);

  /**
   * It reserves the stocks for the assigned serial products
   * @param serialProductModels set of serial product model
   * @param entry the consignment entry
   */
  public void reserveStocksForSerialProductsThroughReshuffler(final Set<BlSerialProductModel> serialProductModels,
      final ConsignmentEntryModel entry);
}
