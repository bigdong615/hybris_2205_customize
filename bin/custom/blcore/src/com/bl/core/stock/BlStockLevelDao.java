package com.bl.core.stock;

import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.solrfacetsearch.model.config.SolrFacetSearchConfigModel;

import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Set;


/**
 * This method is used to fetch the data from stockLevel table
 * @author Moumita
 */
public interface BlStockLevelDao {

	/**
	 * This is to fetch the stock details of a SKU
	 *
	 * @param productCode the product code
	 * @param warehouses  the list of warehouse associated to base store
	 * @param startDate   the rental start date
	 * @param endDate     the rental end date
	 * @return Collection<StockLevelModel> The list of stockLevelModels associated to the SKU
	 */
	public Collection<StockLevelModel> findStockLevelForDate(final String productCode,
			final Collection<WarehouseModel> warehouses,
			final Date startDate, final Date endDate);

	/**
	 * It finds the stock for the given sku and serial from start date to end date
	 *
	 * @param serialProductCode
	 * @param startDate         the rental start date
	 * @param endDate           the rental end date
	 * @return list of stock levels
	 */
	public Collection<StockLevelModel> findSerialStockLevelForDate(final String serialProductCode,
			final Date startDate, final Date endDate);

	/**
	 * It finds the stock for the given sku and serial from start date to end date
	 *
	 * @param serialProductCode
	 * @param startDate         the rental start date
	 * @param endDate           the rental end date
	 * @return list of stock levels
	 */
	public Collection<StockLevelModel> findSerialStockLevelForDateFromNonBufferInv(final String serialProductCode,
			final Date startDate, final Date endDate);

	/**
	 * It finds the stock for the used gear serial
	 *
	 * @param serialProductCode the serial product code
	 * @return list of stock levels
	 */
	public StockLevelModel findStockLevelForUsedGearSerial(final String serialProductCode);

	/**
	 * It checks whether the serial product is not assigned to any rental orders
	 *
	 * @param serialProductCode the serial product code
	 * @param startDate         the start date
	 * @param endDate           the end date
	 * @return boolean
	 */
	public boolean isUsedGearSerialNotAssignedToAnyRentalOrders(final String serialProductCode,
			final Date startDate, final Date endDate);

	/**
	 * Find stock levels for list of products and date.
	 *
	 * @param productCodes the product codes
	 * @param warehouses   the warehouses
	 * @param startDate    the start date
	 * @param endDate      the end date
	 * @return the collection
	 */
	public Collection<StockLevelModel> findStockLevelsForProductCodesAndDate(
			List<String> productCodes,
			List<WarehouseModel> warehouses, Date startDate, Date endDate);

	/**
	 * It finds the stock levels for the given product codes and date range.
	 *
	 * @param productCodes the product codes
	 * @param warehouse    the warehouse
	 * @param startDate    the start date
	 * @param endDate      the end date
	 * @return list of stock levels
	 */
	public Collection<StockLevelModel> findStockLevelsForProductCodesAndDate(final Set<String> productCodes,
			final WarehouseModel warehouse, final Date startDate, final Date endDate);

	/**
	 * It finds the stocks for the given serials and serial from start date to end date
	 *
	 * @param serialProductCodes
	 * @param startDay           the rental start date
	 * @param endDay             the rental end date
	 * @return list of stock levels
	 */
	public Collection<StockLevelModel> findSerialStockLevelsForDateAndCodes(
			final Set<String> serialProductCodes, final Date startDay, final Date endDay, final Boolean reservedStatus);

	/**
	 * This method created to find the stock level for extended order based on extend start date and extend end date
	 */
	Collection<StockLevelModel> findSerialStockLevelForExtendDate(final String serialProductCode, final Collection<WarehouseModel> warehouseModels,
			final Date startDay, final Date endDay);

	/**
	 * It checks whether the product is available in present date to be marked as buffer inventory
	 * @param serialProductCode
	 * @param startDay
	 * @param endDay
	 * @return Collection<StockLevelModel> list of stock level model
	 */
	public Collection<StockLevelModel> checkProductAvailabilityForCurrentDate(final String serialProductCode,
			final Date startDay, final Date endDay);

	/**
	 * It checks whether the product is available in present date to be marked as buffer inventory
	 * @param warehouseModel the warehouse model
	 * @param startDay the start day
	 * @param endDay the end day
	 * @return Collection<StockLevelModel> list of stock level model
	 */
	public Collection<StockLevelModel> reserveProductsBelongToWHForSpecifiedDate(final WarehouseModel warehouseModel,
			final Date startDay, final Date endDay);

	/**
	 * It finds all the stocks for the given serials and serial from start date to end date
	 *
	 * @param serialProductCodes
	 * @param startDay           the rental start date
	 * @param endDay             the rental end date
	 * @return list of stock levels
	 */
	public Collection<StockLevelModel> findALLSerialStockLevelsForDateAndCodes(
			final Set<String> serialProductCodes, final Date startDay, final Date endDay);

	/**
	 * It gets the stock
	 * @param productCodes list of product code
	 * @param warehouses list of warehouse
	 * @param startDate the start date
	 * @param endDate the end date
	 * @return list of stock level model
	 */
	public Collection<StockLevelModel> getStockForUnallocatedProduct(final List<String> productCodes,
			final List<WarehouseModel> warehouses, final Date startDate, final Date endDate);

	/**
	 * It checks whether assigned serials of the order are soft-assigned or not
	 * @param orderCodes list of order code
	 * @return list of stock level model
	 */
	public List<StockLevelModel> getStocksOfSoftAssignedSerialsOfOrders(final Set<String> orderCodes);

	/**
	 * @param serialProductCodes
	 * @param startDay
	 * @param endDay
	 * @param reservedStatus
	 * @return
	 */
	public Collection<StockLevelModel> findSerialStockLevelsForDateAndCodesForWarehouse(Set<String> serialProductCodes,
			Date startDay,
			Date endDay, Boolean reservedStatus, WarehouseModel warehouseModel);

	public SolrFacetSearchConfigModel getFacetConfigModel();
}
