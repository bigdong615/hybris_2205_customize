package com.bl.core.stock;

import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;

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
	public Collection<StockLevelModel> findStockLevelsForProductCodesAndDate(Set<String> productCodes,
			WarehouseModel warehouse, Date startDate, Date endDate);

	/**
	 * It finds the stocks for the given serials and serial from start date to end date
	 *
	 * @param serialProductCodes
	 * @param startDay           the rental start date
	 * @param endDay             the rental end date
	 * @return list of stock levels
	 */
	public Collection<StockLevelModel> findSerialStockLevelsForDateAndCodes(
			final Set<String> serialProductCodes,
			final Date startDay, final Date endDay);

}
