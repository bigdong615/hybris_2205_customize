package com.bl.core.stock;

import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;

import java.util.Collection;
import java.util.Date;


/**
 * This method is used to fetch the data from stockLevel table
 * @author Moumita
 */
public interface BlStockLevelDao
{
	/**
	 * This is to fetch the stock details of a SKU
	 *
	 * @param productCode the product code
	 * @param warehouses the list of warehouse associated to base store
	 * @param startDate the rental start date
	 * @param endDate the rental end date
	 * @return Collection<StockLevelModel> The list of stockLevelModels associated to the SKU
	 */
	public Collection<StockLevelModel> findStockLevelForDate(final String productCode, final Collection<WarehouseModel> warehouses,
			final Date startDate, final Date endDate);

	/**
	 * It finds the stock for the given sku and serial from start date to end date
	 *
	 * @param serialProductCode
	 * @param productCode
	 * @param startDate
	 *           the rental start date
	 * @param endDate
	 *           the rental end date
	 * @return list of stock levels
	 */
	public Collection<StockLevelModel> findSerialStockLevelForDate(final String serialProductCode, final String productCode, final Date startDate,
			final Date endDate);

	/**
	 * It finds the stock for the used gear serial
	 *
	 * @param serialProductCode
	 *           the serial product code
	 * @param productCode
	 *           the sku product code
	 * @return list of stock levels
	 */
	public Collection<StockLevelModel> findUsedGearSerialStockLevel(final String serialProductCode,
			final String productCode);
}
