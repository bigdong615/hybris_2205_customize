package com.bl.core.stock;

import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;

import java.util.Collection;
import java.util.Date;


/**
 * @author Moumita
 *
 * This method is used to fetch the data from stockLevel table
 */
public interface BlStockLevelDao
{
	/**
	 * This is to fetch the stock details of a SKU
	 *
	 * @param productCode the product code
	 * @param warehouses the list of warehouse associated to base store
	 * @param date the date
	 * @return Collection<StockLevelModel> The list of stockLevelModels associated to the SKU
	 */
	public Collection<StockLevelModel> findStockLevelForDate(final String productCode, final Collection<WarehouseModel> warehouses,
			final Date date);
}
