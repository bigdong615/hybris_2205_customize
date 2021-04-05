package com.bl.core.stock;

import de.hybris.platform.basecommerce.enums.StockLevelStatus;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;

import java.time.LocalDateTime;
import java.util.Collection;
import java.util.Date;


/**
 * @author Moumita
 * This class is used to get the inventory for a product
 */
public interface BlCommerceStockService
{
	/**
	 * This is to get the stock details of a SKU
	 *
	 * @param productCode the product code
	 * @param warehouses the list of warehouse associated to base store
	 * @param convertedDate the date
	 * @return Collection<StockLevelModel> The list of stockLevelModels associated to the SKU
	 */
	Collection<StockLevelModel> getStockForDate(String productCode, Collection<WarehouseModel> warehouses, Date convertedDate);

	/**
	 * This is to get the stock level status of a SKU
	 *
	 * @param warehouses the list of warehouse associated to base store
	 * @param productCode the product code
	 * @param startDate the start date of rental period
	 * @param endDate the end date of rental period
	 * @return StockLevelStatus
	 */
	public StockLevelStatus getStockLevelStatus(final Collection<WarehouseModel> warehouses, final String productCode,
			final LocalDateTime startDate, final LocalDateTime endDate);
}
