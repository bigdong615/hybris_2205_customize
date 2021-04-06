package com.bl.core.stock;

import com.bl.core.data.StockResult;
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
	 * This is to get the stock details of a SKU for the duration
	 *
	 * @param productCode the product code
	 * @param warehouses the list of warehouse associated to base store
	 * @param startDate the start date
	 * @param startDate the end date
	 * @return Collection<StockLevelModel> The list of stockLevelModels associated to the SKU
	 */
	Collection<StockLevelModel> getStockForDate(String productCode, Collection<WarehouseModel> warehouses, Date startDate,
			Date endDate);

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

	/**
	 * This is to get the stock level status of a SKU for the entire duration and determines the total and available count
	 *
	 * @param warehouses the list of warehouse associated to base store
	 * @param productCode the product code
	 * @param startDate the start date of rental period
	 * @param endDate the end date of rental period
	 * @return StockLevelStatus
	 */
	public StockResult getStockForEntireDuration(final String productCode, final Collection<WarehouseModel> warehouses,
			final LocalDateTime startDate, LocalDateTime endDate);
}
