package com.bl.core.stock;

import de.hybris.platform.basecommerce.enums.StockLevelStatus;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;

import java.util.Collection;
import java.util.Date;
import java.util.List;

import com.bl.core.data.StockResult;
import com.bl.facades.product.data.RentalDateDto;


/**
 * This class is used to get the inventory for a product
 * @author Moumita
 */
public interface BlCommerceStockService
{
	/**
	 * This is to get the stock details of a SKU for the duration
	 *
	 * @param productCode the product code
	 * @param warehouses the list of warehouse associated to base store
	 * @param startDate the start date
	 * @param endDate the end date
	 * @return Collection<StockLevelModel> The list of stockLevelModels associated to the SKU
	 */
	Collection<StockLevelModel> getStockForDate(final String productCode, final Collection<WarehouseModel> warehouses,
			final Date startDate, final Date endDate);

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
			final Date startDate, final Date endDate);

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
			final Date startDate, final Date endDate);

	/**
	 * This is to get the available count of a SKU
	 *
	 * @param warehouses
	 *           the list of warehouse associated to base store
	 * @param productCode
	 *           the product code
	 * @param startDate
	 *           the start date of rental period
	 * @param endDate
	 *           the end date of rental period
	 * @return Long
	 */
	public Long getAvailableCount(final String productCode, final Collection<WarehouseModel> warehouses,
			final Date startDate, final Date endDate);

	/**
	 * It checks whether the serial product is not assigned to any rental orders.
	 *
	 * @param serialProductCode           the serial product code
	 * @param productCode           the sku product code
	 * @return boolean
	 */
	public boolean isUsedGearSerialNotAssignedToRentalOrder(final String serialProductCode, final String productCode);
	
	/**
	 * Gets the next availability date.
	 *
	 * @param productCode
	 *           the product code
	 * @param dates
	 *           the dates
	 * @param warehouses
	 *           the warehouses
	 * @param qtyToCheck
	 *           the qty to check
	 * @return the next availability date
	 */
	public Date getNextAvailabilityDate(final String productCode, final RentalDateDto dates,
			final Collection<WarehouseModel> warehouses, final int qtyToCheck);

	/**
	 * Gets the stock for product codes and date.
	 *
	 * @param productCodes
	 *           the product codes
	 * @param warehouses
	 *           the warehouses
	 * @param startDate
	 *           the start date
	 * @param endDate
	 *           the end date
	 * @return the stock for product codes and date
	 */
	Collection<StockLevelModel> getStockForProductCodesAndDate(final List<String> productCodes,
			final List<WarehouseModel> warehouses, final Date startDate, final Date endDate);

	/**
	 * Collect available qty.
	 *
	 * @param startDate
	 *           the start date
	 * @param endDate
	 *           the end date
	 * @param stockLevels
	 *           the stock levels
	 * @param availability
	 *           the availability
	 * @param totalUnits
	 *           the total units
	 * @param productCode
	 *           the product code
	 */
	void collectAvailableQty(final Date startDate, final Date endDate, final Collection<StockLevelModel> stockLevels,
			final List<Long> availability, final List<Long> totalUnits, final String productCode);
	
}
