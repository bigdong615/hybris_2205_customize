package com.bl.core.stock;

import com.bl.core.model.BlProductModel;
import de.hybris.platform.basecommerce.enums.StockLevelStatus;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;

import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;

import com.bl.core.data.StockResult;
import com.bl.facades.product.data.RentalDateDto;
import java.util.Set;


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
	 * This is to get the stock details for a collection of SKUs for the given date range
	 *
	 * @param productCodes the product code
	 * @param warehouse the warehouse
	 * @param startDate the start date
	 * @param endDate the end date
	 * @return Collection<StockLevelModel> The list of stockLevelModels associated to the SKU
	 */
	Collection<StockLevelModel> getStockForProductCodesAndDate(final Set<String> productCodes, final WarehouseModel warehouse,
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
	public boolean isUsedGearSerialNotAssignedToRentalOrder(final String productCode);

	/**
	 * Gets the next availability date for the product against the quantity to check.
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
	 * Gets the list of stocks for product codes and dates.
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
	 * Grouping the products availability by product codes.
	 *
	 * @param startDate
	 *           the start date
	 * @param endDate
	 *           the end date
	 * @param lProductCodes
	 *           the l product codes
	 * @param warehouses
	 *           the warehouses
	 * @return the map
	 */
	Map<String, Long> groupByProductsAvailability(final Date startDate, final Date endDate, final List<String> lProductCodes,
			final List<WarehouseModel> warehouses);

	/**
	 * Gets the next availability date for the product on checkout by subtracting one day from available date.
	 *
	 * @param productCode
	 *           the product code
	 * @param rentalDates
	 *           the rental dates
	 * @param warehouses
	 *           the warehouses
	 * @param qtyToCheck
	 *           the qty to check
	 * @return the next availability date
	 */
	String getNextAvailabilityDateInCheckout(final String productCode, final RentalDateDto rentalDates,
			final Collection<WarehouseModel> warehouses, final int qtyToCheck);

	/**
	 * Gets the next availability date for the product on PDP.
	 *
	 * @param productCode
	 *           the product code
	 * @param rentalDate
	 *           the rental date
	 * @return the next availability date for PDP
	 */
	String getNextAvailabilityDateInPDP(final String productCode, final RentalDateDto rentalDate);
	
	/**
	 * This method returns the map of sku product with their stock levels. It takes all stock levels
	 * and group it by product sku with their respective stock levels and add it in Map.
	 *
	 * @param stockLevels the stockLevels
	 * @return map of stock levels group by product codes.
	 */
	public Map<String, List<StockLevelModel>> groupBySkuProductWithAvailability(
			final Collection<StockLevelModel> stockLevels);

	/**
	 * this method created to get stock duration for same serial includes in active rental for extending order
	 * @param productCode the product code
	 * @param warehouses the warehouse
	 * @param startDate start date
	 * @param endDate end date
	 * @return Stock Result
	 */
	 StockResult getStockForEntireExtendDuration(final String productCode, final Collection<WarehouseModel> warehouses,
			final Date startDate, final Date endDate);

	/**
	 * This is to get the stock level status of a Bundle SKU for the entire duration and determines the total and available count.
	 * @param blProductModel the product code
	 * @param warehouses the list of warehouse associated to base store
	 * @param startDate the start date of rental period
	 * @param endDate the end date of rental period
	 * @return StockLevelStatus
	 */
	 StockResult getStockForBundleProduct(final BlProductModel blProductModel, final Collection<WarehouseModel> warehouses,
			final Date startDate, final Date endDate);

	/**
	 * This is to get the available count of a bundle
	 * @param productModel product
	 * @param warehouses store
	 * @param startDate rental start date
	 * @param endDate rental end date
	 * @return availability
	 */
	public Long getAvailableCountForBundle(final BlProductModel productModel, final Collection<WarehouseModel> warehouses,
			final Date startDate, final Date endDate);

	/**
	 * It gets the product with available quantity
	 * @param productCodes
	 * @param warehouses the list of warehouse
	 * @param startDate the start date
	 * @param endDate the end date
	 * @return map which is product with quantity
	 */
	public Map<String, Long> getStockForUnallocatedProduct(final List<String> productCodes,
			final List<WarehouseModel> warehouses, final Date startDate, final Date endDate);

	String getNextAvailableDateForSelectedDuration(final String productCode,
			final int qtyToCheck,final RentalDateDto rentalDates );
}
