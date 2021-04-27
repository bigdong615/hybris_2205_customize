package com.bl.core.stock;

import java.util.Date;
import java.util.List;

import com.bl.core.model.BlProductModel;


/**
 * It is used to create the stock level
 *
 * @author Moumita
 *
 */
public interface BlStockService
{
	/**
	 * It creates the stock level for the given skus from start date to end date in cron job
	 *
	 * @param skuProducts
	 *           the sku products
	 * @param startDate
	 *           the stock will be created from this date
	 * @param endDate
	 *           the stock will be created till this date
	 */
	public void createStockLevelForSkuProductsByDate(final List<BlProductModel> skuProducts, final Date startDate, final Date endDate);

	/**
	 * It creates the stock level for all active skus for a day which is after one year
	 */
	public void createOneDayStockLevelForAllSkuProducts();
}
