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
public interface BlStockManageService
{
	/**
	 * It creates the stock level for the given skus in cron job
	 *
	 * @param skus
	 *           the sku products
	 * @param startDate
	 *           the stock will be created from this date
	 * @param endDate
	 *           the stock will be created till this date
	 */
	public void createStockLevelForSkus(final List<BlProductModel> skus, final Date startDate, final Date endDate);

	/**
	 * It creates the stock level for all active skus for a day which is after one year
	 */
	public void createStockLevelForADayForAllSkus();
}
