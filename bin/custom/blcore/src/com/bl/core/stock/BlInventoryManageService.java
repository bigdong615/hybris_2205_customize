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
public interface BlInventoryManageService
{
	/**
	 * It creates the stock level for all active skus for the date
	 *
	 * @param date
	 *           the stock will be created for the date
	 */
	public void createStockLevelForAllSkus(final Date date);

	/**
	 * It creates the stock level for the given skus in cron job
	 *
	 * @param skus
	 *           the sku products
	 * @param date
	 *           the stock will be created for the date
	 */
	public void createStockLevelForGivenSkus(final List<BlProductModel> skus, final Date date);

	/**
	 * It creates the stock level for all active skus for a day which is after one year
	 */
	public void createStockLevelForADayForAllSkus();
}
