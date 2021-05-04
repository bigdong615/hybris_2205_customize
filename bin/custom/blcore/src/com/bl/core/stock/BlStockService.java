package com.bl.core.stock;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import de.hybris.platform.servicelayer.exceptions.BusinessException;
import java.util.Date;
import java.util.List;


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
	 * @throws BusinessException when given dates are not correct or dates are null
	 */
	public void createStockLevelForSkuProductsByDate(final List<BlProductModel> skuProducts, final Date startDate,
			final Date endDate) throws BusinessException;

	/**
	 * It creates the stock level for all active skus for a day which is after one year
	 */
	public void createOneDayStockLevelForAllSkuProducts();

	/**
	 * It creates the stock level for the new active serial products for present date to
	 * all the future dates
	 * @param blSerialProduct the serial product
	 */
	public void createStockRecordsForNewSerialProducts(BlSerialProductModel blSerialProduct);

	/**
	 * It finds the stock level from present date to all the future dates from current date and deletes all
	 * @param blSerialProduct the serial product
	 */
	public void findAndDeleteStockRecords(BlSerialProductModel blSerialProduct);

	/**
	 * It finds the stock level from present date to all the future dates and
	 * updates the reserved status
	 * @param blSerialProduct the serial product
	 * @param reservedStatus the status of of the serial product
	 */
	public void findAndUpdateStockRecords(BlSerialProductModel blSerialProduct, final boolean reservedStatus);

	/**
	 * It finds the stock level based on given start and end date and updates the reserved status attribute
	 * @param blSerialProduct the serial product
	 * @param reservedStatus the reserved status
	 * @param startDate the rental start date
	 * @param endDate the rental end date
	 */
	public void findAndUpdateStockRecordsForParticularDuration(BlSerialProductModel blSerialProduct, final boolean reservedStatus,
			final Date startDate, final Date endDate);

	/**
	 * It updates the warehouse in the stock records from present date to all the future dates
	 * @param blSerialProduct the serial product
	 */
	public void findAndUpdateWarehouseInStockRecords(final BlSerialProductModel blSerialProduct);
}
