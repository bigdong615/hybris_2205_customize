package com.bl.core.stock;

import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.exceptions.BusinessException;

import java.util.Date;
import java.util.List;
import java.util.Set;

import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;


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
	public void createStockRecordsForNewSerialProducts(final BlSerialProductModel blSerialProduct);

	/**
	 * It finds the stock level from present date to all the future dates and deletes all
	 *
	 * @param blSerialProduct
	 *           the serial product
	 */
	public void findAndDeleteStockRecords(final BlSerialProductModel blSerialProduct);

	/**
	 * It finds the stock level from present date to all the future dates and
	 * updates the reserved status
	 * @param blSerialProduct the serial product
	 * @param reservedStatus the status of of the serial product
	 */
	public void findAndUpdateStockRecords(final BlSerialProductModel blSerialProduct, final boolean reservedStatus);

	/**
	 * It updates the warehouse in the stock records from present date to all the future dates
	 * @param blSerialProduct the serial product
	 */
	public void findAndUpdateWarehouseInStockRecords(final BlSerialProductModel blSerialProduct);

	/**
	 * This method created to find the stock level for same serial from existing rental for extend order
	 * @param serialCode serial product code
	 * @param startDate start date
	 * @param endDate end date
	 */
	void findStockLevelForExtendOrderSerialProducts(final String serialCode , final Date startDate , final Date endDate);

	/**
	 * This method created to find the stock level for the serial and update buffer inventory flag
	 * @param blSerialProduct the serial product model
	 */
	void findAndUpdateBufferInvInStockRecords(final BlSerialProductModel blSerialProduct);

	void releaseStockForGivenSerial(final Set<String> productsCode, final Date startDate,final Date endDate, final String orderCode);
	void removeOrderFromStock(final Set<String> productsCode,final SerialStatusEnum status,final Date startDate,final Date endDate,final String orderCode);

	/**
	 * It defines the active status based on serial status of the product
	 * @param currentStatus the serial status
	 * @return boolean
	 */
	boolean isActiveStatus(final SerialStatusEnum currentStatus);

	/**
	 * It defines the inactive status based on serial status of the product
	 * @param currentStatus the serial status
	 * @return boolean
	 */
	boolean isInactiveStatus(final SerialStatusEnum currentStatus);

	/**
	 * It reserves all the products belong to that warehouse for the specified date range
	 * @param warehouseModel the warehouse model
	 * @param interceptorContext interceptor context
	 */
	void reserveProductsBelongToWHForSpecifiedDate(final WarehouseModel warehouseModel);


	/**
	 * It finds the stock level from present date to all the future dates and updates the serial code
	 *
	 * @param blSerialProduct
	 *           the serial product
	 * @param reservedStatus
	 *           the status of of the serial product
	 */
	public void findAndUpdateStockRecordsForSerialCode(final BlSerialProductModel blSerialProduct, String intialCode);

	boolean isVisibleInPdp(final SerialStatusEnum currentStatus);
	void findAndUpdateAllStock(final BlSerialProductModel blSerialProduct,Boolean reservedStatus);

}
