package com.bl.core.stock.impl;

import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.dao.BlProductDao;
import com.bl.core.stock.BlInventoryManageService;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.logging.BlLogger;


/**
 * It is used to create the stock level
 *
 * @author Moumita
 */
public class DefaultBlInventoryManageService implements BlInventoryManageService
{
	private static final Logger LOG = Logger.getLogger(DefaultBlInventoryManageService.class);
	private ModelService modelService;
	private BlProductDao blProductDao;
	private BlStockLevelDao blStockLevelDao;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void createStockLevelForAllSkus(final Date date)
	{
		final Collection<BlProductModel> skuProducts = getBlProductDao().getAllActiveSkuProducts();
		getAllAssociatedActiveSerials(skuProducts, date);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void createStockLevelForGivenSkus(final List<BlProductModel> skus, final Date date) {
		getAllAssociatedActiveSerials(skus, date);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void createStockLevelForADayForAllSkus() {
		final Collection<BlProductModel> skuProducts = getBlProductDao().getAllActiveSkuProducts();
		final Date currentDate = new Date();
		final Calendar calendar = Calendar.getInstance();
		calendar.setTime(currentDate);
		calendar.add(Calendar.YEAR, 1);
		getAllAssociatedActiveSerials(skuProducts, calendar.getTime());
	}

	/**
	 * It gets all the active associated serials of the sku
	 *
	 * @param skuProducts
	 * @param date
	 */
	private void getAllAssociatedActiveSerials(final Collection<BlProductModel> skuProducts, final Date date)
	{
		skuProducts.forEach(sku -> {
			final List<BlSerialProductModel> serialProducts = sku.getSerialProducts().stream()
					.filter(serialProduct -> serialProduct.getSerialStatus().equals(SerialStatusEnum.ACTIVE))
					.collect(Collectors.toList());
			serialProducts.forEach(serial -> {
				findAndCreateStockLevelForSerial(serial, sku.getCode(), date);
			});
		});
	}

	/**
	 * It first checks whether the stock already exists or not, if not, it creates the stock
	 *
	 * @param serial
	 * @param skuCode
	 * @param stockForTheDate
	 */
	private void findAndCreateStockLevelForSerial(final BlSerialProductModel serial, final String skuCode, final Date stockForTheDate) {
		final StockLevelModel stockLevelModel = getBlStockLevelDao().findSerialStockLevelForDate(serial.getCode(),
				skuCode, stockForTheDate);
		if(Objects.nonNull(stockLevelModel)) {
			BlLogger.logFormatMessageInfo(LOG, Level.WARN, "Stock already exist for product {}", serial.getCode());
		} else {
			createStockLevelForSerial(serial, skuCode, stockForTheDate);
		}
	}

	/**
	 * It creates the stock level for the sku and the serial
	 *
	 * @param serial
	 * @param skuCode
	 * @param date
	 */
	private void createStockLevelForSerial(final BlSerialProductModel serial, final String skuCode, final Date date) {
		if(null != serial.getWarehouseLocation()) {
			final Calendar calendar = BlDateTimeUtils
					.getFormattedDate(date, BlCoreConstants.START_HOURS, BlCoreConstants.START_MINUTES,
							BlCoreConstants.START_SECONDS);
			final StockLevelModel stockLevel = getModelService().create(StockLevelModel.class);
			stockLevel.setProductCode(skuCode);
			stockLevel.setWarehouse(serial.getWarehouseLocation());
			stockLevel.setAvailable(0);
			stockLevel.setOverSelling(0);
			stockLevel.setSerialProductCode(serial.getCode());
			stockLevel.setSerialStatus(serial.getSerialStatus());
			stockLevel.setDate(calendar.getTime());
			try
			{
				getModelService().save(stockLevel);
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Stock created for product {}", serial);
			}
			catch (final ModelSavingException ex)
			{
				if (LOG.isDebugEnabled())
				{
					LOG.error(ex);
				}
				BlLogger.logMessage(LOG, Level.ERROR, "Stock not created for product {} and "
						+ "the error {} ", serial.getCode(), ex);
			}
		}
	}

	/**
	 * @return the modelService
	 */
	public ModelService getModelService()
	{
		return modelService;
	}

	/**
	 * @param modelService
	 *           the modelService to set
	 */
	public void setModelService(final ModelService modelService)
	{
		this.modelService = modelService;
	}

	/**
	 * @return the blProductDao
	 */
	public BlProductDao getBlProductDao()
	{
		return blProductDao;
	}

	/**
	 * @param blProductDao the blProductDao to set
	 */
	public void setBlProductDao(final BlProductDao blProductDao)
	{
		this.blProductDao = blProductDao;
	}

	/**
	 * @return the blStockLevelDao
	 */
	public BlStockLevelDao getBlStockLevelDao()
	{
		return blStockLevelDao;
	}

	/**
	 * @param blStockLevelDao
	 *           the blStockLevelDao to set
	 */
	public void setBlStockLevelDao(final BlStockLevelDao blStockLevelDao)
	{
		this.blStockLevelDao = blStockLevelDao;
	}

}
