package com.bl.core.stock.impl;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.dao.BlProductDao;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.core.stock.BlStockManageService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.logging.BlLogger;
import de.hybris.platform.catalog.enums.ArticleApprovalStatus;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import de.hybris.platform.servicelayer.model.ModelService;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


/**
 * It is used to create the stock level
 *
 * @author Moumita
 */
public class DefaultBlStockManageService implements BlStockManageService
{
	private static final Logger LOG = Logger.getLogger(DefaultBlStockManageService.class);
	private ModelService modelService;
	private BlProductDao productDao;
	private BlStockLevelDao blStockLevelDao;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void createStockLevelForSkus(final List<BlProductModel> skus, Date startDate, Date endDate)
	{
		final Collection<BlProductModel> skuProducts = getSkuProducts(skus);
		startDate = (BlDateTimeUtils.getFormattedStartDay(startDate)).getTime();
		endDate = (BlDateTimeUtils.getFormattedStartDay(endDate)).getTime();
		Date fromDate = startDate;
		final Date toDate = DateUtils.addDays(endDate, 1);
		final Date lastFutureDate = BlDateTimeUtils.getFormattedStartDay(DateUtils.addDays(new Date(), 365))
				.getTime();
		if (lastFutureDate.after(startDate) && lastFutureDate.after(toDate))
		{
			while (fromDate.before(toDate))
			{
				getAssociatedActiveSerialsAndCreateStockLevels(skuProducts, fromDate);
				fromDate = DateUtils.addDays(fromDate, 1);
			}
		}
	}

	/**
	 * It get the sku products
	 * @param skus
	 * @return Collection<BlProductModel>
	 */
	private Collection<BlProductModel> getSkuProducts(final List<BlProductModel> skus)
	{
		if (CollectionUtils.isEmpty(skus))
		{
			return getProductDao().getAllActiveSkuProducts();
		}
		return skus.stream().filter(sku -> sku.getApprovalStatus().equals(ArticleApprovalStatus.APPROVED))
				.collect(Collectors.toList());
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void createStockLevelForADayForAllSkus() {
		final Collection<BlProductModel> skuProducts = getProductDao().getAllActiveSkuProducts();
		final Date currentDate = new Date();
		getAllAssociatedActiveSerials(skuProducts, getNextYearsSameDay(currentDate));
	}

	/**
	 * It gets the date which is after a year
	 * @param currentDate
	 * @return the date
	 */
	private Date getNextYearsSameDay(final Date currentDate) {
		final Calendar calendar = Calendar.getInstance();
		calendar.setTime(currentDate);
		calendar.add(Calendar.YEAR, 1);
		return calendar.getTime();
	}

	/**
	 * It gets the active serials associated to the sku and creates stock for the serials
	 * @param skuProducts
	 * @param date
	 */
	private void getAssociatedActiveSerialsAndCreateStockLevels(final Collection<BlProductModel> skuProducts, final Date date)
	{
		final Collection<BlSerialProductModel> serialProducts = getProductDao().getAssociatedActiveSerials(skuProducts);
			serialProducts.forEach(serial -> {
				findAndCreateStockLevelForSerial(serial, serial.getBlProduct().getCode(), date);
			});
	}

	/**
	 * It gets all the active associated serials of the sku
	 *
	 * @param skuProducts
	 * @param date
	 */
	private void getAllAssociatedActiveSerials(final Collection<BlProductModel> skuProducts, final Date date)
	{
		final Collection<BlSerialProductModel> serialProducts = getProductDao().getAssociatedActiveSerials(skuProducts);
		serialProducts.forEach(serial -> {
			createStockLevelForSerial(serial, serial.getBlProduct().getCode(), date);
		});
	}

	/**
	 * It first checks whether the stock already exists or not, if not, it creates the stock
	 *
	 * @param serial
	 * @param skuCode
	 * @param date
	 */
	private void findAndCreateStockLevelForSerial(final BlSerialProductModel serial, final String skuCode,
			final Date date)
	{
		final StockLevelModel stockLevelModel = getBlStockLevelDao().findSerialStockLevelForDate(serial.getCode(), skuCode,
				date, date);
		if (Objects.nonNull(stockLevelModel))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.WARN, "Stock already exist for product {}", serial.getCode());
		}
		else
		{
			createStockLevelForSerial(serial, skuCode, date);
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
					.getFormattedStartDay(date);
			final StockLevelModel stockLevel = getModelService().create(StockLevelModel.class);
			stockLevel.setProductCode(skuCode);
			stockLevel.setWarehouse(serial.getWarehouseLocation());
			stockLevel.setAvailable(0);
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
	 * @return the productDao
	 */
	public BlProductDao getProductDao()
	{
		return productDao;
	}

	/**
	 * @param productDao
	 *           the productDao to set
	 */
	public void setProductDao(final BlProductDao productDao)
	{
		this.productDao = productDao;
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
