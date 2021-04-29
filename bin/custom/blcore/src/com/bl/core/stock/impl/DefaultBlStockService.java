package com.bl.core.stock.impl;

import de.hybris.platform.catalog.enums.ArticleApprovalStatus;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.servicelayer.exceptions.BusinessException;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import de.hybris.platform.servicelayer.model.ModelService;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.dao.BlProductDao;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.core.stock.BlStockService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.logging.BlLogger;


/**
 * It is used to create the stock level
 *
 * @author Moumita
 */
public class DefaultBlStockService implements BlStockService
{
	private static final Logger LOG = Logger.getLogger(DefaultBlStockService.class);
	private ModelService modelService;
	private BlProductDao productDao;
	private BlStockLevelDao blStockLevelDao;

	/**
	 * {@inheritDoc}
	 *
	 * @throws BusinessException
	 */
	@Override
	public void createStockLevelForSkuProductsByDate(final List<BlProductModel> skus, Date startDate, Date endDate)
			throws BusinessException
	{
		if (null != startDate && null != endDate && startDate.before(endDate)) {
			startDate = (BlDateTimeUtils.getFormattedStartDay(startDate)).getTime();
			endDate = (BlDateTimeUtils.getFormattedStartDay(endDate)).getTime();
			endDate = DateUtils.addDays(endDate, 1);
			final Date lastFutureDate = BlDateTimeUtils
					.getFormattedStartDay(DateUtils.addDays(new Date(), 365))
					.getTime();
			if (lastFutureDate.after(startDate) && lastFutureDate.after(endDate)) {
				final Collection<BlProductModel> skuProducts = getSkuProducts(skus);
				createStockLevelForSerialProductsForGivenDates(skuProducts, startDate, endDate);
			} else {
				BlLogger.logFormatMessageInfo(LOG, Level.WARN,
						"Stock can only be created till 365 days from today"
								+ " and the selected duration from start date {} and end date {} is beyond 365 days",
						startDate, endDate);
			}
		}
		else
		{
			throw new BusinessException("Start and end date can not be null Or Start date should be before end date");
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
	public void createOneDayStockLevelForAllSkuProducts() {
		final Collection<BlProductModel> skuProducts = getProductDao().getAllActiveSkuProducts();
		createStockLevelForAllActiveSerialProducts(skuProducts, getNextYearsSameDay());
	}

	/**
	 * It gets the date which is after a year
	 * @return the date
	 */
	private Date getNextYearsSameDay() {
		final Date currentDate = new Date();
		final Calendar calendar = Calendar.getInstance();
		calendar.setTime(currentDate);
		calendar.add(Calendar.YEAR, 1);
		return calendar.getTime();
	}

	/**
	 * It gets the active serials associated to the sku and creates stock for the serials
	 *
	 * @param skuProducts
	 * @param fromDate
	 * @param toDate
	 */
	private void createStockLevelForSerialProductsForGivenDates(final Collection<BlProductModel> skuProducts, final Date fromDate,
			final Date toDate)
	{
		final LocalDate formattedStartDate = fromDate.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
		final LocalDate formattedEndDate = toDate.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
		for (final BlProductModel skuProduct : skuProducts)
		{
			for (final BlSerialProductModel serial : skuProduct.getSerialProducts())
			{
				createStockForActiveSerials(formattedStartDate, formattedEndDate, serial);
			}
		}
	}

	/**
	 * It creates the stocks for the active serials
	 *
	 * @param formattedStartDate
	 * @param formattedEndDate
	 * @param serial
	 */
	private void createStockForActiveSerials(final LocalDate formattedStartDate, final LocalDate formattedEndDate,
			BlSerialProductModel serial) {
		if ((SerialStatusEnum.ACTIVE).equals(serial.getSerialStatus()) && null != serial.getWarehouseLocation())
		{
			if (Boolean.FALSE.equals(serial.getForRent()))
			{
				findAndCreateStockLevelForUsedGearSerial(serial);
			}
			else
			{
				Stream.iterate(formattedStartDate, date -> date.plusDays(1))
						.limit(ChronoUnit.DAYS.between(formattedStartDate, formattedEndDate))
						.forEach(date -> findAndCreateStockLevelForSerial(serial, date));
			}
		}
	}

	/**
	 * It gets all the active associated serials of the sku
	 *
	 * @param skuProducts
	 * @param date
	 */
	private void createStockLevelForAllActiveSerialProducts(final Collection<BlProductModel> skuProducts, final Date date)
	{
		for (final BlProductModel skuProduct : skuProducts)
		{
			for (final BlSerialProductModel serial : skuProduct.getSerialProducts())
			{
				if ((SerialStatusEnum.ACTIVE).equals(serial.getSerialStatus()) && null != serial.getWarehouseLocation())
				{
					final Date stockDate = Boolean.FALSE.equals(serial.getForRent()) ? null : date;
					createStockLevelForSerial(serial, stockDate);
				}
			}
		}
	}

	/**
	 * It first checks whether the stock already exists or not, if not, it creates the stock
	 *
	 * @param serial
	 * @param localDate
	 */
	private void findAndCreateStockLevelForSerial(final BlSerialProductModel serial, final LocalDate localDate)
	{
		final Date date = Date.from(localDate.atStartOfDay(ZoneId.systemDefault()).toInstant());
		final Collection<StockLevelModel> stockLevelModel = getBlStockLevelDao().findSerialStockLevelForDate(serial.getCode(),
				serial.getBlProduct().getCode(), date, date);
		if (CollectionUtils.isEmpty(stockLevelModel))
		{
			createStockLevelForSerial(serial, date);
		}
		else
		{
			BlLogger.logFormatMessageInfo(LOG, Level.WARN, "Stock already exist for serial product {} for the date {}",
					serial.getCode(),
					date);
		}

	}

	/**
	 * It first checks whether the stock already exists or not, if not, it creates the stock
	 *
	 * @param serial
	 */
	private void findAndCreateStockLevelForUsedGearSerial(final BlSerialProductModel serial)
	{
		final Collection<StockLevelModel> stockLevelModel = getBlStockLevelDao().findStockLevelForUsedGearSerial(serial.getCode(),
				serial.getBlProduct().getCode());
		if (CollectionUtils.isEmpty(stockLevelModel))
		{
			createStockLevelForSerial(serial, null);
		}
		else
		{
			BlLogger.logFormatMessageInfo(LOG, Level.WARN, "Stock already exist for serial product {}", serial.getCode());
		}

	}


	/**
	 * It creates the stock level for the sku and the serial
	 *
	 * @param serial
	 * @param date
	 */
	private void createStockLevelForSerial(final BlSerialProductModel serial, final Date date)
	{
		final StockLevelModel stockLevel = getModelService().create(StockLevelModel.class);
		stockLevel.setDate(date == null ? null : BlDateTimeUtils.getFormattedStartDay(date).getTime());
		stockLevel.setProductCode(serial.getBlProduct().getCode());
		stockLevel.setWarehouse(serial.getWarehouseLocation());
		stockLevel.setAvailable(0);
		stockLevel.setSerialProductCode(serial.getCode());
		stockLevel.setSerialStatus(serial.getSerialStatus());
		try
		{
			getModelService().save(stockLevel);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Stock created for serial product {} for the date {}", serial, date);
		}
		catch (final ModelSavingException ex)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, BlCoreConstants.EMPTY_STRING, ex,
					"Stock not created for the serial product {}", serial.getCode());
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
