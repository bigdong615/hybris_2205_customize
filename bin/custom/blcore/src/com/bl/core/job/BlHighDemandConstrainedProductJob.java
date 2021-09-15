package com.bl.core.job;

import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;

import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.joda.time.DateTime;

import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.dao.BlProductDao;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;


/**
 * This job is responsible to manage High Demand Constrained Product
 * @author Avani Patel
 *
 */
public class BlHighDemandConstrainedProductJob extends AbstractJobPerformable<CronJobModel>
{
	private static final String HIGH_DEMAND_CONSTRAINED_PRODUCT_WEEK = "high.demand.constrained.product.week";
	private static final String HIGH_DEMAND_CONSTRAINED_PRODUCT_AFTER_WEEK_PERCENTAGE = "high.demand.constrained.product.after.week.percentage";
	private static final String HIGH_DEMAND_CONSTRAINED_PRODUCT_BEFOR_WEEK_PERCENTAGE = "high.demand.constrained.product.befor.week.percentage";
	private static final String HIGH_DEMAND_CONSTRAINED_PRODUCT_LENGTH_OF_TIME = "high.demand.constrained.product.length.of.time";
	private static final Logger LOG = Logger.getLogger(BlHighDemandConstrainedProductJob.class);
	private BlProductDao productDao;
	private BlStockLevelDao blStockLevelDao;
	private ConfigurationService configurationService;

	@Override
	public PerformResult perform(final CronJobModel arg0)
	{
		try
		{
			final Long month = getConfigValue(HIGH_DEMAND_CONSTRAINED_PRODUCT_LENGTH_OF_TIME);
			final Long beforWeekPercentage = getConfigValue(HIGH_DEMAND_CONSTRAINED_PRODUCT_BEFOR_WEEK_PERCENTAGE);
			final Long afterWeekPercentage = getConfigValue(HIGH_DEMAND_CONSTRAINED_PRODUCT_AFTER_WEEK_PERCENTAGE);
			final Long week = getConfigValue(HIGH_DEMAND_CONSTRAINED_PRODUCT_WEEK);

			if (isJobIsEligibleToExecute(month, beforWeekPercentage, afterWeekPercentage, week))
			{
				BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
						"Please check the value for property {} is {} , {} is {} , {} is {} , {} is {}",
						HIGH_DEMAND_CONSTRAINED_PRODUCT_LENGTH_OF_TIME, month, HIGH_DEMAND_CONSTRAINED_PRODUCT_BEFOR_WEEK_PERCENTAGE,
						beforWeekPercentage, HIGH_DEMAND_CONSTRAINED_PRODUCT_AFTER_WEEK_PERCENTAGE, afterWeekPercentage,
						HIGH_DEMAND_CONSTRAINED_PRODUCT_WEEK, week);
				return new PerformResult(CronJobResult.FAILURE, CronJobStatus.FINISHED);
			}

			final Collection<BlProductModel> activeBlProductModelList = getProductDao().getAllActiveSkuProducts();
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Number of SKU found : {}", activeBlProductModelList.size());
			final Date currentDate = BlDateTimeUtils.getFormattedStartDay(new Date()).getTime();
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Current Date : {}", currentDate);

			if (CollectionUtils.isNotEmpty(activeBlProductModelList))
			{
				final List<BlProductModel> rentalSKUList = activeBlProductModelList.stream().filter(BlProductModel::getForRent)
						.collect(Collectors.toList());
				for (final BlProductModel sku : rentalSKUList)
				{
					final AtomicBoolean executeNextCriteria = new AtomicBoolean(Boolean.TRUE);
					BlLogger.logFormatMessageInfo(LOG, Level.INFO, "AtomicBoolean : {}", executeNextCriteria);
					checkConstrainedProductEligibilty(month, beforWeekPercentage, afterWeekPercentage, week, executeNextCriteria,
							currentDate, sku);
				}
			}
			return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
		}
		catch (final Exception exception)
		{
			BlLogger.logMessage(LOG, Level.ERROR, "Error while performing cronjob BlHighDemandConstrainedProductJob", exception);
			return new PerformResult(CronJobResult.FAILURE, CronJobStatus.FINISHED);
		}
	}

	/**
	 * Start performing criterias on SKU.
	 *
	 * @param month
	 *           the month
	 * @param beforWeekPercentage
	 *           the befor week percentage
	 * @param afterWeekPercentage
	 *           the after week percentage
	 * @param week
	 *           the week
	 * @param executeCriteria
	 *           the execute next criteria
	 * @param currentDate
	 *           the current date
	 * @param sku
	 *           the sku
	 */
	private void checkConstrainedProductEligibilty(final Long month, final Long beforWeekPercentage, final Long afterWeekPercentage,
			final Long week, final AtomicBoolean executeCriteria, final Date currentDate, final BlProductModel sku)
	{
		if (executeCriteria.get())
		{
			executeFirstCriteria(executeCriteria, currentDate, sku, month);
		}
		if (!executeCriteria.get())
		{
			executeSecondCriteria(beforWeekPercentage, week, executeCriteria, currentDate, sku);
		}
		if (!executeCriteria.get())
		{
			executeThirdCriteria(afterWeekPercentage, week, executeCriteria, currentDate, sku);
		}
	}

	/**
	 * Execute first criteria.
	 *
	 * @param executeCriteria
	 *           the execute next criteria
	 * @param currentDate
	 *           the current date
	 * @param sku
	 *           the sku
	 * @param month
	 *           the month
	 */
	private void executeFirstCriteria(final AtomicBoolean executeCriteria, final Date currentDate, final BlProductModel sku,
			final Long month)
	{
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "executeFirstCriteria : {}", sku.getCode());
		final Date startDateToCheck = BlDateTimeUtils.getFormattedStartDay(new DateTime().minusMonths(month.intValue()).toDate())
				.getTime();
		final List<BlSerialProductModel> lSerials = Lists.newArrayList(sku.getSerialProducts()).stream().filter(serials -> (serials.getSerialStatus().equals(SerialStatusEnum.ACTIVE) || serials.getSerialStatus().equals(SerialStatusEnum.RECEIVED_OR_RETURNED))).collect(
				Collectors.toList());
		if (CollectionUtils.isNotEmpty(lSerials))
		{
			if (lSerials.size() > 1)
			{
				lSerials.forEach(serialcreationtime -> BlLogger.logFormatMessageInfo(LOG, Level.INFO,
						"Serial Product Code Befor Sorting: {} ,Serial product Creationtime : {}", serialcreationtime.getCode(),
						serialcreationtime.getCreationtime()));

				Collections.sort(lSerials, (serial1, serial2) -> serial1.getCreationtime().compareTo(serial2.getCreationtime()));
				lSerials.forEach(serialcreationtime -> BlLogger.logFormatMessageInfo(LOG, Level.INFO,
						"Serial Product Code After Sorting: {} ,Serial product Creationtime : {}", serialcreationtime.getCode(),
						serialcreationtime.getCreationtime()));

			}
			final BlSerialProductModel blSerialProductModel = lSerials.get(0);
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"First Serial Product Code : {} , First Serial product Creationtime : {}", blSerialProductModel.getCode(),
					blSerialProductModel.getCreationtime());
			final boolean firstCriteriaIsEligible = isFirstCriteriaIsEligible(currentDate,
					startDateToCheck, blSerialProductModel);
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, "firstCriteriaIsEligible : {}", firstCriteriaIsEligible);
			executeCriteria.set(firstCriteriaIsEligible);
			if (executeCriteria.get())
			{
				setConstrainedToTrueOnSKU(sku);
			}
		}
	}

	/**
	 * Execute second criteria.
	 *
	 * @param beforWeekPercentage
	 *           the befor week percentage
	 * @param week
	 *           the week
	 * @param executeCriteria
	 *           the execute next criteria
	 * @param currentDate
	 *           the current date
	 * @param sku
	 *           the sku
	 */
	private void executeSecondCriteria(final Long beforWeekPercentage, final Long week, final AtomicBoolean executeCriteria,
			final Date currentDate, final BlProductModel sku)
	{
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "executeSecondCriteria : {}", sku.getCode());
		final Date beforWeekStartDate = BlDateTimeUtils.getFormattedStartDay(new DateTime().minusWeeks(week.intValue()).toDate())
				.getTime();
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "BeforWeek StartDate : {}", beforWeekStartDate);
		final Set<String> collectserialSkuList = sku.getSerialProducts().stream().map(BlSerialProductModel::getCode)
				.collect(Collectors.toSet());
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, " second collectserialSkuList : {}", collectserialSkuList.size());
		if (CollectionUtils.isNotEmpty(collectserialSkuList))
		{
			final Collection<StockLevelModel> serialStock = getBlStockLevelDao()
					.findALLSerialStockLevelsForDateAndCodes(collectserialSkuList, beforWeekStartDate, currentDate);
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"BlProduct Code : {} ,Serial Product's Total Stock Found : {}", sku.getCode(),
					serialStock.size());
			final Map<String, List<StockLevelModel>> stockAsPerSerial = serialStock.stream()
					.collect(Collectors.groupingBy(StockLevelModel::getSerialProductCode));
			long criteria2Numerator = 0;
			long criteria2Denominator = 0;

			for (final String serialCode : collectserialSkuList)
			{
				if (MapUtils.isNotEmpty(stockAsPerSerial) && stockAsPerSerial.containsKey(serialCode))
				{
					final List<StockLevelModel> stockList = stockAsPerSerial.get(serialCode);
					BlLogger.logFormatMessageInfo(LOG, Level.INFO,
							"Serial Product Code : {} ,Serial Product's Stock List Size : {}", serialCode,
								serialStock.size());
				stockList.forEach(
							stockSerial -> 	BlLogger.logFormatMessageInfo(LOG, Level.INFO,
									"Serial Product Status : {} ,Serial Product ReservedStatus : {}", stockSerial.getSerialStatus(),
									stockSerial.getReservedStatus()));
					criteria2Numerator += getActiveSerials(stockList);
					criteria2Denominator += getAllUnsoldAndUnScrappedSerial(stockList);
				}
			}
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, " Criteria2 Numerator : {}", criteria2Numerator);
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, " criteria2 Denominator : {}", criteria2Denominator);
			if (criteria2Denominator > 0 && criteria2Numerator > 0)
			{
				final double serialProductPercentage = ((double) criteria2Numerator / criteria2Denominator) * 100;
				BlLogger.logFormatMessageInfo(LOG, Level.INFO, " Serial Product Percentage : {}", serialProductPercentage);
				executeCriteria.set(serialProductPercentage <= beforWeekPercentage.longValue());
				if (executeCriteria.get())
				{
					setConstrainedToTrueOnSKU(sku);
				}
			}
		}
	}

	/**
	 * Execute third criteria.
	 *
	 * @param afterWeekPercentage
	 *           the after week percentage
	 * @param week
	 *           the week
	 * @param executeCriteria
	 *           the execute next criteria
	 * @param currentDate
	 *           the current date
	 * @param sku
	 *           the sku
	 */
	private void executeThirdCriteria(final Long afterWeekPercentage, final Long week, final AtomicBoolean executeCriteria,
			final Date currentDate, final BlProductModel sku)
	{
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "executeThirdCriteria : {}", sku.getCode());
		final Date afterWeekEndDate = BlDateTimeUtils.getFormattedStartDay(new DateTime().plusWeeks(week.intValue()).toDate())
				.getTime();
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "AfterWeek StartDate : {}", afterWeekEndDate);
		final Set<String> collectserialSkuList = sku.getSerialProducts().stream().map(BlSerialProductModel::getCode)
				.collect(Collectors.toSet());
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, " Third collectserialSkuList : {}", collectserialSkuList.size());
		if (CollectionUtils.isNotEmpty(collectserialSkuList))
		{
			final Collection<StockLevelModel> serialStock = getBlStockLevelDao()
					.findALLSerialStockLevelsForDateAndCodes(collectserialSkuList, currentDate, afterWeekEndDate);
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"BlProduct Code : {} ,Serial Product's Total Stock Found : {}", sku.getCode(),
					serialStock.size());
			final Map<String, List<StockLevelModel>> stockAsPerSerial = serialStock.stream()
					.collect(Collectors.groupingBy(StockLevelModel::getSerialProductCode));
			long criteria3Numerator = 0;
			long criteria3Denominator = 0;

			for (final String serialCode : collectserialSkuList)
			{
				if (MapUtils.isNotEmpty(stockAsPerSerial) && stockAsPerSerial.containsKey(serialCode))
				{
					final List<StockLevelModel> stockList = stockAsPerSerial.get(serialCode);
					BlLogger.logFormatMessageInfo(LOG, Level.INFO,
							"Serial Product Code : {} ,Serial Product's Stock List Size : {}", serialCode,
							serialStock.size());
					stockList.forEach(
							stockSerial -> 	BlLogger.logFormatMessageInfo(LOG, Level.INFO,
									"Serial Product Status : {} ,Serial Product ReservedStatus : {}", stockSerial.getSerialStatus(),
									stockSerial.getReservedStatus()));

					criteria3Numerator += getActiveSerials(stockList);
					criteria3Denominator += getAllUnsoldAndUnScrappedSerial(stockList);
				}
			}
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, " criteria3 Numerator : {}", criteria3Numerator);
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, " criteria3 Denominator : {}", criteria3Denominator);

			if (criteria3Denominator > 0 && criteria3Numerator > 0)
			{
				final double serialProductPercentage = ((double) criteria3Numerator / criteria3Denominator) * 100;
				BlLogger.logFormatMessageInfo(LOG, Level.INFO, " Serial Product Percentage : {}", serialProductPercentage);
				executeCriteria.set(serialProductPercentage <= afterWeekPercentage.longValue());
				if (executeCriteria.get())
				{
					setConstrainedToTrueOnSKU(sku);
				}
			}
		}

	}



	/**
	 * Sets the constrained to true on SKU.
	 *
	 * @param sku
	 *           the new constrained to true on SKU
	 */
	private void setConstrainedToTrueOnSKU(final BlProductModel sku)
	{
		sku.setConstrained(Boolean.TRUE);
		modelService.save(sku);
		modelService.refresh(sku);
	}

	/**
	 * Checks if is first criteria is eligible.
	 *
	 * @param currentDate
	 *           the current date
	 * @param startDateToCheck
	 *           the start date to check
	 * @param blSerialProductModel
	 *           the bl serial product model
	 * @return true, if is first criteria is eligible
	 */
	private boolean isFirstCriteriaIsEligible(final Date currentDate, final Date startDateToCheck,
			final BlSerialProductModel blSerialProductModel)
	{
		return Objects.nonNull(blSerialProductModel.getSerialStatus()) && checkSerialStatus(blSerialProductModel)
				&& blSerialProductModel.getCreationtime().compareTo(startDateToCheck) >= 0
				&& blSerialProductModel.getCreationtime().compareTo(currentDate) <= 0;
	}

	/**
	 * Check serial status.
	 *
	 * @param blSerialProductModel
	 *           the bl serial product model
	 * @return true, if successful
	 */
	private boolean checkSerialStatus(final BlSerialProductModel blSerialProductModel)
	{
		return SerialStatusEnum.ACTIVE.equals(blSerialProductModel.getSerialStatus())
				|| SerialStatusEnum.RECEIVED_OR_RETURNED.equals(blSerialProductModel.getSerialStatus());
	}

	/**
	 * Checks if is job is eligible to execute.
	 *
	 * @param month
	 *           the month
	 * @param beforWeekPercentage
	 *           the befor week percentage
	 * @param afterWeekPercentage
	 *           the after week percentage
	 * @param week
	 *           the week
	 * @return true, if is job is eligible to execute
	 */
	private boolean isJobIsEligibleToExecute(final Long month, final Long beforWeekPercentage, final Long afterWeekPercentage,
			final Long week)
	{
		return isGivenValueIsZero(month) || isGivenValueIsZero(beforWeekPercentage) || isGivenValueIsZero(afterWeekPercentage)
				|| isGivenValueIsZero(week);
	}

	/**
	 * Gets the config value.
	 *
	 * @param code
	 *           the code
	 * @return the config value
	 */
	private Long getConfigValue(final String code)
	{
		return getLongValue(getConfigurationService().getConfiguration().getString(code, StringUtils.EMPTY));
	}

	/**
	 * Gets the long value.
	 *
	 * @param value
	 *           the value
	 * @return the long value
	 */
	private Long getLongValue(final String value)
	{
		if (StringUtils.isNotBlank(value) && NumberUtils.isParsable(value))
		{
			return Long.valueOf(value);
		}
		return Long.valueOf(0);
	}

	/**
	 * Checks if is given value is zero.
	 *
	 * @param value
	 *           the value
	 * @return true, if is given value is zero
	 */
	private boolean isGivenValueIsZero(final long value)
	{
		return value <= 0;
	}

	/**
	 * Gets the all unsold and un scrapped serial.
	 *
	 * @param stockLevelModels
	 *           the stock level models
	 * @return the all unsold and un scrapped serial
	 */
	private long getAllUnsoldAndUnScrappedSerial(final List<StockLevelModel> stockLevelModels)
	{
		return CollectionUtils.isNotEmpty(stockLevelModels)
				? stockLevelModels.stream().filter(stockLevelModel -> !SerialStatusEnum.SOLD.equals(stockLevelModel.getSerialStatus())
				&& !SerialStatusEnum.SCRAPPED.equals(stockLevelModel.getSerialStatus())).count()
				: 0;
	}

	/**
	 * Gets the active serials.
	 *
	 * @param stockLevelModels
	 *           the stock level models
	 * @return the active serials
	 */
	private long getActiveSerials(final List<StockLevelModel> stockLevelModels)
	{
		return CollectionUtils.isNotEmpty(stockLevelModels) ? stockLevelModels.stream()
				.filter(stockLevelModel -> !stockLevelModel.getReservedStatus()
						&& (SerialStatusEnum.ACTIVE.equals(stockLevelModel.getSerialStatus())
						|| SerialStatusEnum.RECEIVED_OR_RETURNED.equals(stockLevelModel.getSerialStatus())))
				.count() : 0;
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
	 * @return the configurationService
	 */
	public ConfigurationService getConfigurationService()
	{
		return configurationService;
	}

	/**
	 * @param configurationService
	 *           the configurationService to set
	 */
	public void setConfigurationService(final ConfigurationService configurationService)
	{
		this.configurationService = configurationService;
	}

	public BlStockLevelDao getBlStockLevelDao()
	{
		return blStockLevelDao;
	}

	public void setBlStockLevelDao(final BlStockLevelDao blStockLevelDao)
	{
		this.blStockLevelDao = blStockLevelDao;
	}
}
