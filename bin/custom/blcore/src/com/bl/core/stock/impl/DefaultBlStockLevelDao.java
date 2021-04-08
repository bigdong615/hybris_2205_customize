package com.bl.core.stock.impl;

import com.bl.logging.BlLogger;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.SearchResult;
import de.hybris.platform.stock.impl.DefaultStockLevelDao;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.stock.BlStockLevelDao;


/**
 * @author Moumita
 * This class is used to get the inventory for a product
 */
public class DefaultBlStockLevelDao extends DefaultStockLevelDao implements BlStockLevelDao
{
	private static final Logger LOG = Logger.getLogger(DefaultBlStockLevelDao.class);

	private static final String STOCK_LEVEL_FOR_DATE_QUERY = "SELECT {" + StockLevelModel.PK + "} from {"
			+ StockLevelModel._TYPECODE + "} WHERE {" + StockLevelModel.PRODUCTCODE + "} = ?productCode " +
			"AND {" + StockLevelModel.DATE + "} BETWEEN ?startDate AND ?endDate " +
			"AND {" + StockLevelModel.SERIALSTATUS + "} IN ({{SELECT {sse:PK} FROM {" + SerialStatusEnum._TYPECODE +
			" as sse} WHERE {sse:CODE} = (?active)}}) " +
			"AND {" + StockLevelModel.WAREHOUSE + "} IN (?warehouses)";


	/**
	 * {@inheritDoc}
	 */
	@Override
	public Collection<StockLevelModel> findStockLevelForDate(final String productCode, final Collection<WarehouseModel> warehouseModels,
			final Date startDay, final Date endDay)
	{
		if (CollectionUtils.isEmpty(warehouseModels))
		{
				throw new IllegalArgumentException("warehouses cannot be null.");
		}
		else
		{
			final Set<WarehouseModel> warehouses = filterWarehouseModels(warehouseModels);
			final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(STOCK_LEVEL_FOR_DATE_QUERY);
			fQuery.addQueryParameter(BlCoreConstants.PRODUCT_CODE, productCode);

			final Calendar startDate = getFormattedDate(startDay, BlCoreConstants.START_HOURS, BlCoreConstants.START_MINUTES, 
					BlCoreConstants.START_SECONDS);
			fQuery.addQueryParameter(BlCoreConstants.START_DATE, startDate.getTime());

			final Calendar endDate = getFormattedDate(endDay, BlCoreConstants.END_HOURS, BlCoreConstants.END_MINUTES,
					BlCoreConstants.END_SECONDS);
			fQuery.addQueryParameter(BlCoreConstants.END_DATE, endDate.getTime());

			fQuery.addQueryParameter(BlCoreConstants.ACTIVE, SerialStatusEnum.ACTIVE.getCode());
			fQuery.addQueryParameter(BlCoreConstants.WAREHOUSES, warehouses);
			final SearchResult result = getFlexibleSearchService().search(fQuery);
			final List<StockLevelModel> stockLevels = result.getResult();
			if (CollectionUtils.isEmpty(stockLevels))
			{
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "No Stock Levels found for product : {} and date between : {} and {}",
						productCode, startDate, endDate);
				return Collections.emptyList();
			}
			return stockLevels;
		}
	}

	/**
	 * To get the formatted date
	 * @param day the date
	 * @param hours the hours
	 * @param minutes the minutes
	 * @param seconds the seconds
	 * @return Calendar
	 */
	private Calendar getFormattedDate(Date day, int hours, int minutes, int seconds) {
		final Calendar startDate = new GregorianCalendar();
		startDate.setTime(day);
		startDate.set(Calendar.HOUR_OF_DAY, hours);
		startDate.set(Calendar.MINUTE, minutes);
		startDate.set(Calendar.SECOND, seconds);
		return startDate;
	}

	/**
	 * Filters warehouse list to remove null's and duplicate elements.
	 * @param warehouses the list of warehouseModel
	 * @return List<WarehouseModel> the list of warehouseModel
	 */
	protected Set<WarehouseModel> filterWarehouseModels(final Collection<WarehouseModel> warehouses)
	{
		final Set<WarehouseModel> setOfWarehouses = new HashSet<>();
		for (final WarehouseModel house : warehouses)
		{
			if (house != null)
			{
				setOfWarehouses.add(house);
			}
		}
		return setOfWarehouses;
	}
}
