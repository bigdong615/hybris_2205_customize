package com.bl.core.stock.impl;

import com.bl.core.enums.SerialStatusEnum;
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
import org.apache.log4j.Logger;

import com.bl.core.stock.BlStockLevelDao;


/**
 * @author Moumita
 *
 */
public class BlStockLevelDaoImpl extends DefaultStockLevelDao implements BlStockLevelDao
{
	private static final Logger LOG = Logger.getLogger(BlStockLevelDaoImpl.class);

	private static final String STOCK_LEVEL_FOR_DATE_QUERY = "SELECT {" + StockLevelModel.PK + "} from {"
			+ StockLevelModel._TYPECODE + "} WHERE {" + StockLevelModel.PRODUCTCODE + "} = ?productCode " + 
			"AND {" + StockLevelModel.DATE + "} BETWEEN ?startDate AND ?endDate " +
			"AND {" + StockLevelModel.SERIALSTATUS + "} IN ({{SELECT {sse:PK} FROM {" + SerialStatusEnum._TYPECODE +
			" as sse} WHERE {sse:CODE} = (?active)}}) " +
			"AND {" + StockLevelModel.WAREHOUSE + "} IN (?warehouses)";
	private static final String ACTIVE = "active";
	private static final String PRODUCT_CODE = "productCode";
	private static final String START_DATE = "startDate";
	private static final String END_DATE = "endDate";
	private static final String WAREHOUSES = "warehouses";
	private static final int END_HOURS = 23;
	private static final int END_MINUTES = 59;
	private static final int END_SECONDS = 59;
	private static final int START_HOURS = 0;
	private static final int START_MINUTES = 0;
	private static final int START_SECONDS = 0;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Collection<StockLevelModel> findStockLevelForDate(final String productCode, final Collection<WarehouseModel> warehouseModels,
			final Date date)
	{
		final List warehouses = filterWarehouseModels(warehouseModels);
		if (warehouses.isEmpty())
		{
			return Collections.emptyList();
		}
		else
		{
			final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(STOCK_LEVEL_FOR_DATE_QUERY);
			fQuery.addQueryParameter(PRODUCT_CODE, productCode);

			final Calendar startDate = new GregorianCalendar();
			startDate.setTime(date);
			startDate.set(Calendar.HOUR_OF_DAY, START_HOURS);
			startDate.set(Calendar.MINUTE, START_MINUTES);
			startDate.set(Calendar.SECOND, START_SECONDS);
			fQuery.addQueryParameter(START_DATE, startDate.getTime());

			final Calendar endDate = new GregorianCalendar();
			endDate.setTime(date);
			endDate.set(Calendar.HOUR_OF_DAY, END_HOURS);
			endDate.set(Calendar.MINUTE, END_MINUTES);
			endDate.set(Calendar.SECOND, END_SECONDS);
			fQuery.addQueryParameter(END_DATE, endDate.getTime());

			fQuery.addQueryParameter(ACTIVE, SerialStatusEnum.ACTIVE.getCode());
			fQuery.addQueryParameter(WAREHOUSES, warehouses);
			final SearchResult result = getFlexibleSearchService().search(fQuery);
			final List<StockLevelModel> stockLevels = result.getResult();
			if (CollectionUtils.isEmpty(stockLevels))
			{
				LOG.debug("No Stock Levels found for product: " + productCode + " and date: " + date);
				return Collections.emptyList();
			}
			return stockLevels;
		}
	}

	/**
	 * Filters warehouse list to remove null's and duplicate elements.
	 * @param warehouses the list of warehouseModel
	 * @return List<WarehouseModel> the list of warehouseModel
	 */
	protected List<WarehouseModel> filterWarehouseModels(final Collection<WarehouseModel> warehouses)
	{
		if (warehouses == null)
		{
			throw new IllegalArgumentException("warehouses cannot be null.");
		}
		final Set<WarehouseModel> result = new HashSet<>();
		for (final WarehouseModel house : warehouses)
		{
			if (house != null)
			{
				result.add(house);
			}
		}
		return new ArrayList<>(result);
	}
}
