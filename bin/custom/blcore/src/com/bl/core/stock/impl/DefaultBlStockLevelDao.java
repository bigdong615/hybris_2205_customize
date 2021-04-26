package com.bl.core.stock.impl;

import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.SearchResult;
import de.hybris.platform.stock.impl.DefaultStockLevelDao;

import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.logging.BlLogger;


/**
 * This class is used to get the inventory for a product
 * @author Moumita
 */
public class DefaultBlStockLevelDao extends DefaultStockLevelDao implements BlStockLevelDao
{
	private static final Logger LOG = Logger.getLogger(DefaultBlStockLevelDao.class);
	private static final String AND = "AND {";

	private static final String STOCK_LEVEL_FOR_DATE_QUERY = "SELECT {" + StockLevelModel.PK + "} from {"
			+ StockLevelModel._TYPECODE + "} WHERE {" + StockLevelModel.PRODUCTCODE + "} = ?productCode " +
			AND + StockLevelModel.DATE + "} BETWEEN ?startDate AND ?endDate " +
			AND + StockLevelModel.SERIALSTATUS + "} IN ({{SELECT {sse:PK} FROM {" + SerialStatusEnum._TYPECODE +
			" as sse} WHERE {sse:CODE} = (?active)}}) " +
			AND + StockLevelModel.WAREHOUSE + "} IN (?warehouses)";

	private static final String SERIAL_STOCK_LEVEL_FOR_DATE_QUERY = "SELECT {" + StockLevelModel.PK + "} from {"
			+ StockLevelModel._TYPECODE + "} WHERE {" + StockLevelModel.PRODUCTCODE + "} = ?productCode " +
			AND + StockLevelModel.DATE + "} BETWEEN ?startDate AND ?endDate " +
			AND + StockLevelModel.SERIALPRODUCTCODE + "} = ?serialProductCode";


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
			//Filters warehouse list to remove null's and duplicate elements.
			final Set<WarehouseModel> warehouses = new HashSet<>(warehouseModels);
			final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(STOCK_LEVEL_FOR_DATE_QUERY);

			addQueryParameter(productCode, startDay, endDay, fQuery);

			fQuery.addQueryParameter(BlCoreConstants.ACTIVE, SerialStatusEnum.ACTIVE.getCode());
			fQuery.addQueryParameter(BlCoreConstants.WAREHOUSES, warehouses);
			final SearchResult result = getFlexibleSearchService().search(fQuery);
			final List<StockLevelModel> stockLevels = result.getResult();
			if (CollectionUtils.isEmpty(stockLevels))
			{
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "No Stock Levels found for product : {} and date between : {} and {}",
						productCode, startDay, endDay);
				return Collections.emptyList();
			}
			return stockLevels;
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public StockLevelModel findSerialStockLevelForDate(final String serialProductCode,
			final String productCode, final Date startDay, final Date endDay)
	{
			final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(SERIAL_STOCK_LEVEL_FOR_DATE_QUERY);
			fQuery.addQueryParameter(BlCoreConstants.SERIAL_PRODUCT_CODE, serialProductCode);

			addQueryParameter(productCode, startDay, endDay, fQuery);

			final SearchResult result = getFlexibleSearchService().search(fQuery);
			final List<StockLevelModel> stockLevels = result.getResult();
			if (CollectionUtils.isEmpty(stockLevels))
			{
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "No Stock Levels found for product : {} and date between : {} and {}",
						productCode, startDay, endDay);
				return null;
			}
			return stockLevels.get(0);
	}

	/**
	 * It adds the parameters value into query
	 * @param productCode
	 * @param startDay
	 * @param endDay
	 * @param fQuery
	 */
	private void addQueryParameter(final String productCode, final Date startDay, final Date endDay,
			final FlexibleSearchQuery fQuery)
	{
		fQuery.addQueryParameter(BlCoreConstants.PRODUCT_CODE, productCode);

		final Calendar startDate = BlDateTimeUtils.getFormattedStartDay(startDay);
		fQuery.addQueryParameter(BlCoreConstants.START_DATE, startDate.getTime());

		final Calendar endDate = BlDateTimeUtils.getFormattedEndDay(endDay);
		fQuery.addQueryParameter(BlCoreConstants.END_DATE, endDate.getTime());
	}

}
