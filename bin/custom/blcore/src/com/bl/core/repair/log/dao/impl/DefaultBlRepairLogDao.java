package com.bl.core.repair.log.dao.impl;

import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;

import java.util.List;
import java.util.Objects;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.Validate;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.assertj.core.util.Lists;

import com.bl.core.model.BlRepairLogModel;
import com.bl.core.repair.log.dao.BlRepairLogDao;
import com.bl.logging.BlLogger;


/**
 * Bl Repair Log Dao class to perform logic to get data fron DB with specific requirements
 *
 * @author Ravikumar
 *
 */
public class DefaultBlRepairLogDao implements BlRepairLogDao
{
	private static final Logger LOG = Logger.getLogger(DefaultBlRepairLogDao.class);

	private FlexibleSearchService flexibleSearchService;

	private static final String REPAIR_LOG_FOR_ORDER_CODE_QUERY = "SELECT {rl:" + ItemModel.PK + "} from {"
			+ BlRepairLogModel._TYPECODE + " as rl}, {" + AbstractOrderModel._TYPECODE + " as o} where {o:" + ItemModel.PK
			+ "} = ({{SELECT {order:" + ItemModel.PK + "} from {" + AbstractOrderModel._TYPECODE + " as order} where {order:"
			+ AbstractOrderModel.CODE + "} = ?" + AbstractOrderModel.CODE + "}}) AND {rl:" + BlRepairLogModel.ORDER + "} = {o:"
			+ ItemModel.PK + "}";

	/**
	 * {@inheritDoc}
	 */
	@Override
	public List<BlRepairLogModel> getRepairLogForOrderCode(final String orderCode)
	{
		Validate.notBlank(orderCode, "Parameter orderCode must not be null or blanck", null);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
				"DefaultBlRepairLogDao : getRepairLogForOrderCode : Query : {} with parameter : code : {}",
				REPAIR_LOG_FOR_ORDER_CODE_QUERY, orderCode);
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(REPAIR_LOG_FOR_ORDER_CODE_QUERY);
		fQuery.addQueryParameter(AbstractOrderModel.CODE, orderCode);
		final SearchResult<BlRepairLogModel> search = getFlexibleSearchService().<BlRepairLogModel> search(fQuery);
		if (Objects.isNull(search) || CollectionUtils.isEmpty(search.getResult()))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
					"DefaultBlRepairLogDao : getRepairLogForOrderCode : No Repair Log found for order code : {}", orderCode);
			return Lists.newArrayList();
		}
		final List<BlRepairLogModel> result = search.getResult();
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Repair Log found : {} for order code : {}", result, orderCode);
		return Lists.newArrayList(result);
	}

	/**
	 * @return the flexibleSearchService
	 */
	public FlexibleSearchService getFlexibleSearchService()
	{
		return flexibleSearchService;
	}

	/**
	 * @param flexibleSearchService
	 *           the flexibleSearchService to set
	 */
	public void setFlexibleSearchService(final FlexibleSearchService flexibleSearchService)
	{
		this.flexibleSearchService = flexibleSearchService;
	}

}
