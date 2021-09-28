package com.bl.core.blackout.date.dao.impl;

import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;

import java.util.Date;
import java.util.List;
import java.util.Objects;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.Validate;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.assertj.core.util.Lists;

import com.bl.core.blackout.date.dao.BlBlackoutDatesDao;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.BlackoutDateShippingMethodEnum;
import com.bl.core.enums.BlackoutDateTypeEnum;
import com.bl.core.model.BlBlackoutDateModel;
import com.bl.logging.BlLogger;


/**
 * This is an Implementaion of Dao Class contains the logic to get list of blackout dates.
 *
 * @author Ravikumar
 *
 */
public class DefaultBlBlackoutDatesDao implements BlBlackoutDatesDao
{
	private static final String SELECT_BD = "select {bd:";
	private static final String WHERE_BD = " as bd} where {bd:";
	private static final String RESULT_SIZE_FETCHED_FOR_BLACKOUT_DATES = "Result Size fetched for {} Blackout Dates: {}";
	private static final String FROM = "} from {";
	private static final Logger LOG = Logger.getLogger(DefaultBlBlackoutDatesDao.class);
	private FlexibleSearchService flexibleSearchService;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public List<BlBlackoutDateModel> getAllBlackoutDatesForGivenType(final BlackoutDateTypeEnum blackoutDateType)
	{
		Validate.notNull(blackoutDateType, "Blackout Date Type must not be null");
		final String flexiQuery = SELECT_BD + ItemModel.PK + FROM + BlBlackoutDateModel._TYPECODE + WHERE_BD
				+ BlBlackoutDateModel.BLACKOUTDATETYPE + "} = ({{select {type:" + ItemModel.PK + FROM + BlackoutDateTypeEnum._TYPECODE
				+ " as type} where {type:code} = ?code}})";
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Query to Fetch {} Blackout Dates : {}", blackoutDateType.toString(),
				flexiQuery);
		final FlexibleSearchQuery query = new FlexibleSearchQuery(flexiQuery);
		query.addQueryParameter(BlCoreConstants.CODE, blackoutDateType.toString());
		final SearchResult<BlBlackoutDateModel> search = getFlexibleSearchService().<BlBlackoutDateModel> search(query);
		if (Objects.isNull(search) || CollectionUtils.isEmpty(search.getResult()))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
					"DefaultBlBlackoutDatesDao : getAllBlackoutDatesForGivenType : Empty Blackout Dates found for {} Type",
					blackoutDateType.toString());
			return Lists.newArrayList();
		}
		final List<BlBlackoutDateModel> result = search.getResult();
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, RESULT_SIZE_FETCHED_FOR_BLACKOUT_DATES, blackoutDateType.toString(),
				result.size());
		return Lists.newArrayList(result);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public List<BlBlackoutDateModel> getAllBlackoutDatesForShippingMethods(final List<String> deliveryModeCodes)
	{
		Validate.notNull(deliveryModeCodes, "Delivery modes code must not be null");
		final String flexiQuery = SELECT_BD + ItemModel.PK + FROM + BlBlackoutDateModel._TYPECODE + WHERE_BD
				+ BlBlackoutDateModel.BLOCKEDSHIPPINGMETHOD + "} IN ({{select {type:" + ItemModel.PK + FROM
				+ BlackoutDateShippingMethodEnum._TYPECODE + " as type} where {type:code} IN (?code)}})";
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Query to Fetch Blocked Shipping Methods Blackout Dates {} is : {}",
				deliveryModeCodes, flexiQuery);
		final FlexibleSearchQuery query = new FlexibleSearchQuery(flexiQuery);
		query.addQueryParameter(BlCoreConstants.CODE, deliveryModeCodes);
		final SearchResult<BlBlackoutDateModel> search = getFlexibleSearchService().<BlBlackoutDateModel> search(query);
		if (Objects.isNull(search) || CollectionUtils.isEmpty(search.getResult()))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
					"DefaultBlBlackoutDatesDao : getAllBlackoutDatesForShippingMethods : No Blackout Dates found for {}",
					deliveryModeCodes);
			return Lists.newArrayList();
		}
		final List<BlBlackoutDateModel> result = search.getResult();
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, RESULT_SIZE_FETCHED_FOR_BLACKOUT_DATES, deliveryModeCodes, result.size());
		return Lists.newArrayList(result);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public List<BlBlackoutDateModel> getAllBlackoutDatesForGivenTypeAndFromDate(final Date fromDate,
			final BlackoutDateTypeEnum blackoutDateType)
	{
		Validate.notNull(fromDate, "From Date must not be null");
		Validate.notNull(blackoutDateType, "Blackout Date Type must not be null");
		final String flexiQuery = SELECT_BD + ItemModel.PK + FROM + BlBlackoutDateModel._TYPECODE + WHERE_BD
				+ BlBlackoutDateModel.BLACKOUTDATE + " } >= ?fromDate and {bd:" + BlBlackoutDateModel.BLACKOUTDATETYPE
				+ "} = ({{select {type:" + ItemModel.PK + FROM + BlackoutDateTypeEnum._TYPECODE
				+ " as type} where {type:code} = ?code}})";
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Query to Fetch {} Blackout Dates : {}", blackoutDateType.toString(),
				flexiQuery);
		final FlexibleSearchQuery query = new FlexibleSearchQuery(flexiQuery);
		query.addQueryParameter(BlCoreConstants.CODE, blackoutDateType.toString());
		query.addQueryParameter(BlCoreConstants.FROM_DATE, fromDate);
		final SearchResult<BlBlackoutDateModel> search = getFlexibleSearchService().<BlBlackoutDateModel> search(query);
		if (Objects.isNull(search) || CollectionUtils.isEmpty(search.getResult()))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
					"DefaultBlBlackoutDatesDao : getAllBlackoutDatesForGivenType : Empty Blackout Dates found for {} Type",
					blackoutDateType.toString());
			return Lists.newArrayList();
		}
		final List<BlBlackoutDateModel> result = search.getResult();
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, RESULT_SIZE_FETCHED_FOR_BLACKOUT_DATES, blackoutDateType.toString(),
				result.size());
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
