package com.bl.core.blackout.date.dao.impl;

import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;

import java.util.List;
import java.util.Objects;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.Validate;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.assertj.core.util.Lists;

import com.bl.core.blackout.date.dao.BlBlackoutDatesDao;
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
		final String flexiQuery = "select {bd:" + ItemModel.PK + FROM + BlBlackoutDateModel._TYPECODE + " as bd} where {bd:"
				+ BlBlackoutDateModel.BLACKOUTDATETYPE + "} = ({{select {type:" + ItemModel.PK + FROM + BlackoutDateTypeEnum._TYPECODE
				+ " as type} where {type:code} = ?code}})";
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Query to Fetch {} Blackout Dates : {}", blackoutDateType.toString(),
				flexiQuery);
		final FlexibleSearchQuery query = new FlexibleSearchQuery(flexiQuery);
		query.addQueryParameter("code", blackoutDateType.toString());
		final SearchResult<BlBlackoutDateModel> search = getFlexibleSearchService().<BlBlackoutDateModel> search(query);
		if (Objects.isNull(search))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
					"DefaultBlBlackoutDatesDao : getAllBlackoutDatesForGivenType : Error Occured while fetching Blackout Dates for {} Type",
					blackoutDateType.toString());
			return Lists.newArrayList();
		}
		final List<BlBlackoutDateModel> result = Lists.newArrayList(CollectionUtils.emptyIfNull(search.getResult()));
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Result Size fetched for {} Blackout Dates: {}",
				blackoutDateType.toString(), result.size());
		return result;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public List<BlBlackoutDateModel> getAllBlackoutDatesForShippingMethods(final List<String> deliveryModeCodes)
	{
		Validate.notNull(deliveryModeCodes, "Delivery modes code must not be null");
		final String flexiQuery = "select {bd:" + ItemModel.PK + FROM + BlBlackoutDateModel._TYPECODE + " as bd} where {bd:"
				+ BlBlackoutDateModel.BLOCKEDSHIPPINGMETHOD + "} IN ({{select {type:" + ItemModel.PK + FROM
				+ BlackoutDateShippingMethodEnum._TYPECODE + " as type} where {type:code} IN (?code)}})";
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Query to Fetch Blocked Shipping Methods Blackout Dates {} is : {}",
				deliveryModeCodes, flexiQuery);
		final FlexibleSearchQuery query = new FlexibleSearchQuery(flexiQuery);
		query.addQueryParameter("code", deliveryModeCodes);
		final SearchResult<BlBlackoutDateModel> search = getFlexibleSearchService().<BlBlackoutDateModel> search(query);
		if (Objects.isNull(search))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
					"DefaultBlBlackoutDatesDao : getAllBlackoutDatesForShippingMethods : Error Occured while fetching Blackout Dates for {}",
					deliveryModeCodes);
			return Lists.newArrayList();
		}
		final List<BlBlackoutDateModel> result = Lists.newArrayList(CollectionUtils.emptyIfNull(search.getResult()));
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Result Size fetched for {} Blackout Dates: {}", deliveryModeCodes,
				result.size());
		return result;
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
