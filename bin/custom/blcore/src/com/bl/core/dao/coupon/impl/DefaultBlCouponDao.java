package com.bl.core.dao.coupon.impl;

import de.hybris.platform.couponservices.dao.impl.DefaultCouponDao;
import de.hybris.platform.couponservices.model.AbstractCouponModel;
import de.hybris.platform.couponservices.model.MultiCodeCouponModel;
import de.hybris.platform.couponservices.model.SingleCodeCouponModel;
import de.hybris.platform.jalo.Item;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import de.hybris.platform.util.Config;

import java.util.Collections;
import java.util.Map;

import com.bl.core.constants.BlCoreConstants;


/**
 * Adding this to have a case check for coupon id for SQL Server Cloud Portal
 *
 * @author Ritika
 */
public class DefaultBlCouponDao extends DefaultCouponDao
{
	private static final String CASE_SENISTIVE_CHECK = " COLLATE SQL_Latin1_General_CP1_CS_AS";


	public DefaultBlCouponDao()
	{
		// do nothing
	}

	@Override
	public AbstractCouponModel findCouponById(final String coupon)
	{
		final String[] parts = coupon.split("-");
		final String couponId = parts[0];

		String query = "SELECT {" + Item.PK + "} " + "FROM   {" + "AbstractCoupon" + "} " + "WHERE  {" + "couponId"
				+ "} = ?couponId";
		ServicesUtil.validateParameterNotNull(couponId, "String couponId cannot be null");
		final Map<String, String> params = Collections.singletonMap(BlCoreConstants.COUPON_ID, couponId);
		if (Config.isSQLServerUsed())
		{
			query = query.concat(CASE_SENISTIVE_CHECK);
		}
		return (AbstractCouponModel) this.getFlexibleSearchService().searchUnique(new FlexibleSearchQuery(query, params));
	}

	@Override
	public SingleCodeCouponModel findSingleCodeCouponById(final String couponId)
	{
		String singleCodeCouponQuery = "SELECT {" + Item.PK + "} " + "FROM   {" + "SingleCodeCoupon" + "} " + "WHERE  {"
				+ "couponId" + "} = ?couponId";
		ServicesUtil.validateParameterNotNull(couponId, "String couponId must not be null");
		final Map<String, String> params = Collections.singletonMap(BlCoreConstants.COUPON_ID, couponId);
		if (Config.isSQLServerUsed())
		{
			singleCodeCouponQuery = singleCodeCouponQuery.concat(CASE_SENISTIVE_CHECK);
		}
		return (SingleCodeCouponModel) this.getFlexibleSearchService()
				.searchUnique(new FlexibleSearchQuery(singleCodeCouponQuery, params));
	}

	@Override
	public MultiCodeCouponModel findMultiCodeCouponById(final String couponId)
	{
		ServicesUtil.validateParameterNotNull(couponId, "String couponId must not be null");
		String multiCodeCouponQuery = "SELECT {pk} FROM   {MultiCodeCoupon} WHERE  {couponId} = ?couponId";
		final Map<String, String> params = Collections.singletonMap(BlCoreConstants.COUPON_ID, couponId);
		if (Config.isSQLServerUsed())
		{
			multiCodeCouponQuery = multiCodeCouponQuery.concat(CASE_SENISTIVE_CHECK);
		}
		return (MultiCodeCouponModel) this.getFlexibleSearchService()
				.searchUnique(new FlexibleSearchQuery(multiCodeCouponQuery, params));
	}
}

