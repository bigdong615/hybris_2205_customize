/**
 *
 */
package com.bl.core.dao.ipVelocity.impl;

import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;

import java.util.List;

import com.bl.core.dao.ipVelocity.BlIpVelocityDao;
import com.bl.core.model.IpVelocityFilterModel;


/**
 * @author Admin
 *
 */



public class DefaultBlIpVelocityDao implements BlIpVelocityDao
{


	private static final Object IP_VELOCITY_LIST = "select {ip:" + IpVelocityFilterModel.PK + "} from {"
			+ IpVelocityFilterModel._TYPECODE + " as ip} where {ip:" + IpVelocityFilterModel.USERIP + "} =?ipAddress and {ip:"
			+ IpVelocityFilterModel.USERID + "} =?userId";
	private static final Object IP_VELOCITY_LIST_ALL = "select {ip:" + IpVelocityFilterModel.PK + "} from {"
			+ IpVelocityFilterModel._TYPECODE + " as ip}";
	private FlexibleSearchService flexibleSearchService;

	@Override
	public IpVelocityFilterModel getUserData(final String ipAddress, final String userId)
	{
		final StringBuilder queryString = new StringBuilder();
		queryString.append(IP_VELOCITY_LIST);
		final FlexibleSearchQuery query = new FlexibleSearchQuery(queryString.toString());
		query.addQueryParameter("ipAddress", ipAddress);
		query.addQueryParameter("userId", userId);


		final List<IpVelocityFilterModel> searchResult = flexibleSearchService.<IpVelocityFilterModel> search(query).getResult();

		return searchResult != null && searchResult.size() > 0 ? searchResult.get(0) : null;
	}

	@Override


	public List<IpVelocityFilterModel> getAll()
	{
		final StringBuilder queryString = new StringBuilder();
		queryString.append(IP_VELOCITY_LIST_ALL);
		final FlexibleSearchQuery query = new FlexibleSearchQuery(queryString.toString());

		final List<IpVelocityFilterModel> searchResult = flexibleSearchService.<IpVelocityFilterModel> search(query).getResult();

		return searchResult != null && searchResult.size() > 0 ? searchResult : null;
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
