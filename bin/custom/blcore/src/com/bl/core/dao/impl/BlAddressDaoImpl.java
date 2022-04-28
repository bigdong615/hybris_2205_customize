/**
 *
 */
package com.bl.core.dao.impl;

import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.servicelayer.exceptions.ModelNotFoundException;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.dao.BlAddressDao;
import com.bl.core.model.BlItemsBillingChargeModel;


/**
 * @author srinivas
 *
 */
public class BlAddressDaoImpl implements BlAddressDao
{
	private static final Logger LOG = Logger.getLogger(BlAddressDaoImpl.class);
	private FlexibleSearchService flexibleSearchService;
	private static final String FIND_ADDRESS_BY_ADDRESSID = "SELECT {" + AddressModel.PK
			+ "} FROM {"
			+ AddressModel._TYPECODE + "} WHERE {" + AddressModel.ADDRESSID + "} =?addressId";//NOSONAR
	private static final String GET_BILLCHARGE_CODES = "SELECT {" + BlItemsBillingChargeModel.PK
			+ "} FROM {"
			+ BlItemsBillingChargeModel._TYPECODE
			+ "} WHERE {" + BlItemsBillingChargeModel.CODE + "} IN (?code)";//NOSONAR

	@Override
	public AddressModel getAddressById(final String addressId)
	{
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(FIND_ADDRESS_BY_ADDRESSID);
		fQuery.addQueryParameter(BlCoreConstants.ADDRESS_ID, addressId);
		final SearchResult<AddressModel> search = getFlexibleSearchService().<AddressModel> search(fQuery);
		if (CollectionUtils.isNotEmpty(search.getResult()))
		{
			return search.getResult().get(0);
		}
		return null;
	}

	@Override
	public List<BlItemsBillingChargeModel> getBillChargeList(final String[] chargeCodes)
	{
		try
		{
			final StringBuilder query = new StringBuilder(GET_BILLCHARGE_CODES);
			final Map<String, Object> queryParams = new HashMap<>();
			queryParams.put("code", Arrays.asList(chargeCodes));
			final SearchResult<BlItemsBillingChargeModel> search = getFlexibleSearchService()
					.<BlItemsBillingChargeModel> search(query.toString(), queryParams);
			if (CollectionUtils.isNotEmpty(search.getResult()))
			{
				final List<BlItemsBillingChargeModel> blList = search.getResult();
				return blList;
			}
		}
		catch (final ModelNotFoundException ex)
		{
			LOG.error("Model not found Exception" + ex);
		}
		return Collections.emptyList();
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
