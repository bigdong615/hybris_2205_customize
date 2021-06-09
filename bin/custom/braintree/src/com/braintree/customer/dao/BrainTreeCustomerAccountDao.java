/**
 *
 */
package com.braintree.customer.dao;

import com.braintree.model.BrainTreePaymentInfoModel;
import de.hybris.platform.commerceservices.customer.dao.impl.DefaultCustomerAccountDao;
import de.hybris.platform.core.PK;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.search.SearchResult;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import org.apache.log4j.Logger;

import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class BrainTreeCustomerAccountDao extends DefaultCustomerAccountDao
{
	private final static Logger LOG = Logger.getLogger(BrainTreeCustomerAccountDao.class);

	public BrainTreePaymentInfoModel findBrainTreePaymentInfoByCustomer(final CustomerModel customerModel, final String code)
	{
		ServicesUtil.validateParameterNotNull(customerModel, "Customer must not be null");
		final Map queryParams = new HashMap();
		queryParams.put("customer", customerModel);
		queryParams.put("duplicate", Boolean.FALSE);
		queryParams.put("pk", PK.parse(code));
		final SearchResult result = getFlexibleSearchService().search(
				"SELECT {" + BrainTreePaymentInfoModel.PK + "} FROM {" + BrainTreePaymentInfoModel._TYPECODE + "} WHERE {" +
						BrainTreePaymentInfoModel.USER + "} = ?customer AND {" + BrainTreePaymentInfoModel.PK + "} = ?pk AND {"
						+ BrainTreePaymentInfoModel.DUPLICATE + "} = ?duplicate",
				queryParams);

		return ((result.getCount() > 0) ? (BrainTreePaymentInfoModel) result.getResult().get(0) : null);
	}

	public List<BrainTreePaymentInfoModel> findBrainTreePaymentInfosByCustomer(final CustomerModel customerModel,
			final boolean saved, final String accountId)
	{
		final Map queryParams = new HashMap();
		queryParams.put("duplicate", Boolean.FALSE);
		final StringBuilder queryBySaved = new StringBuilder(
				"SELECT {" + BrainTreePaymentInfoModel.PK + "} FROM {" + BrainTreePaymentInfoModel._TYPECODE + "} WHERE {" +
						BrainTreePaymentInfoModel.USER + "} = ?customer AND {" + BrainTreePaymentInfoModel.SAVED + "} = ?saved AND {"
						+ BrainTreePaymentInfoModel.DUPLICATE + "} = ?duplicate");
		ServicesUtil.validateParameterNotNull(customerModel, "Customer must not be null");

		queryParams.put("customer", customerModel);

		if (saved)
		{
			queryParams.put("saved", Boolean.TRUE);
			if (accountId != null)
			{
				queryParams.put("accountId", accountId);
			}
			else
			{
				LOG.error("[Get Payment Infos ERROR] Merchant account ID is null! Search default payment infos... "
						+ "Please check configuration of property: braintree.merchant.account.ids");
			}

		}
		final SearchResult result = getFlexibleSearchService().search(
				(saved) ? queryBySaved.toString()
						: "SELECT {" + BrainTreePaymentInfoModel.PK + "} FROM {" + BrainTreePaymentInfoModel._TYPECODE + "} WHERE {" +
						BrainTreePaymentInfoModel.USER + "} = ?customer AND {" + BrainTreePaymentInfoModel.DUPLICATE + "} = ?duplicate",
				queryParams);
		return result.getResult();
	}

	public CustomerModel findCustomerByBrainTreeCustomerId(final String customerId)
	{
		ServicesUtil.validateParameterNotNull(customerId, "Customer must not be null");
		final Map queryParams = new HashMap();
		queryParams.put("customerId", customerId);

		final SearchResult result = getFlexibleSearchService().search(
				"SELECT {" + CustomerModel.PK + "} FROM {" + CustomerModel._TYPECODE + "} WHERE {" + CustomerModel.BRAINTREECUSTOMERID
						+ "} = ?customerId", queryParams);
		return ((result.getCount() > 0) ? (CustomerModel) result.getResult().get(0) : null);
	}
}
