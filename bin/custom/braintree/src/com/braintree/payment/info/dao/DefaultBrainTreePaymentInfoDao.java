package com.braintree.payment.info.dao;

import com.braintree.model.BrainTreePaymentInfoModel;
import de.hybris.platform.servicelayer.internal.dao.AbstractItemDao;
import de.hybris.platform.servicelayer.search.SearchResult;
import de.hybris.platform.servicelayer.util.ServicesUtil;

import java.util.HashMap;
import java.util.Map;


public class DefaultBrainTreePaymentInfoDao extends AbstractItemDao implements BrainTreePaymentInfoDao
{

	@Override
	public BrainTreePaymentInfoModel find(final String customerId, final String paymentMethodToken)
	{
		ServicesUtil.validateParameterNotNull(customerId, "customerId must not be null");
		ServicesUtil.validateParameterNotNull(paymentMethodToken, "paymentMethodToken must not be null");
		final Map queryParams = new HashMap();
		queryParams.put("customerId", customerId);
		queryParams.put("paymentMethodToken", paymentMethodToken);
		queryParams.put("duplicate", Boolean.FALSE);

		final SearchResult result = getFlexibleSearchService().search(
				"SELECT {pk} FROM {BrainTreePaymentInfo} WHERE {customerId}=?customerId "
						+ "AND {paymentMethodToken}=?paymentMethodToken AND {duplicate}=?duplicate", queryParams);

		return ((result.getCount() > 0) ? (BrainTreePaymentInfoModel) result.getResult().get(0) : null);
	}

}
