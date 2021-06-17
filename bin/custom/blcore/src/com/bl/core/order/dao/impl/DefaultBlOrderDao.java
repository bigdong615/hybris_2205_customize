package com.bl.core.order.dao.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.order.dao.BlOrderDao;
import com.bl.core.suggestion.dao.SimpleSuggestionDao;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.order.daos.impl.DefaultOrderDao;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.SearchResult;
import java.util.Collections;
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


/**
 * Default implementation of {@link SimpleSuggestionDao}.
 * @author Moumita
 */
public class DefaultBlOrderDao extends DefaultOrderDao implements BlOrderDao
{
	private static final Logger LOG = Logger.getLogger(DefaultBlOrderDao.class);
	private static final String GET_ORDERS_FOR_AUTHORIZATION_QUERY = "SELECT {" + ItemModel.PK + "} FROM {"
			+ OrderModel._TYPECODE + " AS o} WHERE {o:" + AbstractOrderModel.ISAUTHORISED + "} = ?isAuthorized ";

	/**
 	* {@inheritDoc}
 	*/
	@Override
	public List<AbstractOrderModel> getOrdersForAuthorization()
	{
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(GET_ORDERS_FOR_AUTHORIZATION_QUERY);
		fQuery.addQueryParameter(BlCoreConstants.IS_AUTHORISED, Boolean.FALSE);
		final SearchResult result = getFlexibleSearchService().search(fQuery);
		final List<AbstractOrderModel> ordersToAuthorizePayment = result.getResult();
		if (CollectionUtils.isEmpty(ordersToAuthorizePayment))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "No orders found to authorize the payment");
			return Collections.emptyList();
		}
		return ordersToAuthorizePayment;
	}

}
