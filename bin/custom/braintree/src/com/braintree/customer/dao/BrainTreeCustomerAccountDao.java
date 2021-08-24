/**
 *
 */
package com.braintree.customer.dao;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import com.braintree.model.BrainTreePaymentInfoModel;
import de.hybris.platform.commerceservices.customer.dao.impl.DefaultCustomerAccountDao;
import de.hybris.platform.commerceservices.search.flexiblesearch.data.SortQueryData;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.core.PK;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.c2l.CountryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.search.SearchResult;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.util.Config;
import java.util.Arrays;
import java.util.Collection;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;

import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class BrainTreeCustomerAccountDao extends DefaultCustomerAccountDao
{
	private final static Logger LOG = Logger.getLogger(BrainTreeCustomerAccountDao.class);
	// Address Queries
	private static final String FIND_ADDRESS_BOOK_DELIVERY_ENTRIES = "SELECT {address:" + AddressModel.PK + "} FROM {"
			+ AddressModel._TYPECODE + " AS address LEFT JOIN " + CustomerModel._TYPECODE + " AS customer ON {address:"
			+ AddressModel.OWNER + "}={customer:" + CustomerModel.PK + "}} WHERE {customer:" + CustomerModel.PK
			+ "} = ?customer AND {address:"
			+ AddressModel.VISIBLEINADDRESSBOOK + "} = ?visibleInAddressBook AND {address:" + AddressModel.COUNTRY
			+ "} IN (?deliveryCountries)";



	private static final String FIND_ORDERS_BY_CUSTOMER_STORE_QUERY = "SELECT {" + OrderModel.PK + "}, {"
			+ OrderModel.MODIFIEDTIME + "}, {" + OrderModel.CODE + "} FROM {" + OrderModel._TYPECODE + "} WHERE {" + OrderModel.USER
			+ "} = ?customer AND {" + OrderModel.VERSIONID + "} IS NULL AND {" + OrderModel.STORE + "} = ?store "
			+ Config.getString("commerceservices.additional.order.status.filter", "");


	private static final String FIND_ORDERS_BY_CUSTOMER_STORE_QUERY_AND_STATUS = FIND_ORDERS_BY_CUSTOMER_STORE_QUERY + " AND {"
		+ OrderModel.STATUS + "} IN (?statusList)";

	private static final String FILTER_ORDER_STATUS = " AND {" + OrderModel.STATUS + "} NOT IN (?filterStatusList)";

	private static final String SORT_ORDERS_BY_DATE = " ORDER BY {" + OrderModel.MODIFIEDTIME + "} DESC, {" + OrderModel.PK + "}";

	private static final String SORT_ORDERS_BY_CODE = " ORDER BY {" + OrderModel.CODE + "},{" + OrderModel.MODIFIEDTIME
			+ "} DESC, {" + OrderModel.PK + "}";




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

	/**
	 *  This method used for getting visible address from db on the basis of current customer and delivery country.
	 * @param customerModel
	 * @param deliveryCountries
	 * @return
	 */
	@Override
	public List<AddressModel> findAddressBookDeliveryEntriesForCustomer(final CustomerModel customerModel,
			final Collection<CountryModel> deliveryCountries)
	{
		validateParameterNotNull(customerModel, "Customer must not be null");
		final Map<String, Object> queryParams = new HashMap<String, Object>();
		queryParams.put("customer", customerModel);
		queryParams.put("visibleInAddressBook", Boolean.TRUE);
		queryParams.put("deliveryCountries", deliveryCountries);
		final SearchResult<AddressModel> result = getFlexibleSearchService().search(FIND_ADDRESS_BOOK_DELIVERY_ENTRIES,
				queryParams);
		return result.getResult();
	}


	/**
	 * To get the list of order by modified time
	 */
	@Override
	public SearchPageData<OrderModel> findOrdersByCustomerAndStore(final CustomerModel customerModel, final BaseStoreModel store,
			final OrderStatus[] status, final PageableData pageableData)
	{
		validateParameterNotNull(customerModel, "Customer must not be null");
		validateParameterNotNull(store, "Store must not be null");

		final Map<String, Object> queryParams = new HashMap<String, Object>();
		queryParams.put("customer", customerModel);
		queryParams.put("store", store);

		String filterClause = StringUtils.EMPTY;
		if (CollectionUtils.isNotEmpty(getFilterOrderStatusList()))
		{
			queryParams.put("filterStatusList", getFilterOrderStatusList());
			filterClause = FILTER_ORDER_STATUS;
		}

		final List<SortQueryData> sortQueries;

		if (ArrayUtils.isNotEmpty(status))
		{
			queryParams.put("statusList", Arrays.asList(status));
			sortQueries = Arrays.asList(
					createSortQueryData("byDate",
							createQuery(FIND_ORDERS_BY_CUSTOMER_STORE_QUERY_AND_STATUS, filterClause, SORT_ORDERS_BY_DATE)),
					createSortQueryData("byOrderNumber",
							createQuery(FIND_ORDERS_BY_CUSTOMER_STORE_QUERY_AND_STATUS, filterClause, SORT_ORDERS_BY_CODE)));
		}
		else
		{
			sortQueries = Arrays
					.asList(
							createSortQueryData("byDate",
									createQuery(FIND_ORDERS_BY_CUSTOMER_STORE_QUERY, filterClause, SORT_ORDERS_BY_DATE)),
							createSortQueryData("byOrderNumber",
									createQuery(FIND_ORDERS_BY_CUSTOMER_STORE_QUERY, filterClause, SORT_ORDERS_BY_CODE)));
		}

		return getPagedFlexibleSearchService().search(sortQueries, "byDate", queryParams, pageableData);
	}

}
