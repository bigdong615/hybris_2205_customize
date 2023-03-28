/**
 *
 */
package com.braintree.customer.dao;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.PK;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import com.braintree.model.BrainTreePaymentInfoModel;


@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class BrainTreeCustomerAccountDaoTest
{
	private static final String CUSTOMER_ID = "CUSTOMER_ID";

	private static final String ACCOUNT_ID = "ACCOUNT_ID";

	private static final String CODE = "1";

	private static final String BT_PAYMENT_INFO_SEARCH_QUERY = "SELECT {pk} FROM {BrainTreePaymentInfo} WHERE {user} = ?customer AND {pk} = ?pk AND {duplicate} = ?duplicate";

	private static final String CUSTOMER_SEARCH_QUERY = "SELECT {pk} FROM {Customer} WHERE {braintreeCustomerId} = ?customerId";

	private static final String BT_PAYMENT_INFO_LIST_SEARCH_QUERY = "SELECT {pk} FROM {BrainTreePaymentInfo} WHERE {user} = ?customer AND {saved} = ?saved ";

	private static final String BT_PAYMENT_INFO_LIST_SEARCH_QUERY_APPEND_ACCOUNT = "AND {merchantAccountIdForCurrentSite} = ?accountId ";

	private static final String BT_PAYMENT_INFO_LIST_SEARCH_QUERY_APPEND_DUPLICATE = "AND {duplicate} = ?duplicate";

	private static final String BT_PAYMENT_INFO_LIST_SEARCH_QUERY_NOT_SAVED = "SELECT {pk} FROM {BrainTreePaymentInfo} WHERE {user} = ?customer AND {duplicate} = ?duplicate";

	public static final Boolean DUPLICATE = Boolean.FALSE;

	private static final int COUNT = 1;

	private static final int COUNT_ZERO = 0;

	public static final boolean SAVED = true;

	public static final boolean NOT_SAVED = false;

	@Mock
	private FlexibleSearchService flexibleSearchService;

	@InjectMocks
	private final BrainTreeCustomerAccountDao defaultBrainTreePaymentInfoDao = new BrainTreeCustomerAccountDao();

	@Before
	public void init()
	{
		//MockitoAnnotations.initMocks(this);
	}

	@Test
	public void shouldFindBrainTreePaymentInfoByCustomer()
	{
		// given
		final BrainTreePaymentInfoModel expectedBrainTreePaymentInfoModel = mock(BrainTreePaymentInfoModel.class);
		final CustomerModel customerModel = mock(CustomerModel.class);
		final SearchResult searchResult = mock(SearchResult.class);
		final List list = mock(List.class);
		final Map queryParams = new HashMap();
		queryParams.put("customer", customerModel);
		queryParams.put("pk", PK.parse(CODE));
		queryParams.put("duplicate", DUPLICATE);
		when(flexibleSearchService.search(BT_PAYMENT_INFO_SEARCH_QUERY, queryParams)).thenReturn(searchResult);
		when(Integer.valueOf(searchResult.getCount())).thenReturn(Integer.valueOf(COUNT));
		when(searchResult.getResult()).thenReturn(list);
		when(list.get(0)).thenReturn(expectedBrainTreePaymentInfoModel);

		// when
		final BrainTreePaymentInfoModel actualBrainTreePaymentInfoModel = defaultBrainTreePaymentInfoDao
				.findBrainTreePaymentInfoByCustomer(customerModel, CODE);

		// then
		assertEquals(expectedBrainTreePaymentInfoModel, actualBrainTreePaymentInfoModel);
	}

	@Test
	public void shouldNotFindBrainTreePaymentInfoByCustomer()
	{
		// given
		final CustomerModel customerModel = mock(CustomerModel.class);
		final SearchResult searchResult = mock(SearchResult.class);
		final Map queryParams = new HashMap();
		queryParams.put("customer", customerModel);
		queryParams.put("pk", PK.parse(CODE));
		queryParams.put("duplicate", DUPLICATE);
		when(flexibleSearchService.search(BT_PAYMENT_INFO_SEARCH_QUERY, queryParams)).thenReturn(searchResult);
		when(Integer.valueOf(searchResult.getCount())).thenReturn(Integer.valueOf(COUNT_ZERO));

		// when
		final BrainTreePaymentInfoModel actualBrainTreePaymentInfoModel = defaultBrainTreePaymentInfoDao
				.findBrainTreePaymentInfoByCustomer(customerModel, CODE);

		// then
		assertNull(actualBrainTreePaymentInfoModel);
	}

	@Test
	public void shouldFindBrainTreePaymentInfosByCustomer()
	{
		// given
		final CustomerModel customerModel = mock(CustomerModel.class);
		final List<BrainTreePaymentInfoModel> expectedBrainTreePaymentInfoModels = mock(List.class);
		final SearchResult searchResult = mock(SearchResult.class);
		final Map queryParams = new HashMap();
		queryParams.put("customer", customerModel);
		queryParams.put("saved", Boolean.valueOf(SAVED));
		queryParams.put("accountId", ACCOUNT_ID);
		queryParams.put("duplicate", DUPLICATE);
		final StringBuilder queryBySaved = new StringBuilder(BT_PAYMENT_INFO_LIST_SEARCH_QUERY);
		queryBySaved.append(BT_PAYMENT_INFO_LIST_SEARCH_QUERY_APPEND_ACCOUNT);
		queryBySaved.append(BT_PAYMENT_INFO_LIST_SEARCH_QUERY_APPEND_DUPLICATE);

		when(flexibleSearchService
				.search(
						(SAVED) ? queryBySaved.toString()
								: "SELECT {pk} FROM {BrainTreePaymentInfo} WHERE {user} = ?customer AND {duplicate} = ?duplicate",
						queryParams)).thenReturn(searchResult);
		when(searchResult.getResult()).thenReturn(expectedBrainTreePaymentInfoModels);

		// when
		final List<BrainTreePaymentInfoModel> actualBrainTreePaymentInfoModels = defaultBrainTreePaymentInfoDao
				.findBrainTreePaymentInfosByCustomer(customerModel, SAVED, ACCOUNT_ID);

		// then
		assertEquals(expectedBrainTreePaymentInfoModels, actualBrainTreePaymentInfoModels);
	}

	@Test
	public void shouldFindNotSavedBrainTreePaymentInfosByCustomer()
	{
		// given
		final CustomerModel customerModel = mock(CustomerModel.class);
		final List<BrainTreePaymentInfoModel> expectedBrainTreePaymentInfoModels = mock(List.class);
		final SearchResult searchResult = mock(SearchResult.class);
		final Map queryParams = new HashMap();
		queryParams.put("customer", customerModel);
		queryParams.put("duplicate", DUPLICATE);
		final StringBuilder queryBySaved = new StringBuilder(BT_PAYMENT_INFO_LIST_SEARCH_QUERY);
		queryBySaved.append(BT_PAYMENT_INFO_LIST_SEARCH_QUERY_APPEND_DUPLICATE);

		when(flexibleSearchService.search((NOT_SAVED) ? queryBySaved.toString() : BT_PAYMENT_INFO_LIST_SEARCH_QUERY_NOT_SAVED,
				queryParams)).thenReturn(searchResult);
		when(searchResult.getResult()).thenReturn(expectedBrainTreePaymentInfoModels);

		// when
		final List<BrainTreePaymentInfoModel> actualBrainTreePaymentInfoModels = defaultBrainTreePaymentInfoDao
				.findBrainTreePaymentInfosByCustomer(customerModel, NOT_SAVED, ACCOUNT_ID);

		// then
		assertEquals(expectedBrainTreePaymentInfoModels, actualBrainTreePaymentInfoModels);
	}

	@Test
	public void shouldFindBrainTreePaymentInfosByCustomerIfAccountIsNull()
	{
		// given
		final CustomerModel customerModel = mock(CustomerModel.class);
		final List<BrainTreePaymentInfoModel> expectedBrainTreePaymentInfoModels = mock(List.class);
		final SearchResult searchResult = mock(SearchResult.class);
		final Map queryParams = new HashMap();
		queryParams.put("customer", customerModel);
		queryParams.put("saved", Boolean.valueOf(SAVED));
		queryParams.put("duplicate", DUPLICATE);
		final StringBuilder queryBySaved = new StringBuilder(BT_PAYMENT_INFO_LIST_SEARCH_QUERY);
		queryBySaved.append(BT_PAYMENT_INFO_LIST_SEARCH_QUERY_APPEND_DUPLICATE);

		when(flexibleSearchService
				.search(
						(SAVED) ? queryBySaved.toString()
								: "SELECT {pk} FROM {BrainTreePaymentInfo} WHERE {user} = ?customer AND {duplicate} = ?duplicate",
						queryParams)).thenReturn(searchResult);
		when(searchResult.getResult()).thenReturn(expectedBrainTreePaymentInfoModels);

		// when
		final List<BrainTreePaymentInfoModel> actualBrainTreePaymentInfoModels = defaultBrainTreePaymentInfoDao
				.findBrainTreePaymentInfosByCustomer(customerModel, SAVED, null);

		// then
		assertEquals(expectedBrainTreePaymentInfoModels, actualBrainTreePaymentInfoModels);
	}

	@Test
	public void shouldFindCustomerByBrainTreeCustomerId()
	{
		// given
		final CustomerModel expectedCustomerModel = mock(CustomerModel.class);
		final SearchResult searchResult = mock(SearchResult.class);
		final List list = mock(List.class);
		final Map queryParams = new HashMap();
		queryParams.put("customerId", CUSTOMER_ID);
		when(flexibleSearchService.search(CUSTOMER_SEARCH_QUERY, queryParams)).thenReturn(searchResult);
		when(Integer.valueOf(searchResult.getCount())).thenReturn(Integer.valueOf(COUNT));
		when(searchResult.getResult()).thenReturn(list);
		when(list.get(0)).thenReturn(expectedCustomerModel);

		// when
		final CustomerModel actualCustomerModel = defaultBrainTreePaymentInfoDao.findCustomerByBrainTreeCustomerId(CUSTOMER_ID);

		// then
		assertEquals(expectedCustomerModel, actualCustomerModel);
	}

	@Test
	public void shouldNotFindCustomerByBrainTreeCustomerId()
	{
		// given
		final SearchResult searchResult = mock(SearchResult.class);
		final Map queryParams = new HashMap();
		queryParams.put("customerId", CUSTOMER_ID);
		when(flexibleSearchService.search(CUSTOMER_SEARCH_QUERY, queryParams)).thenReturn(searchResult);
		when(Integer.valueOf(searchResult.getCount())).thenReturn(Integer.valueOf(COUNT_ZERO));

		// when
		final CustomerModel actualCustomerModel = defaultBrainTreePaymentInfoDao.findCustomerByBrainTreeCustomerId(CUSTOMER_ID);

		// then
		assertNull(actualCustomerModel);
	}

}
