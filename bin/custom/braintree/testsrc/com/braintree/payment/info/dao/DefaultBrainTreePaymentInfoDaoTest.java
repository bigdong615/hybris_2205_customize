/**
 *
 */
package com.braintree.payment.info.dao;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
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
public class DefaultBrainTreePaymentInfoDaoTest
{

	private static final String CUSTOMER_ID = "CUSTOMER_ID";

	private static final String PAYMENT_METHOD_TOKEN = "PAYMENT_METHOD_TOKEN";

	private static final String SEARCH_QUERY = "SELECT {pk} FROM {BrainTreePaymentInfo} WHERE {customerId}=?customerId "
			+ "AND {paymentMethodToken}=?paymentMethodToken AND {duplicate}=?duplicate";

	public static final Boolean DUPLICATE = Boolean.FALSE;

	private static final int COUNT = 1;

	private static final int COUNT_ZERO = 0;

	@Mock
	private FlexibleSearchService flexibleSearchService;

	@InjectMocks
	private final BrainTreePaymentInfoDao defaultBrainTreePaymentInfoDao = new DefaultBrainTreePaymentInfoDao();

	@Before
	public void init()
	{
		//MockitoAnnotations.initMocks(this);
	}

	@Test
	public void shouldFindBrainTreePaymentInfo()
	{
		// given
		final BrainTreePaymentInfoModel expectedBrainTreePaymentInfoModel = mock(BrainTreePaymentInfoModel.class);
		final SearchResult searchResult = mock(SearchResult.class);
		final Map queryParams = new HashMap();
		final List list = mock(List.class);
		queryParams.put("customerId", CUSTOMER_ID);
		queryParams.put("paymentMethodToken", PAYMENT_METHOD_TOKEN);
		queryParams.put("duplicate", DUPLICATE);
		when(flexibleSearchService.search(SEARCH_QUERY, queryParams)).thenReturn(searchResult);
		when(Integer.valueOf(searchResult.getCount())).thenReturn(Integer.valueOf(COUNT));
		when(searchResult.getResult()).thenReturn(list);
		when(list.get(0)).thenReturn(expectedBrainTreePaymentInfoModel);

		// when
		final BrainTreePaymentInfoModel actualBrainTreePaymentInfoModel = defaultBrainTreePaymentInfoDao.find(CUSTOMER_ID,
				PAYMENT_METHOD_TOKEN);

		// then
		assertEquals(expectedBrainTreePaymentInfoModel, actualBrainTreePaymentInfoModel);
	}

	@Test
	public void shouldNotFindBrainTreePaymentInfo()
	{
		// given
		final SearchResult searchResult = mock(SearchResult.class);
		final Map queryParams = new HashMap();
		queryParams.put("customerId", CUSTOMER_ID);
		queryParams.put("paymentMethodToken", PAYMENT_METHOD_TOKEN);
		queryParams.put("duplicate", DUPLICATE);
		when(flexibleSearchService.search(SEARCH_QUERY, queryParams)).thenReturn(searchResult);
		when(Integer.valueOf(searchResult.getCount())).thenReturn(Integer.valueOf(COUNT_ZERO));

		// when
		final BrainTreePaymentInfoModel actualBrainTreePaymentInfoModel = defaultBrainTreePaymentInfoDao.find(CUSTOMER_ID,
				PAYMENT_METHOD_TOKEN);

		// then
		assertNull(actualBrainTreePaymentInfoModel);
	}

}
