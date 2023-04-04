/**
 *
 */
package com.braintree.facade.impl;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commerceservices.customer.CustomerAccountService;
import de.hybris.platform.commerceservices.strategies.CheckoutCustomerStrategy;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.order.CartService;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import com.braintree.hybris.data.BrainTreePaymentInfoData;
import com.braintree.method.BrainTreePaymentService;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.paypal.converters.impl.BrainTreePaymentInfoDataConverter;


@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class BrainTreePaymentFacadeImplTest
{

	private static final String ORDER_CODE = "123456";

	@Mock
	private BrainTreePaymentService brainTreePaymentService;

	@Mock
	private CheckoutCustomerStrategy checkoutCustomerStrategy;

	@Mock
	private CartService cartService;

	@Mock
	private BaseStoreService baseStoreService;

	@Mock
	private CustomerAccountService customerAccountService;

	@Mock
	private BrainTreePaymentInfoDataConverter brainTreePaymentInfoDataConverter;

	@InjectMocks
	private final BrainTreePaymentFacadeImpl brainTreePaymentFacadeImpl = new BrainTreePaymentFacadeImpl();

	@Before
	public void init()
	{
		//MockitoAnnotations.initMocks(this);
	}

	@Test
	public void shouldGetBrainTreePaymentInfoData()
	{
		final CartModel cartModel = mock(CartModel.class);
		final BrainTreePaymentInfoModel paymentInfoModel = mock(BrainTreePaymentInfoModel.class);
		final BrainTreePaymentInfoData brainTreePaymentInfoDataExpected = mock(BrainTreePaymentInfoData.class);

		when(cartService.getSessionCart()).thenReturn(cartModel);
		when(cartModel.getPaymentInfo()).thenReturn(paymentInfoModel);
		when(brainTreePaymentInfoDataConverter.convert(paymentInfoModel)).thenReturn(brainTreePaymentInfoDataExpected);

		final BrainTreePaymentInfoData brainTreePaymentInfoDataActual = brainTreePaymentFacadeImpl.getBrainTreePaymentInfoData();

		assertEquals(brainTreePaymentInfoDataExpected, brainTreePaymentInfoDataActual);

	}

	@Test
	public void shouldGetBrainTreePaymentInfoDataByOrderCode()
	{
		final BrainTreePaymentInfoData brainTreePaymentInfoDataExpected = mock(BrainTreePaymentInfoData.class);
		final BrainTreePaymentInfoModel paymentInfo = mock(BrainTreePaymentInfoModel.class);
		final OrderModel order = mock(OrderModel.class);
		final BaseStoreModel baseStoreModel = mock(BaseStoreModel.class);

		when(baseStoreService.getCurrentBaseStore()).thenReturn(baseStoreModel);
		when(customerAccountService.getOrderForCode(ORDER_CODE, baseStoreModel)).thenReturn(order);
		when(order.getPaymentInfo()).thenReturn(paymentInfo);
		when(brainTreePaymentInfoDataConverter.convert(paymentInfo)).thenReturn(brainTreePaymentInfoDataExpected);

		final BrainTreePaymentInfoData brainTreePaymentInfoDataActual = brainTreePaymentFacadeImpl
				.getBrainTreePaymentInfoData(ORDER_CODE);

		assertEquals(brainTreePaymentInfoDataExpected, brainTreePaymentInfoDataActual);
	}

}
