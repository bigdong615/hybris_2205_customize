/**
 *
 */
package com.braintree.store.currency;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commercefacades.storesession.data.CurrencyData;
import de.hybris.platform.commerceservices.storesession.StoreSessionService;
import de.hybris.platform.core.model.c2l.CurrencyModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.payment.PaymentInfoModel;
import de.hybris.platform.order.CartService;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.model.ModelService;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InOrder;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;

import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.model.BrainTreePaymentInfoModel;


@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class BrainTreeStoreSessionFacadeTest
{

	private static final String CURRENCY_EQ_ISOCODE = "ISOCODE";

	private static final String CURRENCY_NOT_EQ_ISOCODE = "NOT_ISOCODE";

	private static final String ISOCODE = "ISOCODE";

	private static final String ACCOUNT_ID = "ACCOUNT_ID";

	@Mock
	private Converter currencyConverter;

	@Mock
	private CommonI18NService commonI18NService;

	@Mock
	private BrainTreeConfigService brainTreeConfigService;

	@Mock
	private CurrencyData currencyData;

	@Mock
	private StoreSessionService storeSessionService;

	@Mock
	private ModelService modelService;

	@Mock
	private CartService cartService;

	@InjectMocks
	private final BrainTreeStoreSessionFacade brainTreeStoreSessionFacade = new BrainTreeStoreSessionFacade();

	@Before
	public void init()
	{
		//MockitoAnnotations.initMocks(this);
		final CurrencyModel currencyModel = mock(CurrencyModel.class);
		when(commonI18NService.getCurrentCurrency()).thenReturn(currencyModel);
		when(currencyConverter.convert(currencyModel)).thenReturn(currencyData);
	}

	@Test
	public void shouldSetCurrentCurrency()
	{
		final CartModel cart = mock(CartModel.class);
		final BrainTreePaymentInfoModel paymentInfo = mock(BrainTreePaymentInfoModel.class);

		when(Boolean.valueOf(cartService.hasSessionCart())).thenReturn(Boolean.TRUE);
		when(cartService.getSessionCart()).thenReturn(cart);
		when(cart.getPaymentInfo()).thenReturn(paymentInfo);
		when(brainTreeConfigService.getMerchantAccountIdForCurrentSiteAndCurrencyIsoCode(ISOCODE)).thenReturn(ACCOUNT_ID);

		brainTreeStoreSessionFacade.setCurrentCurrency(ISOCODE);

		final InOrder inOrder = Mockito.inOrder(storeSessionService, cartService, cart, brainTreeConfigService, modelService);
		inOrder.verify(storeSessionService).setCurrentCurrency(ISOCODE);
		inOrder.verify(cartService).getSessionCart();
		inOrder.verify(cart).getPaymentInfo();
		inOrder.verify(brainTreeConfigService).getMerchantAccountIdForCurrentSiteAndCurrencyIsoCode(ISOCODE);
		inOrder.verify(modelService).save(paymentInfo);
	}

	@Test
	public void shouldSetCurrentCurrencyNoSessionCart()
	{
		final CartModel cart = mock(CartModel.class);
		final BrainTreePaymentInfoModel paymentInfo = mock(BrainTreePaymentInfoModel.class);

		when(Boolean.valueOf(cartService.hasSessionCart())).thenReturn(Boolean.FALSE);

		brainTreeStoreSessionFacade.setCurrentCurrency(ISOCODE);

		final InOrder inOrder = Mockito.inOrder(storeSessionService, cartService, cart, brainTreeConfigService, modelService);
		inOrder.verify(storeSessionService).setCurrentCurrency(ISOCODE);
		inOrder.verify(cartService, never()).getSessionCart();
		inOrder.verify(cart, never()).getPaymentInfo();
		inOrder.verify(brainTreeConfigService, never()).getMerchantAccountIdForCurrentSiteAndCurrencyIsoCode(ISOCODE);
		inOrder.verify(modelService, never()).save(paymentInfo);
	}

	@Test
	public void shouldSetCurrentCurrencyIsocodeEqPrevCurrency()
	{
		final CartModel cart = mock(CartModel.class);
		final BrainTreePaymentInfoModel paymentInfo = mock(BrainTreePaymentInfoModel.class);

		when(Boolean.valueOf(cartService.hasSessionCart())).thenReturn(Boolean.TRUE);
		when(cartService.getSessionCart()).thenReturn(cart);
		when(cart.getPaymentInfo()).thenReturn(paymentInfo);
		when(currencyData.getIsocode()).thenReturn(CURRENCY_EQ_ISOCODE);

		brainTreeStoreSessionFacade.setCurrentCurrency(ISOCODE);

		final InOrder inOrder = Mockito.inOrder(storeSessionService, cartService, cart, brainTreeConfigService, modelService);
		inOrder.verify(storeSessionService).setCurrentCurrency(ISOCODE);
		inOrder.verify(cartService).getSessionCart();
		inOrder.verify(cart, never()).getPaymentInfo();
		inOrder.verify(brainTreeConfigService, never()).getMerchantAccountIdForCurrentSiteAndCurrencyIsoCode(ISOCODE);
		inOrder.verify(modelService, never()).save(paymentInfo);
	}

	@Test
	public void shouldSetCurrentCurrencyIsocodeNotEqPrevCurrencyAndPaymentInfoEqNull()
	{
		final CartModel cart = mock(CartModel.class);
		final BrainTreePaymentInfoModel paymentInfo = mock(BrainTreePaymentInfoModel.class);

		when(Boolean.valueOf(cartService.hasSessionCart())).thenReturn(Boolean.TRUE);
		when(cartService.getSessionCart()).thenReturn(cart);
		when(cart.getPaymentInfo()).thenReturn(null);
		when(currencyData.getIsocode()).thenReturn(CURRENCY_NOT_EQ_ISOCODE);

		brainTreeStoreSessionFacade.setCurrentCurrency(ISOCODE);

		final InOrder inOrder = Mockito.inOrder(storeSessionService, cartService, cart, brainTreeConfigService, modelService);
		inOrder.verify(storeSessionService).setCurrentCurrency(ISOCODE);
		inOrder.verify(cartService).getSessionCart();
		inOrder.verify(cart).getPaymentInfo();
		inOrder.verify(brainTreeConfigService, never()).getMerchantAccountIdForCurrentSiteAndCurrencyIsoCode(ISOCODE);
		inOrder.verify(modelService, never()).save(paymentInfo);
	}

	@Test
	public void shouldSetCurrentCurrencyIsocodeNotEqPrevCurrencyAndPaymentInfoNotBTPayment()
	{
		final CartModel cart = mock(CartModel.class);
		final PaymentInfoModel paymentInfo = mock(PaymentInfoModel.class);

		when(Boolean.valueOf(cartService.hasSessionCart())).thenReturn(Boolean.TRUE);
		when(cartService.getSessionCart()).thenReturn(cart);
		when(cart.getPaymentInfo()).thenReturn(paymentInfo);
		when(currencyData.getIsocode()).thenReturn(CURRENCY_NOT_EQ_ISOCODE);

		brainTreeStoreSessionFacade.setCurrentCurrency(ISOCODE);

		verify(storeSessionService).setCurrentCurrency(ISOCODE);

		final InOrder inOrder = Mockito.inOrder(storeSessionService, cartService, cart, brainTreeConfigService, modelService);
		inOrder.verify(cartService).getSessionCart();
		inOrder.verify(cart).getPaymentInfo();
		inOrder.verify(brainTreeConfigService, never()).getMerchantAccountIdForCurrentSiteAndCurrencyIsoCode(ISOCODE);
		inOrder.verify(modelService, never()).save(paymentInfo);
	}

}
