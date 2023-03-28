/**
 *
 */
package com.braintree.configuration.service;

import static com.braintree.constants.BraintreeConstants.BRAINTREE_3D_SECURE;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_ACCEPTED_PAYMENT_METHODS;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_IMAGES_PREFIX;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_MERCHANT_ACCOUNT_PREFIX;
import static com.braintree.constants.BraintreeConstants.CONFIGURATION_PROPERTY_DELIMETER;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_SUBMIT_FOR_SETTLEMENT;
import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.basecommerce.model.site.BaseSiteModel;
import de.hybris.platform.core.model.c2l.CurrencyModel;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.site.BaseSiteService;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.configuration.Configuration;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;


@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class BrainTreeConfigServiceTest
{
	private static final String PAYMENT_METHOD1 = "PAYMENT_METHOD1";
	private static final String IMAGE_LINK1 = "IMAGE_LINK1";
	private static final String PAYMENT_METHOD2 = "PAYMENT_METHOD2";
	private static final String IMAGE_LINK2 = "IMAGE_LINK2";
	private static final String PAYMENT_METHODS_STRING_ONE_PAYMENT = PAYMENT_METHOD1;
	private static final String PAYMENT_METHODS_STRING_MORE_THAN_ONE_PAYMENT = PAYMENT_METHOD1 + ";" + PAYMENT_METHOD2;

	private static final String MERCHANT_ACCOUNT_ID = "MERCHANT_ACCOUNT_ID";
	private static final String UID = "uid";
	private static final String ISO_CODE = "iso_code";

	@Mock
	private ConfigurationService configurationService;
	@Mock
	private BaseSiteService baseSiteService;
	@Mock
	private CommonI18NService commonI18NService;

	@InjectMocks
	private final BrainTreeConfigService brainTreeCustomerAccountService = new BrainTreeConfigService();

	@Before
	public void init()
	{
		//MockitoAnnotations.initMocks(this);
	}

	@Test
	public void shouldGetSettlementConfigParameter()
	{
		final Boolean expected = Boolean.TRUE;
		final Configuration configuration = mock(Configuration.class);
		when(configurationService.getConfiguration()).thenReturn(configuration);
		when(Boolean.valueOf(configuration.getBoolean(BRAINTREE_SUBMIT_FOR_SETTLEMENT, false))).thenReturn(expected);

		final Boolean actual = brainTreeCustomerAccountService.getSettlementConfigParameter();

		assertEquals(expected, actual);
	}

	@Test
	public void shouldGet3dSecureConfiguration()
	{
		final Boolean expected = Boolean.TRUE;
		final Configuration configuration = mock(Configuration.class);
		when(configurationService.getConfiguration()).thenReturn(configuration);
		when(Boolean.valueOf(configuration.getBoolean(BRAINTREE_3D_SECURE, false))).thenReturn(expected);

		final Boolean actual = brainTreeCustomerAccountService.get3dSecureConfiguration();

		assertEquals(expected, actual);
	}

	@Test
	public void shouldGetAcceptedPaymentMethodImagesNoAcceptedPaymentMethods()
	{
		final Map<String, String> acceptedPaymentMethodImagesExpected = new HashMap<>();
		final Configuration configuration = mock(Configuration.class);
		when(configurationService.getConfiguration()).thenReturn(configuration);
		when(configuration.getString(BRAINTREE_ACCEPTED_PAYMENT_METHODS)).thenReturn("");

		final Map<String, String> acceptedPaymentMethodImagesActual = brainTreeCustomerAccountService
				.getAcceptedPaymentMethodImages();

		assertEquals(acceptedPaymentMethodImagesExpected, acceptedPaymentMethodImagesActual);
	}

	@Test
	public void shouldGetAcceptedPaymentMethodImagesOneAcceptedPaymentMethods()
	{
		final Map<String, String> acceptedPaymentMethodImagesExpected = new HashMap<>();
		acceptedPaymentMethodImagesExpected.put(PAYMENT_METHOD1, IMAGE_LINK1);
		final Configuration configuration = mock(Configuration.class);
		when(configurationService.getConfiguration()).thenReturn(configuration);
		when(configuration.getString(BRAINTREE_ACCEPTED_PAYMENT_METHODS)).thenReturn(PAYMENT_METHODS_STRING_ONE_PAYMENT);
		when(configuration.getString(BRAINTREE_IMAGES_PREFIX + PAYMENT_METHOD1)).thenReturn(IMAGE_LINK1);

		final Map<String, String> acceptedPaymentMethodImagesActual = brainTreeCustomerAccountService
				.getAcceptedPaymentMethodImages();

		assertEquals(acceptedPaymentMethodImagesExpected, acceptedPaymentMethodImagesActual);
	}

	@Test
	public void shouldGetAcceptedPaymentMethodImagesMoreThanOneAcceptedPaymentMethods()
	{
		final Map<String, String> acceptedPaymentMethodImagesExpected = new HashMap<>();
		acceptedPaymentMethodImagesExpected.put(PAYMENT_METHOD1, IMAGE_LINK1);
		acceptedPaymentMethodImagesExpected.put(PAYMENT_METHOD2, IMAGE_LINK2);
		final Configuration configuration = mock(Configuration.class);
		when(configurationService.getConfiguration()).thenReturn(configuration);
		when(configuration.getString(BRAINTREE_ACCEPTED_PAYMENT_METHODS)).thenReturn(PAYMENT_METHODS_STRING_MORE_THAN_ONE_PAYMENT);
		when(configuration.getString(BRAINTREE_IMAGES_PREFIX + PAYMENT_METHOD1)).thenReturn(IMAGE_LINK1);
		when(configuration.getString(BRAINTREE_IMAGES_PREFIX + PAYMENT_METHOD2)).thenReturn(IMAGE_LINK2);

		final Map<String, String> acceptedPaymentMethodImagesActual = brainTreeCustomerAccountService
				.getAcceptedPaymentMethodImages();

		assertEquals(acceptedPaymentMethodImagesExpected, acceptedPaymentMethodImagesActual);
	}

	@Test
	public void shouldGetMerchantAccountIdByCurrentSiteNameAndCurrency()
	{
		// given
		final BaseSiteModel currentBaseSite = mock(BaseSiteModel.class);
		final CurrencyModel currencyModel = mock(CurrencyModel.class);
		final Configuration configuration = mock(Configuration.class);
		when(baseSiteService.getCurrentBaseSite()).thenReturn(currentBaseSite);
		when(currentBaseSite.getUid()).thenReturn(UID);
		when(commonI18NService.getCurrentCurrency()).thenReturn(currencyModel);
		when(currencyModel.getIsocode()).thenReturn(ISO_CODE);
		when(configurationService.getConfiguration()).thenReturn(configuration);
		when(configuration.getString(BRAINTREE_MERCHANT_ACCOUNT_PREFIX + UID + CONFIGURATION_PROPERTY_DELIMETER + ISO_CODE))
				.thenReturn(MERCHANT_ACCOUNT_ID);

		// when
		final String merchantAccountIdActual = brainTreeCustomerAccountService.getMerchantAccountIdForCurrentSiteAndCurrency();

		// then
		assertEquals(MERCHANT_ACCOUNT_ID, merchantAccountIdActual);
	}

	@Test
	public void shouldGetMerchantAccountIdByCurrentSiteNameAndCurrencyIsoCode()
	{
		// given
		final BaseSiteModel currentBaseSite = mock(BaseSiteModel.class);
		final Configuration configuration = mock(Configuration.class);
		when(baseSiteService.getCurrentBaseSite()).thenReturn(currentBaseSite);
		when(currentBaseSite.getUid()).thenReturn(UID);
		when(configurationService.getConfiguration()).thenReturn(configuration);
		when(configuration.getString(BRAINTREE_MERCHANT_ACCOUNT_PREFIX + UID + CONFIGURATION_PROPERTY_DELIMETER + ISO_CODE))
				.thenReturn(MERCHANT_ACCOUNT_ID);

		// when
		final String merchantAccountIdActual = brainTreeCustomerAccountService
				.getMerchantAccountIdForCurrentSiteAndCurrencyIsoCode(ISO_CODE);

		// then
		assertEquals(MERCHANT_ACCOUNT_ID, merchantAccountIdActual);
	}

}
