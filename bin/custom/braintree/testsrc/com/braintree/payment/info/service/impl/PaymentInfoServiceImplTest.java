/**
 *
 */
package com.braintree.payment.info.service.impl;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.model.ModelService;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.customer.dao.BrainTreeCustomerAccountDao;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.payment.info.dao.BrainTreePaymentInfoDao;
import com.braintree.payment.info.service.PaymentInfoService;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class PaymentInfoServiceImplTest
{
	private static final String CUSTOMER_ID = "CUSTOMER_ID";

	private static final String PAYMENT_METHOD_TOKEN = "PAYMENT_METHOD_TOKEN";

	private static final String UID = "UID";

	public static final boolean SAVE_PAYMENT_INFO = true;

	public static final Boolean USE_PAYMENT_METHOD_TOKEN = Boolean.TRUE;

	public static final Boolean BRAINTREE_3D_SECURE = Boolean.TRUE;

	public static final Boolean BRAINTREE_ADVANCED_FRAUD_TOOLS_ENABLED = Boolean.TRUE;

	public static final Boolean IS_SKIP_3D_SECURE_LIABILITY_RESULT = Boolean.TRUE;

	private static final String BRAINTREE_CREDIT_CARD_STATEMENT_NAME = "BRAINTREE_CREDIT_CARD_STATEMENT_NAME";

	public static final String MERCHANT_ACCOUNT_ID_FOR_CURRENT_SITE_AND_CURRENCY = "MERCHANT_ACCOUNT_ID_FOR_CURRENT_SITE_AND_CURRENCY";

	public static final String BRAINTREE_CHANNEL = "Hybris_BT";

	public static final String CARDHOLDER_NAME = "CARDHOLDER_NAME";

	public static final String CARD_NUMBER = "CARD_NUMBER";

	public static final String EXPIRATION_MONTH = "EXPIRATION_MONTH";

	public static final String EXPIRATION_YEAR = "EXPIRATION_YEAR";

	@Mock
	private BrainTreePaymentInfoDao brainTreePaymentInfoDao;

	@Mock
	private BrainTreeCustomerAccountDao brainTreeCustomerAccountDao;

	@Mock
	private ModelService modelService;

	@Mock
	private BrainTreeConfigService brainTreeConfigService;

	@InjectMocks
	private final PaymentInfoService paymentInfoService = new PaymentInfoServiceImpl();

	@Before
	public void init()
	{
		//MockitoAnnotations.initMocks(this);
	}

	@Test
	public void shouldRemovePaymentInfo()
	{
		// given
		final BrainTreePaymentInfoModel brainTreePaymentInfoModel = mock(BrainTreePaymentInfoModel.class);
		when(brainTreePaymentInfoDao.find(CUSTOMER_ID, PAYMENT_METHOD_TOKEN)).thenReturn(brainTreePaymentInfoModel);

		// when
		paymentInfoService.remove(CUSTOMER_ID, PAYMENT_METHOD_TOKEN);

		// then
		verify(modelService).remove(brainTreePaymentInfoModel);
	}

	@Test
	public void shouldAddToCustomer()
	{
		// given
		final BrainTreePaymentInfoModel brainTreePaymentInfoModel = mock(BrainTreePaymentInfoModel.class);
		final CustomerModel customerModel = mock(CustomerModel.class);
		final AddressModel billingAddressModel = mock(AddressModel.class);

		when(brainTreeCustomerAccountDao.findCustomerByBrainTreeCustomerId(CUSTOMER_ID)).thenReturn(customerModel);
		when(customerModel.getUid()).thenReturn(UID);

		when(brainTreePaymentInfoModel.getCustomerId()).thenReturn(CUSTOMER_ID);
		when(brainTreePaymentInfoModel.getBillingAddress()).thenReturn(billingAddressModel);

		when(brainTreeConfigService.get3dSecureConfiguration()).thenReturn(BRAINTREE_3D_SECURE);
		when(brainTreeConfigService.getAdvancedFraudTools()).thenReturn(BRAINTREE_ADVANCED_FRAUD_TOOLS_ENABLED);
		when(brainTreeConfigService.getIsSkip3dSecureLiabilityResult()).thenReturn(IS_SKIP_3D_SECURE_LIABILITY_RESULT);
		when(brainTreeConfigService.getCreditCardStatementName()).thenReturn(BRAINTREE_CREDIT_CARD_STATEMENT_NAME);
		when(brainTreeConfigService.getMerchantAccountIdForCurrentSiteAndCurrency())
				.thenReturn(MERCHANT_ACCOUNT_ID_FOR_CURRENT_SITE_AND_CURRENCY);
		when(brainTreeConfigService.getBrainTreeChannel()).thenReturn(BRAINTREE_CHANNEL);


		// when
		paymentInfoService.addToCustomer(brainTreePaymentInfoModel);

		// then
		verify(brainTreePaymentInfoModel).setUser(customerModel);
		verify(brainTreePaymentInfoModel).setCode(anyString());
		verify(brainTreePaymentInfoModel).setSaved(SAVE_PAYMENT_INFO);
		verify(brainTreePaymentInfoModel).setUsePaymentMethodToken(USE_PAYMENT_METHOD_TOKEN);
		verify(brainTreePaymentInfoModel).setThreeDSecureConfiguration(BRAINTREE_3D_SECURE);
		verify(brainTreePaymentInfoModel).setAdvancedFraudTools(BRAINTREE_ADVANCED_FRAUD_TOOLS_ENABLED);
		verify(brainTreePaymentInfoModel).setIsSkip3dSecureLiabilityResult(IS_SKIP_3D_SECURE_LIABILITY_RESULT);
		verify(brainTreePaymentInfoModel).setCreditCardStatementName(BRAINTREE_CREDIT_CARD_STATEMENT_NAME);
		verify(brainTreePaymentInfoModel).setMerchantAccountIdForCurrentSite(MERCHANT_ACCOUNT_ID_FOR_CURRENT_SITE_AND_CURRENCY);
		verify(brainTreePaymentInfoModel).setBrainTreeChannel(BRAINTREE_CHANNEL);

		verify(modelService).save(billingAddressModel);
		verify(modelService).save(brainTreePaymentInfoModel);
		verify(modelService).refresh(customerModel);
	}

	@Test
	public void shouldUpdatePaymentInfo()
	{
		// given
		final BrainTreePaymentInfoModel paymentInfoModel = mock(BrainTreePaymentInfoModel.class);
		final BrainTreePaymentInfoModel brainTreePaymentInfoModel = mock(BrainTreePaymentInfoModel.class);
		final AddressModel billingAddressModel = mock(AddressModel.class);

		when(paymentInfoModel.getCustomerId()).thenReturn(CUSTOMER_ID);
		when(brainTreePaymentInfoDao.find(CUSTOMER_ID, PAYMENT_METHOD_TOKEN)).thenReturn(brainTreePaymentInfoModel);
		when(paymentInfoModel.getPaymentMethodToken()).thenReturn(PAYMENT_METHOD_TOKEN);
		when(paymentInfoModel.getCardNumber()).thenReturn(CARD_NUMBER);
		when(paymentInfoModel.getCardholderName()).thenReturn(CARDHOLDER_NAME);
		when(paymentInfoModel.getExpirationMonth()).thenReturn(EXPIRATION_MONTH);
		when(paymentInfoModel.getExpirationYear()).thenReturn(EXPIRATION_YEAR);
		when(paymentInfoModel.getBillingAddress()).thenReturn(billingAddressModel);

		// when
		paymentInfoService.update(PAYMENT_METHOD_TOKEN, paymentInfoModel);

		// then
		verify(brainTreePaymentInfoModel).setPaymentMethodToken(PAYMENT_METHOD_TOKEN);
		verify(brainTreePaymentInfoModel).setCardNumber(CARD_NUMBER);
		verify(brainTreePaymentInfoModel).setCardholderName(CARDHOLDER_NAME);
		verify(brainTreePaymentInfoModel).setExpirationMonth(EXPIRATION_MONTH);
		verify(brainTreePaymentInfoModel).setExpirationYear(EXPIRATION_YEAR);
		verify(billingAddressModel).setOwner(brainTreePaymentInfoModel);
		verify(brainTreePaymentInfoModel).setBillingAddress(billingAddressModel);

		verify(modelService).save(billingAddressModel);
		verify(modelService).save(brainTreePaymentInfoModel);
		verify(modelService).refresh(brainTreePaymentInfoModel);
	}

}
