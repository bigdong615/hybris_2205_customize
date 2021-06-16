/**
 *
 */
package com.braintree.transaction.service.impl;

import static com.braintree.constants.BraintreeConstants.BRAINTREE_PROVIDER_NAME;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commerceservices.strategies.CheckoutCustomerStrategy;
import de.hybris.platform.core.model.c2l.CurrencyModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.order.CartService;
import de.hybris.platform.payment.PaymentService;
import de.hybris.platform.payment.dto.BillingInfo;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.dto.TransactionStatusDetails;
import de.hybris.platform.payment.enums.PaymentTransactionType;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;

import java.math.BigDecimal;
import java.util.Currency;
import java.util.Date;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;

import com.braintree.command.request.BrainTreeAuthorizationRequest;
import com.braintree.command.request.BrainTreeFindMerchantAccountRequest;
import com.braintree.command.result.BrainTreeAuthorizationResult;
import com.braintree.command.result.BrainTreeFindMerchantAccountResult;
import com.braintree.command.result.BrainTreeRefundTransactionResult;
import com.braintree.command.result.BrainTreeVoidResult;
import com.braintree.method.BrainTreePaymentService;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.paypal.converters.impl.BillingAddressConverter;
import com.google.common.collect.Lists;


@UnitTest
public class BrainTreeTransactionServiceImplTest
{
	private static final String BT_CUSTOMER_ID = "BT_CUSTOMER_ID";

	private static final String NONCE = "NONCE";
	private static final String DEVICE_DATA = "DEVICE_DATA";
	private static final String PAYMENT_PROVIDER = "PAYMENT_PROVIDER";
	private static final Boolean USE_PAYMENT_METHOD_TOKEN = Boolean.TRUE;
	private static final String PAYMNET_METHOD_TOKEN = "PAYMNET_METHOD_TOKEN";
	private static final Boolean IS_PAYMENT_SAVED = Boolean.TRUE;
	private static final Boolean LIABILITY_SHIFTED = Boolean.TRUE;
	private static final Boolean THREE_D_SECURE_CONFIGURATION = Boolean.TRUE;
	private static final Boolean ADVANCED_FRAUD_TOOLS = Boolean.TRUE;
	private static final Boolean IS_SKIP_3D_SECURE_LIABILITY_RESULT = Boolean.TRUE;
	private static final String CREDIT_CARD_STATEMENT_NAME = "CREDIT_CARD_STATEMENT_NAME";
	private static final String MERCHANT_ACCOUNT_ID_FOR_CURRENT_SITE = "MERCHANT_ACCOUNT_ID_FOR_CURRENT_SITE";
	private static final String BRAINTREE_CHANNEL = "BRAINTREE_CHANNEL";
	private static final String PAYMENT_CODE = "PAYMENT_CODE";

	private static final String CURRENCY_CODE_USD = "USD";
	private static final Double TOTAL_PRICE = Double.valueOf(100);
	private static final String REQUEST_ID = "REQUEST_ID";

	private static final TransactionStatusDetails TRANSACTION_STATUS_DETAILS = TransactionStatusDetails.SUCCESFULL;
	private static final TransactionStatus TRANSACTION_STATUS = TransactionStatus.ACCEPTED;

	private static final String NEW_ENTRY_CODE = "NEW_ENTRY_CODE";
	private static final Currency CURRENCY = Currency.getInstance(CURRENCY_CODE_USD);


	@Mock
	private CartService cartService;

	@Mock
	private ModelService modelService;

	@Mock
	private UserService userService;

	@Mock
	private BrainTreePaymentService brainTreePaymentService;

	@Mock
	private PaymentService paymentService;

	@Mock
	private BillingAddressConverter billingAddressConverter;

	@Mock
	private CheckoutCustomerStrategy checkoutCustomerStrategy;

	@Mock
	private CommonI18NService commonI18NService;

	@Spy
	@InjectMocks
	private final BrainTreeTransactionServiceImpl brainTreeTransactionServiceImpl = new BrainTreeTransactionServiceImpl();

	@Before
	public void init()
	{
		MockitoAnnotations.initMocks(this);
		final CurrencyModel currencyModel = mock(CurrencyModel.class);
		when(commonI18NService.getCurrentCurrency()).thenReturn(currencyModel);
	}

	@Test
	public void shouldCreateAuthTransactionSuccess()
	{
		final CartModel cart = mock(CartModel.class);
		final BrainTreePaymentInfoModel paymentInfo = mock(BrainTreePaymentInfoModel.class);
		final CustomerModel customerModel = mock(CustomerModel.class);
		when(cart.getUser()).thenReturn(customerModel);
		when(customerModel.getBraintreeCustomerId()).thenReturn(BT_CUSTOMER_ID);
		when(cartService.getSessionCart()).thenReturn(cart);
		when(cart.getPaymentInfo()).thenReturn(paymentInfo);
		final CurrencyModel currencyModel = mock(CurrencyModel.class);
		when(currencyModel.getIsocode()).thenReturn(CURRENCY_CODE_USD);
		when(cart.getCurrency()).thenReturn(currencyModel);
		when(paymentInfo.getNonce()).thenReturn(NONCE);
		when(paymentInfo.getDeviceData()).thenReturn(DEVICE_DATA);
		when(paymentInfo.getPaymentProvider()).thenReturn(PAYMENT_PROVIDER);
		when(paymentInfo.getUsePaymentMethodToken()).thenReturn(USE_PAYMENT_METHOD_TOKEN);
		when(paymentInfo.getPaymentMethodToken()).thenReturn(PAYMNET_METHOD_TOKEN);
		when(Boolean.valueOf(paymentInfo.isSaved())).thenReturn(IS_PAYMENT_SAVED);
		when(paymentInfo.getLiabilityShifted()).thenReturn(LIABILITY_SHIFTED);
		when(paymentInfo.getThreeDSecureConfiguration()).thenReturn(THREE_D_SECURE_CONFIGURATION);
		when(paymentInfo.getAdvancedFraudTools()).thenReturn(ADVANCED_FRAUD_TOOLS);
		when(paymentInfo.getIsSkip3dSecureLiabilityResult()).thenReturn(IS_SKIP_3D_SECURE_LIABILITY_RESULT);
		when(paymentInfo.getCreditCardStatementName()).thenReturn(CREDIT_CARD_STATEMENT_NAME);
		when(paymentInfo.getMerchantAccountIdForCurrentSite()).thenReturn(MERCHANT_ACCOUNT_ID_FOR_CURRENT_SITE);
		when(paymentInfo.getBrainTreeChannel()).thenReturn(BRAINTREE_CHANNEL);

		final AddressModel shippingAddress = mock(AddressModel.class);
		final BillingInfo shippingInfo = mock(BillingInfo.class);
		when(cart.getDeliveryAddress()).thenReturn(shippingAddress);
		when(billingAddressConverter.convert(shippingAddress)).thenReturn(shippingInfo);
		when(cart.getTotalPrice()).thenReturn(TOTAL_PRICE);

		final BrainTreeFindMerchantAccountResult brainTreeFindMerchantAccountResult = mock(
				BrainTreeFindMerchantAccountResult.class);
		when(Boolean.valueOf(brainTreeFindMerchantAccountResult.isMerchantAccountExist())).thenReturn(Boolean.valueOf(true));
		when(brainTreePaymentService.findMerchantAccount(any(BrainTreeFindMerchantAccountRequest.class)))
				.thenReturn(brainTreeFindMerchantAccountResult);

		when(paymentInfo.getCode()).thenReturn(PAYMENT_CODE);

		final PaymentTransactionEntryModel paymentTransactionEntry = mock(PaymentTransactionEntryModel.class);
		when(paymentTransactionEntry.getRequestId()).thenReturn(REQUEST_ID);
		when(modelService.create(PaymentTransactionEntryModel.class)).thenReturn(paymentTransactionEntry);
		final BrainTreeAuthorizationResult authorizationResult = mock(BrainTreeAuthorizationResult.class);
		when(authorizationResult.getTransactionStatus()).thenReturn(TRANSACTION_STATUS);
		when(authorizationResult.getTransactionStatusDetails()).thenReturn(TRANSACTION_STATUS_DETAILS);
		when(authorizationResult.getRequestId()).thenReturn(REQUEST_ID);
		when(Boolean.valueOf(authorizationResult.isSuccess())).thenReturn(Boolean.TRUE);
		when(authorizationResult.getCurrency()).thenReturn(CURRENCY);

		when(brainTreePaymentService.authorize(any(BrainTreeAuthorizationRequest.class), any(CustomerModel.class)))
				.thenReturn(authorizationResult);

		final PaymentTransactionModel paymentTransaction = mock(PaymentTransactionModel.class);
		when(modelService.create(PaymentTransactionModel.class)).thenReturn(paymentTransaction);
		final List<PaymentTransactionEntryModel> paymentTransactionEntries = Lists.newArrayList();
		paymentTransactionEntries.add(paymentTransactionEntry);
		when(commonI18NService.getCurrency(CURRENCY_CODE_USD)).thenReturn(currencyModel);

		final boolean createResult = brainTreeTransactionServiceImpl.createAuthorizationTransaction();

		verify(paymentTransactionEntry).setType(PaymentTransactionType.AUTHORIZATION);
		verify(paymentTransactionEntry).setTransactionStatus(TRANSACTION_STATUS.toString());
		verify(paymentTransactionEntry).setTransactionStatusDetails(TRANSACTION_STATUS_DETAILS.toString());
		verify(paymentTransactionEntry).setRequestId(REQUEST_ID);
		verify(paymentTransactionEntry).setCurrency(currencyModel);
		verify(paymentTransactionEntry).setRequestToken(NONCE);
		verify(paymentTransactionEntry).setCode(anyString());
		verify(paymentTransactionEntry).setAmount(Mockito.<BigDecimal> anyObject());
		verify(paymentTransactionEntry).setTime(Mockito.<Date> anyObject());

		verify(paymentTransaction).setRequestId(REQUEST_ID);
		verify(paymentTransaction).setEntries(paymentTransactionEntries);
		verify(paymentTransaction).setRequestToken(NONCE);
		verify(paymentTransaction).setPaymentProvider(BRAINTREE_PROVIDER_NAME);
		final List<PaymentTransactionModel> paymentTransactions = Lists.newArrayList();
		paymentTransactions.add(paymentTransaction);
		verify(cart).setPaymentTransactions(paymentTransactions);

		verify(modelService).saveAll(cart);

		assertTrue(createResult);
	}

	@Test
	public void shouldCreateCancelTransaction()
	{
		final PaymentTransactionEntryModel paymentTransactionEntryModelExpected = mock(PaymentTransactionEntryModel.class);
		final BrainTreeVoidResult brainTreeVoidResult = mock(BrainTreeVoidResult.class);
		final PaymentTransactionModel paymentTransactionModel = mock(PaymentTransactionModel.class);
		final CurrencyModel currencyModel = mock(CurrencyModel.class);
		when(paymentTransactionModel.getCurrency()).thenReturn(currencyModel);
		when(paymentService.getNewPaymentTransactionEntryCode(paymentTransactionModel, PaymentTransactionType.CANCEL))
				.thenReturn(NEW_ENTRY_CODE);
		when(modelService.create(PaymentTransactionEntryModel.class)).thenReturn(paymentTransactionEntryModelExpected);
		when(brainTreeVoidResult.getRequestId()).thenReturn(REQUEST_ID);
		when(brainTreeVoidResult.getTransactionStatus()).thenReturn(TRANSACTION_STATUS);
		final BigDecimal amount = BigDecimal.ONE;
		when(brainTreeVoidResult.getAmount()).thenReturn(amount);
		when(brainTreeVoidResult.getTransactionStatusDetails()).thenReturn(TRANSACTION_STATUS_DETAILS);

		final PaymentTransactionEntryModel paymentTransactionEntryModelActual = brainTreeTransactionServiceImpl
				.createCancelTransaction(paymentTransactionModel, brainTreeVoidResult);

		assertEquals(paymentTransactionEntryModelExpected, paymentTransactionEntryModelActual);

		verify(modelService).saveAll(paymentTransactionEntryModelExpected, paymentTransactionModel);
	}

	@Test
	public void shouldCreateRefundTransaction()
	{
		final PaymentTransactionEntryModel paymentTransactionEntryModelExpected = mock(PaymentTransactionEntryModel.class);
		final BrainTreeRefundTransactionResult brainTreeVoidResult = mock(BrainTreeRefundTransactionResult.class);
		final PaymentTransactionModel paymentTransactionModel = mock(PaymentTransactionModel.class);
		final CurrencyModel currencyModel = mock(CurrencyModel.class);
		when(paymentTransactionModel.getCurrency()).thenReturn(currencyModel);
		when(paymentService.getNewPaymentTransactionEntryCode(paymentTransactionModel, PaymentTransactionType.REFUND_STANDALONE))
				.thenReturn(NEW_ENTRY_CODE);
		when(modelService.create(PaymentTransactionEntryModel.class)).thenReturn(paymentTransactionEntryModelExpected);
		when(brainTreeVoidResult.getRequestId()).thenReturn(REQUEST_ID);
		when(brainTreeVoidResult.getTransactionStatus()).thenReturn(TRANSACTION_STATUS);
		final BigDecimal amount = BigDecimal.ONE;
		when(brainTreeVoidResult.getAmount()).thenReturn(amount);
		when(brainTreeVoidResult.getTransactionStatusDetails()).thenReturn(TRANSACTION_STATUS_DETAILS);

		final PaymentTransactionEntryModel paymentTransactionEntryModelActual = brainTreeTransactionServiceImpl
				.createRefundTransaction(paymentTransactionModel, brainTreeVoidResult);

		assertEquals(paymentTransactionEntryModelExpected, paymentTransactionEntryModelActual);
		verify(modelService).saveAll(paymentTransactionEntryModelExpected, paymentTransactionModel);
	}
}
