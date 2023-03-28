/**
 *
 */
package com.braintree.method.impl;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.anyObject;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commerceservices.strategies.CheckoutCustomerStrategy;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.order.CartService;
import de.hybris.platform.payment.AdapterException;
import de.hybris.platform.payment.commands.AuthorizationCommand;
import de.hybris.platform.payment.commands.CreateSubscriptionCommand;
import de.hybris.platform.payment.commands.factory.CommandFactory;
import de.hybris.platform.payment.commands.factory.CommandFactoryRegistry;
import de.hybris.platform.payment.commands.factory.CommandNotSupportedException;
import de.hybris.platform.payment.commands.request.AuthorizationRequest;
import de.hybris.platform.payment.commands.request.CreateSubscriptionRequest;
import de.hybris.platform.payment.commands.request.VoidRequest;
import de.hybris.platform.payment.commands.result.AuthorizationResult;
import de.hybris.platform.payment.commands.result.SubscriptionResult;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import com.braintree.command.request.BrainTreeAuthorizationRequest;
import com.braintree.command.request.BrainTreeCloneTransactionRequest;
import com.braintree.command.request.BrainTreeCustomerRequest;
import com.braintree.command.request.BrainTreeRefundTransactionRequest;
import com.braintree.command.request.BrainTreeSaleTransactionRequest;
import com.braintree.command.result.BrainTreeCloneTransactionResult;
import com.braintree.command.result.BrainTreeFindCustomerResult;
import com.braintree.command.result.BrainTreeRefundTransactionResult;
import com.braintree.command.result.BrainTreeSaleTransactionResult;
import com.braintree.command.result.BrainTreeVoidResult;
import com.braintree.commands.BrainTreeCloneCommand;
import com.braintree.commands.BrainTreeFindCustomerCommand;
import com.braintree.commands.BrainTreeRefundCommand;
import com.braintree.commands.BrainTreeSaleCommand;
import com.braintree.commands.BrainTreeVoidCommand;
import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.constants.BraintreeConstants;
import com.braintree.customer.service.BrainTreeCustomerAccountService;
import com.braintree.method.BrainTreePaymentService;
import com.braintree.paypal.converters.impl.BillingAddressConverter;
import com.braintreegateway.exceptions.NotFoundException;


@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class BrainTreePaymentServiceImplTest
{
	public static final String CUSTOMER_ID = "CUSTOMER_ID";
	public static final String NOT_FOUND_EXCEPTION_MESSAGE = "Problem occurred in Payment Provider configuration. Please contact with store support.";
	public static final String EXCEPT_NOT_FOUND_EXCEPTION_MESSAGE = "Any exception except NotFoundException";
	public static final String MERCHANT_TRANSACTION_CODE = "MERCHANT_TRANSACTION_CODE";
	public static final String ANY_EXCEPTION_MESSAGE = "Any exception";
	public static final String COMMAND_NOT_SUPPORTED_EXCEPTION = "CommandNotSupportedException";


	@Rule
	public ExpectedException exception = ExpectedException.none();

	@Mock
	private CommandFactoryRegistry commandFactoryRegistry;

	@Mock
	private ModelService modelService;
	@Mock
	private UserService userService;
	@Mock
	private CartService cartService;

	@Mock
	private BillingAddressConverter billingAddressConverter;
	@Mock
	private BrainTreeCustomerAccountService brainTreeCustomerAccountService;
	@Mock
	private CheckoutCustomerStrategy checkoutCustomerStrategy;
	@Mock
	private BrainTreeConfigService brainTreeConfigService;

	@InjectMocks
	private final BrainTreePaymentService unit = new BrainTreePaymentServiceImpl();

	@Before
	public void init()
	{
		//MockitoAnnotations.initMocks(this);
	}

	@Test
	public void shouldAuthorizeIfCustomerExists() throws CommandNotSupportedException
	{
		// given
		final CustomerModel customerModel = mock(CustomerModel.class);
		final AuthorizationCommand authorizationCommand = mock(AuthorizationCommand.class);
		final AuthorizationResult expectedAuthorizationResult = mock(AuthorizationResult.class);
		final AuthorizationRequest authorizationRequest = mock(AuthorizationRequest.class);
		final BrainTreeFindCustomerResult brainTreeFindCustomerResult = mock(BrainTreeFindCustomerResult.class);
		final BrainTreeFindCustomerCommand command = mock(BrainTreeFindCustomerCommand.class);
		final CommandFactory commandFactory = mock(CommandFactory.class);

		when(checkoutCustomerStrategy.getCurrentUserForCheckout()).thenReturn(customerModel);
		when(customerModel.getBraintreeCustomerId()).thenReturn(CUSTOMER_ID);
		when(commandFactoryRegistry.getFactory(BraintreeConstants.BRAINTREE_PROVIDER_NAME)).thenReturn(commandFactory);
		when(commandFactory.createCommand(BrainTreeFindCustomerCommand.class)).thenReturn(command);
		when(command.perform((BrainTreeCustomerRequest) anyObject())).thenReturn(brainTreeFindCustomerResult);
		when(Boolean.valueOf(brainTreeFindCustomerResult.isCustomerExist())).thenReturn(Boolean.TRUE);

		when(commandFactory.createCommand(AuthorizationCommand.class)).thenReturn(authorizationCommand);
		when(authorizationCommand.perform(authorizationRequest)).thenReturn(expectedAuthorizationResult);

		// when
		final AuthorizationResult actualAuthorizationResult = unit.authorize(authorizationRequest);

		// then
		assertEquals(expectedAuthorizationResult, actualAuthorizationResult);
	}

	@Test
	public void shouldThrowNotFoundExceptionOnAuthorize() throws CommandNotSupportedException
	{
		// given
		exception.expect(AdapterException.class);
		exception.expectMessage(NOT_FOUND_EXCEPTION_MESSAGE);
		final CustomerModel customerModel = mock(CustomerModel.class);
		final AuthorizationCommand authorizationCommand = mock(AuthorizationCommand.class);
		final AuthorizationRequest authorizationRequest = mock(AuthorizationRequest.class);
		final BrainTreeFindCustomerResult brainTreeFindCustomerResult = mock(BrainTreeFindCustomerResult.class);
		final BrainTreeFindCustomerCommand command = mock(BrainTreeFindCustomerCommand.class);
		final CommandFactory commandFactory = mock(CommandFactory.class);
		when(checkoutCustomerStrategy.getCurrentUserForCheckout()).thenReturn(customerModel);
		when(customerModel.getBraintreeCustomerId()).thenReturn(CUSTOMER_ID);
		when(commandFactoryRegistry.getFactory(BraintreeConstants.BRAINTREE_PROVIDER_NAME)).thenReturn(commandFactory);
		when(commandFactory.createCommand(BrainTreeFindCustomerCommand.class)).thenReturn(command);
		when(command.perform((BrainTreeCustomerRequest) anyObject())).thenReturn(brainTreeFindCustomerResult);
		when(Boolean.valueOf(brainTreeFindCustomerResult.isCustomerExist())).thenReturn(Boolean.TRUE);
		when(commandFactory.createCommand(AuthorizationCommand.class)).thenReturn(authorizationCommand);

		when(authorizationCommand.perform(authorizationRequest)).thenThrow(new NotFoundException());

		// when
		unit.authorize(authorizationRequest);
	}

	@Test
	public void shouldThrowExceptionOnAuthorize() throws CommandNotSupportedException
	{
		// given
		exception.expect(Exception.class);
		final CustomerModel customerModel = mock(CustomerModel.class);
		final AuthorizationRequest authorizationRequest = mock(AuthorizationRequest.class);
		final BrainTreeCustomerRequest brainTreeCustomerRequest = mock(BrainTreeCustomerRequest.class);

		final BrainTreeFindCustomerCommand command = mock(BrainTreeFindCustomerCommand.class);
		final CommandFactory commandFactory = mock(CommandFactory.class);
		when(checkoutCustomerStrategy.getCurrentUserForCheckout()).thenReturn(customerModel);
		when(customerModel.getBraintreeCustomerId()).thenReturn(CUSTOMER_ID);
		when(commandFactoryRegistry.getFactory(BraintreeConstants.BRAINTREE_PROVIDER_NAME)).thenReturn(commandFactory);
		when(commandFactory.createCommand(BrainTreeFindCustomerCommand.class)).thenReturn(command);

		when(command.perform(brainTreeCustomerRequest)).thenThrow(new RuntimeException(EXCEPT_NOT_FOUND_EXCEPTION_MESSAGE));

		// then
		unit.authorize(authorizationRequest);
	}

	@Test
	public void shouldSaveBTCustomer() throws CommandNotSupportedException
	{
		// given
		final CustomerModel customerModel = mock(CustomerModel.class);
		final AuthorizationCommand authorizationCommand = mock(AuthorizationCommand.class);
		final AuthorizationResult expectedAuthorizationResult = mock(AuthorizationResult.class);
		final AuthorizationRequest authorizationRequest = mock(BrainTreeAuthorizationRequest.class);
		final BrainTreeFindCustomerResult brainTreeFindCustomerResult = mock(BrainTreeFindCustomerResult.class);
		final BrainTreeFindCustomerCommand brainTreeFindCustomerCommand = mock(BrainTreeFindCustomerCommand.class);
		final CommandFactory commandFactory = mock(CommandFactory.class);
		final AddressModel addressModel = mock(AddressModel.class);
		final CartModel cartModel = mock(CartModel.class);
		final SubscriptionResult subscriptionResult = mock(SubscriptionResult.class);
		final CreateSubscriptionCommand createSubscriptionCommand = mock(CreateSubscriptionCommand.class);

		when(checkoutCustomerStrategy.getCurrentUserForCheckout()).thenReturn(customerModel);
		when(customerModel.getBraintreeCustomerId()).thenReturn(CUSTOMER_ID);
		when(commandFactoryRegistry.getFactory(BraintreeConstants.BRAINTREE_PROVIDER_NAME)).thenReturn(commandFactory);
		when(commandFactory.createCommand(BrainTreeFindCustomerCommand.class)).thenReturn(brainTreeFindCustomerCommand);
		when(brainTreeFindCustomerCommand.perform((BrainTreeCustomerRequest) anyObject())).thenReturn(brainTreeFindCustomerResult);
		when(Boolean.valueOf(brainTreeFindCustomerResult.isCustomerExist())).thenReturn(Boolean.FALSE);
		when(cartService.getSessionCart()).thenReturn(cartModel);
		when(cartModel.getDeliveryAddress()).thenReturn(addressModel);
		when(commandFactory.createCommand(CreateSubscriptionCommand.class)).thenReturn(createSubscriptionCommand);
		when(createSubscriptionCommand.perform((CreateSubscriptionRequest) anyObject())).thenReturn(subscriptionResult);
		when(subscriptionResult.getMerchantTransactionCode()).thenReturn(MERCHANT_TRANSACTION_CODE);

		when(commandFactory.createCommand(AuthorizationCommand.class)).thenReturn(authorizationCommand);
		when(authorizationCommand.perform(authorizationRequest)).thenReturn(expectedAuthorizationResult);

		// when
		final AuthorizationResult actualAuthorizationResult = unit.authorize(authorizationRequest);

		// then
		assertEquals(expectedAuthorizationResult, actualAuthorizationResult);
		verify(customerModel).setBraintreeCustomerId(MERCHANT_TRANSACTION_CODE);
		verify(modelService).save(customerModel);
	}

	@Test
	public void shouldVoidTransaction() throws CommandNotSupportedException
	{
		// given
		final CommandFactory commandFactory = mock(CommandFactory.class);
		final BrainTreeVoidCommand brainTreeVoidCommand = mock(BrainTreeVoidCommand.class);
		final BrainTreeVoidResult expectedBrainTreeVoidResult = mock(BrainTreeVoidResult.class);
		final VoidRequest voidRequest = mock(VoidRequest.class);
		when(commandFactoryRegistry.getFactory(BraintreeConstants.BRAINTREE_PROVIDER_NAME)).thenReturn(commandFactory);
		when(commandFactory.createCommand(BrainTreeVoidCommand.class)).thenReturn(brainTreeVoidCommand);
		when(brainTreeVoidCommand.perform(voidRequest)).thenReturn(expectedBrainTreeVoidResult);

		// when
		final BrainTreeVoidResult actualBrainTreeVoidResult = unit.voidTransaction(voidRequest);

		// then
		assertEquals(expectedBrainTreeVoidResult, actualBrainTreeVoidResult);
	}

	@Test
	public void shouldThrowExceptionOnVoidTransaction() throws CommandNotSupportedException
	{
		// given
		exception.expect(AdapterException.class);
		exception.expectMessage(ANY_EXCEPTION_MESSAGE);
		final CommandFactory commandFactory = mock(CommandFactory.class);
		final BrainTreeVoidCommand brainTreeVoidCommand = mock(BrainTreeVoidCommand.class);
		final VoidRequest voidRequest = mock(VoidRequest.class);
		when(commandFactoryRegistry.getFactory(BraintreeConstants.BRAINTREE_PROVIDER_NAME)).thenReturn(commandFactory);
		when(commandFactory.createCommand(BrainTreeVoidCommand.class)).thenReturn(brainTreeVoidCommand);
		when(brainTreeVoidCommand.perform(voidRequest)).thenThrow(new RuntimeException(ANY_EXCEPTION_MESSAGE));

		// when
		unit.voidTransaction(voidRequest);
	}

	@Test
	public void shouldCloneTransaction() throws CommandNotSupportedException
	{
		// given
		final CommandFactory commandFactory = mock(CommandFactory.class);
		final BrainTreeCloneCommand brainTreeCloneCommand = mock(BrainTreeCloneCommand.class);
		final BrainTreeCloneTransactionResult expectedBrainTreeCloneTransactionResult = mock(BrainTreeCloneTransactionResult.class);
		final BrainTreeCloneTransactionRequest cloneRequest = mock(BrainTreeCloneTransactionRequest.class);
		when(commandFactoryRegistry.getFactory(BraintreeConstants.BRAINTREE_PROVIDER_NAME)).thenReturn(commandFactory);
		when(commandFactory.createCommand(BrainTreeCloneCommand.class)).thenReturn(brainTreeCloneCommand);
		when(brainTreeCloneCommand.perform(cloneRequest)).thenReturn(expectedBrainTreeCloneTransactionResult);

		// when
		final BrainTreeCloneTransactionResult actualBrainTreeCloneTransactionResult = unit.cloneTransaction(cloneRequest);

		// then
		assertEquals(expectedBrainTreeCloneTransactionResult, actualBrainTreeCloneTransactionResult);
	}

	@Test
	public void shouldThrowExceptionOnCloneTransaction() throws CommandNotSupportedException
	{
		// given
		exception.expect(AdapterException.class);
		exception.expectMessage(ANY_EXCEPTION_MESSAGE);
		final CommandFactory commandFactory = mock(CommandFactory.class);
		final BrainTreeCloneCommand brainTreeCloneCommand = mock(BrainTreeCloneCommand.class);
		final BrainTreeCloneTransactionRequest cloneRequest = mock(BrainTreeCloneTransactionRequest.class);
		when(commandFactoryRegistry.getFactory(BraintreeConstants.BRAINTREE_PROVIDER_NAME)).thenReturn(commandFactory);
		when(commandFactory.createCommand(BrainTreeCloneCommand.class)).thenReturn(brainTreeCloneCommand);
		when(brainTreeCloneCommand.perform(cloneRequest)).thenThrow(new RuntimeException(ANY_EXCEPTION_MESSAGE));

		// when
		unit.cloneTransaction(cloneRequest);
	}

	@Test
	public void shouldRefundTransaction() throws CommandNotSupportedException
	{
		// given
		final CommandFactory commandFactory = mock(CommandFactory.class);
		final BrainTreeRefundCommand brainTreeRefundCommand = mock(BrainTreeRefundCommand.class);
		final BrainTreeRefundTransactionResult expectedBrainTreeRefundTransactionResult = mock(
				BrainTreeRefundTransactionResult.class);
		final BrainTreeRefundTransactionRequest refundRequest = mock(BrainTreeRefundTransactionRequest.class);
		when(commandFactoryRegistry.getFactory(BraintreeConstants.BRAINTREE_PROVIDER_NAME)).thenReturn(commandFactory);
		when(commandFactory.createCommand(BrainTreeRefundCommand.class)).thenReturn(brainTreeRefundCommand);
		when(brainTreeRefundCommand.perform(refundRequest)).thenReturn(expectedBrainTreeRefundTransactionResult);

		// when
		final BrainTreeRefundTransactionResult actualBrainTreeRefundTransactionResult = unit.refundTransaction(refundRequest);

		// then
		assertEquals(expectedBrainTreeRefundTransactionResult, actualBrainTreeRefundTransactionResult);
	}

	@Test
	public void shouldThrowExceptionOnRefundTransaction() throws CommandNotSupportedException
	{
		// given
		exception.expect(AdapterException.class);
		exception.expectMessage(COMMAND_NOT_SUPPORTED_EXCEPTION);
		final CommandFactory commandFactory = mock(CommandFactory.class);
		final BrainTreeRefundTransactionRequest refundRequest = mock(BrainTreeRefundTransactionRequest.class);
		when(commandFactoryRegistry.getFactory(BraintreeConstants.BRAINTREE_PROVIDER_NAME)).thenReturn(commandFactory);
		when(commandFactory.createCommand(BrainTreeRefundCommand.class))
				.thenThrow(new CommandNotSupportedException(COMMAND_NOT_SUPPORTED_EXCEPTION));

		// when
		unit.refundTransaction(refundRequest);
	}

	@Test
	public void shouldSaleTransaction() throws CommandNotSupportedException
	{
		// given
		final CommandFactory commandFactory = mock(CommandFactory.class);
		final BrainTreeSaleCommand brainTreeSaleCommand = mock(BrainTreeSaleCommand.class);
		final BrainTreeSaleTransactionResult expectedBrainTreeSaleTransactionResult = mock(BrainTreeSaleTransactionResult.class);
		final BrainTreeSaleTransactionRequest saleRequest = mock(BrainTreeSaleTransactionRequest.class);
		when(commandFactoryRegistry.getFactory(BraintreeConstants.BRAINTREE_PROVIDER_NAME)).thenReturn(commandFactory);
		when(commandFactory.createCommand(BrainTreeSaleCommand.class)).thenReturn(brainTreeSaleCommand);
		when(brainTreeSaleCommand.perform(saleRequest)).thenReturn(expectedBrainTreeSaleTransactionResult);

		// when
		final BrainTreeSaleTransactionResult actualBrainTreeSaleTransactionResult = unit.saleTransaction(saleRequest);

		// then
		assertEquals(expectedBrainTreeSaleTransactionResult, actualBrainTreeSaleTransactionResult);
	}

	@Test
	public void shouldThrowExceptionOnSaleTransaction() throws CommandNotSupportedException
	{
		// given
		exception.expect(AdapterException.class);
		exception.expectMessage(COMMAND_NOT_SUPPORTED_EXCEPTION);
		final CommandFactory commandFactory = mock(CommandFactory.class);
		final BrainTreeSaleTransactionRequest saleRequest = mock(BrainTreeSaleTransactionRequest.class);
		when(commandFactoryRegistry.getFactory(BraintreeConstants.BRAINTREE_PROVIDER_NAME)).thenReturn(commandFactory);
		when(commandFactory.createCommand(BrainTreeSaleCommand.class))
				.thenThrow(new CommandNotSupportedException(COMMAND_NOT_SUPPORTED_EXCEPTION));

		// when
		unit.saleTransaction(saleRequest);
	}

}
