/**
 *
 */
package com.braintree.commands.impl;

import com.braintree.constants.BraintreeConstants;
import com.braintreegateway.BraintreeGateway;
import com.braintreegateway.Customer;
import com.braintreegateway.CustomerRequest;
import com.braintreegateway.Result;
import com.braintreegateway.ValidationError;
import de.hybris.platform.payment.AdapterException;
import de.hybris.platform.payment.commands.CreateSubscriptionCommand;
import de.hybris.platform.payment.commands.request.CreateSubscriptionRequest;
import de.hybris.platform.payment.commands.result.SubscriptionResult;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.dto.TransactionStatusDetails;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;

import java.util.List;


public class CreateSubscriptionCommandImpl extends AbstractCommand implements CreateSubscriptionCommand
{

	private final static Logger LOG = Logger.getLogger(CreateSubscriptionCommandImpl.class);

	@Override
	public SubscriptionResult perform(final CreateSubscriptionRequest request)
	{
		try
		{
			final CustomerRequest braintreeRequest = translateRequest(request);

			final BraintreeGateway gateway = getBraintreeGateway();

			final Result<Customer> braintreeReply = gateway.customer().create(braintreeRequest);

			final SubscriptionResult result = translateResponse(braintreeReply);

			return result;
		}
		catch (final Exception exception)
		{
			LOG.error("[BT Create Customer] Error during customer creation!");
			throw new AdapterException(exception.getMessage(), exception);
		}
	}

	private CustomerRequest translateRequest(final CreateSubscriptionRequest paramRequestType)
	{
		CustomerRequest request = null;
		final String firstName = paramRequestType.getBillingInfo().getFirstName();
		final String lastName = paramRequestType.getBillingInfo().getLastName();
		final String email = paramRequestType.getBillingInfo().getEmail();
		final String payee = StringUtils.EMPTY;
		if (StringUtils.isNotEmpty(payee) && BraintreeConstants.PAYPAL_INTENT_ORDER
				.equalsIgnoreCase(getBrainTreeConfigService().getIntent()))
		{
			request = new CustomerRequest().firstName(firstName).lastName(lastName).email(email).options().paypal().payeeEmail(payee)
					.done().done();
		}
		else
		{
			request = new CustomerRequest().firstName(firstName).lastName(lastName).email(email);
		}
		getLoggingHandler().handleCreateSubscriptionRequest(paramRequestType, payee);
		return request;
	}

	private SubscriptionResult translateResponse(final Result<Customer> braintreeReply)
	{
		List<ValidationError> errors = null;
		final SubscriptionResult result = new SubscriptionResult();
		result.setTransactionStatus(TransactionStatus.REJECTED);

		Customer customer = null;
		if (braintreeReply != null)
		{
			customer = braintreeReply.getTarget();
			if (braintreeReply.isSuccess())
			{
				if (customer != null)
				{
					result.setMerchantTransactionCode(customer.getId());
					result.setRequestId(customer.getId());
					result.setTransactionStatus(TransactionStatus.ACCEPTED);
					result.setTransactionStatusDetails(TransactionStatusDetails.SUCCESFULL);
				}
			}
			else if (braintreeReply.getErrors() != null && braintreeReply.getErrors().getAllDeepValidationErrors() != null
					&& braintreeReply.getErrors().getAllDeepValidationErrors().size() > 0)
			{
				errors = braintreeReply.getErrors().getAllDeepValidationErrors();


				final String errorMessage = getErrorTranslator()
						.getMessage(braintreeReply.getErrors().getAllDeepValidationErrors().get(0));
				throw new IllegalArgumentException(errorMessage);
			}
			getLoggingHandler().handleResult("[CREATE CUSTOMER]", customer);
			getLoggingHandler().handleErrors(errors);

		}
		return result;
	}

}
