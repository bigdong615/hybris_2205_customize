/**
 *
 */
package com.braintree.commands.impl;

import com.braintree.command.request.BrainTreeCustomerRequest;
import com.braintree.command.result.BrainTreeFindCustomerResult;
import com.braintree.command.result.BrainTreeFindCustomersResult;
import com.braintree.commands.BrainTreeFindCustomerCommand;
import com.braintreegateway.Customer;
import com.braintreegateway.CustomerSearchRequest;
import com.braintreegateway.ResourceCollection;
import com.braintreegateway.exceptions.NotFoundException;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;


public class FindCustomerCommandImpl extends AbstractCommand<BrainTreeCustomerRequest, BrainTreeFindCustomerResult> implements
		BrainTreeFindCustomerCommand
{
	private final static Logger LOG = Logger.getLogger(FindCustomerCommandImpl.class);


	@Override
	public BrainTreeFindCustomerResult perform(final BrainTreeCustomerRequest request)
	{
		validateParameterNotNullStandardMessage("Find Customer Request", request);
		final String customerId = request.getCustomerId();
		validateParameterNotNullStandardMessage("customerId", customerId);
		try
		{
			final Customer customer = getBraintreeGateway().customer().find(customerId);
			boolean isCustomerExist = false;
			if (customer != null)
			{
				isCustomerExist = true;
			}
			BrainTreeFindCustomerResult result = new BrainTreeFindCustomerResult(isCustomerExist);
			result.setCustomer(customer);
			return result;
		}
		catch (final Exception exception)
		{
			if (exception instanceof NotFoundException)
			{
				LOG.error("[BT Find Customer] Can't find BrainTree customer with id: " + customerId);
				return new BrainTreeFindCustomerResult(false);
			}
			else
			{
				LOG.error("[BT Find Customer] Error during try to find customer: " + customerId);
				throw new IllegalArgumentException(exception.getMessage());
			}
		}
	}


	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * com.braintree.commands.BrainTreeFindCustomerCommand#process(com.braintree.command.request.BrainTreeCustomerRequest
	 * )
	 */
	@Override
	public BrainTreeFindCustomersResult process(final BrainTreeCustomerRequest request)
	{
		LOG.info("process, request: " + request);
		try
		{
			final CustomerSearchRequest customerSearchRequest = translateRequest(request);
			final ResourceCollection<Customer> customers = getBraintreeGateway().customer().search(customerSearchRequest);
			return new BrainTreeFindCustomersResult(customers);
		}
		catch (final Exception exception)
		{
			if (exception instanceof NotFoundException)
			{
				LOG.error("Can't find BrainTree Customer, message: " + exception.getMessage(), exception);
				return new BrainTreeFindCustomersResult();
			}
			else
			{
				LOG.error("Error while search for Customer, message: " + exception.getMessage(), exception);
				throw new IllegalArgumentException(exception.getMessage());
			}
		}
	}

	private CustomerSearchRequest translateRequest(final BrainTreeCustomerRequest request)
	{
		final CustomerSearchRequest customerSearchRequest = new CustomerSearchRequest();
		if (request != null)
		{
			if (StringUtils.isNotBlank(request.getCustomerId()))
			{
				customerSearchRequest.id().is(request.getCustomerId());
			}
			if (StringUtils.isNotBlank(request.getCustomerEmail()))
			{
				customerSearchRequest.email().is(request.getCustomerEmail());
			}
		}
		return customerSearchRequest;
	}

}
