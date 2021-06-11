package com.braintree.commands.impl;

import com.braintree.command.request.BrainTreeGenerateClientTokenRequest;
import com.braintree.command.result.BrainTreeGenerateClientTokenResult;
import com.braintree.commands.BrainTreeGenerateClientTokenCommand;
import com.braintreegateway.BraintreeGateway;
import com.braintreegateway.ClientTokenRequest;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;


public class GenerateClientTokenCommandImpl extends
		AbstractCommand<BrainTreeGenerateClientTokenRequest, BrainTreeGenerateClientTokenResult> implements
		BrainTreeGenerateClientTokenCommand
{
	private final static Logger LOG = Logger.getLogger(GenerateClientTokenCommandImpl.class);

	@Override
	public BrainTreeGenerateClientTokenResult perform(
			final BrainTreeGenerateClientTokenRequest brainTreeGenerateClientTokenRequest)
	{
		LOG.debug("[Generate Token] Start GenerateToken");

		try
		{
			final ClientTokenRequest clientTokenRequest = new ClientTokenRequest();

			if (brainTreeGenerateClientTokenRequest.getMerchantAccount() != null)
			{
				clientTokenRequest.merchantAccountId(brainTreeGenerateClientTokenRequest.getMerchantAccount());
			}

			LOG.info("clientTokenRequest: " + clientTokenRequest);
			LOG.info("clientTokenRequest.xml: " + clientTokenRequest.toXML()); // toQueryString

			final BraintreeGateway gateway = getBraintreeGateway();
			final String braintreeReply = gateway.clientToken().generate(clientTokenRequest);

			if (StringUtils.isNotBlank(braintreeReply))
			{
				LOG.debug(String.format("[Generate Token] Generated token: %s", braintreeReply));
			}
			else
			{
				final String errorMessage = "[Generate Token] Error due to creation token.";
				LOG.error(errorMessage);
				throw new IllegalArgumentException(errorMessage);
			}
			return translateResponse(braintreeReply);
		}
		catch (final Exception exception)
		{
			LOG.error("[Generate Token] Error message: " + exception.getMessage(), exception);
			throw new IllegalArgumentException(exception.getMessage(), exception);
		}
	}

	private BrainTreeGenerateClientTokenResult translateResponse(final String clientToken)
	{
		return new BrainTreeGenerateClientTokenResult(clientToken);
	}

}
