/**
 *
 */
package com.braintree.commands.impl;

import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintreegateway.BraintreeGateway;
import de.hybris.platform.payment.commands.request.AbstractRequest;
import de.hybris.platform.util.localization.Localization;
import org.apache.commons.lang.StringUtils;

import static com.braintree.constants.BraintreeConstants.BRAINTREE_ECVZ_ACEESS_TOKEN;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_MERCHANT_ID;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_PRIVATE_KEY;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_PUBLIC_KEY;
import static de.hybris.platform.util.Config.getParameter;


public abstract class AbstractCommand<RequestType, ResponseType>
{
	protected AbstractRequest request;
	private BrainTreeLoggingHandler loggingHandler;
	private BraintreeCodeTranslator codeTranslator;
	private BraintreeErrorTranslator errorTranslator;
	private BrainTreeConfigService brainTreeConfigService;

	public BraintreeGateway getBraintreeGateway()
	{
		final BraintreeGateway gateway;
		if (StringUtils.isNotEmpty(getParameter(BRAINTREE_ECVZ_ACEESS_TOKEN)))
		{
			gateway = new BraintreeGateway(getParameter(BRAINTREE_ECVZ_ACEESS_TOKEN));

		}
		else
		{
			gateway = new BraintreeGateway(getBrainTreeConfigService().getEnvironmentType(),
					getParameter(BRAINTREE_MERCHANT_ID), getParameter(BRAINTREE_PUBLIC_KEY), getParameter(BRAINTREE_PRIVATE_KEY));
		}
		return gateway;
	}


	protected String getLocalizedErrorMessage(String errorMessageCode)
	{
		return Localization.getLocalizedString(errorMessageCode);
	}

	/**
	 * @return the loggingHandler
	 */
	public BrainTreeLoggingHandler getLoggingHandler()
	{
		return loggingHandler;
	}

	/**
	 * @param loggingHandler
	 *           the loggingHandler to set
	 */
	public void setLoggingHandler(final BrainTreeLoggingHandler loggingHandler)
	{
		this.loggingHandler = loggingHandler;
	}

	/**
	 * @return the codeTranslator
	 */
	public BraintreeCodeTranslator getCodeTranslator()
	{
		return codeTranslator;
	}

	/**
	 * @param codeTranslator
	 *           the codeTranslator to set
	 */
	public void setCodeTranslator(final BraintreeCodeTranslator codeTranslator)
	{
		this.codeTranslator = codeTranslator;
	}

	/**
	 * @return the errorTranslator
	 */
	public BraintreeErrorTranslator getErrorTranslator()
	{
		return errorTranslator;
	}

	/**
	 * @param errorTranslator
	 *           the errorTranslator to set
	 */
	public void setErrorTranslator(final BraintreeErrorTranslator errorTranslator)
	{
		this.errorTranslator = errorTranslator;
	}

	/**
	 * @return the brainTreeConfigService
	 */
	public BrainTreeConfigService getBrainTreeConfigService()
	{
		return brainTreeConfigService;
	}

	/**
	 * @param brainTreeConfigService
	 *           the brainTreeConfigService to set
	 */
	public void setBrainTreeConfigService(final BrainTreeConfigService brainTreeConfigService)
	{
		this.brainTreeConfigService = brainTreeConfigService;
	}
}
