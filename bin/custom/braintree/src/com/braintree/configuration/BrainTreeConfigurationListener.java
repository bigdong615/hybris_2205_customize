/**
 *
 */
package com.braintree.configuration;

import de.hybris.platform.core.Registry;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.servicelayer.config.impl.HybrisConfiguration;
import de.hybris.platform.util.config.ConfigIntf;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import java.util.Enumeration;

import static com.braintree.constants.BraintreeConstants.BRAINTREE_ENVIRONMENT;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_MERCHANT_ID;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_PRIVATE_KEY;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_PUBLIC_KEY;
import static com.braintree.constants.BraintreeConstants.LOG_ALL_ENABLE;
import static com.braintree.constants.BraintreeConstants.LOG_PACKAGE_PATH;


public class BrainTreeConfigurationListener implements ConfigIntf.ConfigChangeListener
{
	private final static Logger LOG = Logger.getLogger(BrainTreeConfigurationListener.class);

	private static ConfigurationService getConfigurationService()
	{
		return (ConfigurationService) Registry.getApplicationContext().getBean("configurationService");
	}

	@Override
	public void configChanged(final String key, final String newValue)
	{
		if (LOG_ALL_ENABLE.equals(key))
		{
			LOG.info(String.format("[Configuration properties] Configuration property: %s was changed on %s", key, newValue));
			if (Boolean.TRUE.equals(Boolean.valueOf(newValue)))
			{
				LOG.info("[Configuration properties] now debug level is: " + Level.ALL);
				setLogLevel(Level.ALL);
			}
			else
			{
				LOG.info("[Configuration properties] now debug level is: " + Level.ERROR);
				setLogLevel(Level.ERROR);
			}
		}

		if (BRAINTREE_ENVIRONMENT.equals(key))
		{
			LOG.info("Clear merchant gateway account credentials on environment type change");
			clearMerchantGatewayAccountCredentials();
		}
	}

	private void clearMerchantGatewayAccountCredentials()
	{
		final HybrisConfiguration hybrisConfiguration = (HybrisConfiguration) getConfigurationService().getConfiguration();
		hybrisConfiguration.setProperty(BRAINTREE_PRIVATE_KEY, "");
		hybrisConfiguration.setProperty(BRAINTREE_MERCHANT_ID, "");
		hybrisConfiguration.setProperty(BRAINTREE_PUBLIC_KEY, "");
	}

	private void setLogLevel(final Level level)
	{
		final Enumeration<Logger> currentLoggers = LogManager.getCurrentLoggers();
		while (currentLoggers.hasMoreElements())
		{
			final Logger logger = currentLoggers.nextElement();
			if (StringUtils.equalsIgnoreCase(logger.getName(), LOG_PACKAGE_PATH))
			{
				logger.setLevel(level);
			}
		}
	}
}
