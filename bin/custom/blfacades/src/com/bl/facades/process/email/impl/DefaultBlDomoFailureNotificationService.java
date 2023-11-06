/**
 *
 */
package com.bl.facades.process.email.impl;

import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.util.mail.MailUtils;

import org.apache.commons.mail.EmailException;
import org.apache.commons.mail.HtmlEmail;

import com.bl.facades.process.email.BlDomoFailureNotificationService;


/**
 * @author Kumar
 *
 */
public class DefaultBlDomoFailureNotificationService implements BlDomoFailureNotificationService
{

	private ConfigurationService configurationService;
	public static final String DOMO_FAILURE_NOTIFI_EMAIL = "domo.failure.email";

	@Override
	public void send(final String exception, final String pk, final String API)
	{
		try
		{
			final String emailFromAddress = "noreply@shutterfly.com";
			final String emailDName = "BL Domo alert";
			final String emailToAdd = getConfigurationService().getConfiguration().getString(DOMO_FAILURE_NOTIFI_EMAIL);
			final HtmlEmail email = (HtmlEmail) MailUtils.getPreConfiguredEmail();
			email.setCharset("UTF-8");
			email.addTo(emailToAdd);
			email.setSubject("DOMO Error For" + " " + API + " " + pk);
			email.setHtmlMsg(exception);
			email.send();
		}
		catch (final EmailException e)
		{
			e.printStackTrace();
		}
	}

	/**
	 * @return the configurationService
	 */
	public ConfigurationService getConfigurationService()
	{
		return configurationService;
	}

	/**
	 * @param configurationService
	 *           the configurationService to set
	 */
	public void setConfigurationService(final ConfigurationService configurationService)
	{
		this.configurationService = configurationService;
	}

}
