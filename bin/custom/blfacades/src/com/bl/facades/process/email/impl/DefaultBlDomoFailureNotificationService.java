/**
 *
 */
package com.bl.facades.process.email.impl;

import de.hybris.platform.servicelayer.config.ConfigurationService;

import org.apache.commons.mail.Email;
import org.apache.commons.mail.EmailException;
import org.apache.commons.mail.SimpleEmail;

import com.bl.facades.process.email.BlDomoFailureNotificationService;


/**
 * @author Kumar
 *
 */
public class DefaultBlDomoFailureNotificationService implements BlDomoFailureNotificationService
{

	private ConfigurationService configurationService;
	public static final String DOMO_FAILURE_NOTIFI_EMAIL = "domo.failure.email";
	public static final String DOMO_MAIL_SMTP_SERVER = "domo.mail.smtp.server";
	public static final String DOMO_MAIL_SMTP_PORT = "domo.mail.smtp.port";
	public static final String DOMO_MAIL_SMTP_USER = "domo.mail.smtp.user";
	public static final String DOMO_MAIL_SMTP_PWD = "domo.mail.smtp.password";
	public static final String DOMO_MAIL_FROM = "domo.mail.from";


	@Override
	public void send(final String exception, final String pk, final String API)
	{
		try
		{
			//final String emailFromAddress = "noreply@shutterfly.com";
			final String emailDName = "BL Domo alert";
			final String emailToAdd = getConfigurationService().getConfiguration().getString(DOMO_FAILURE_NOTIFI_EMAIL);
			final String emailServer = getConfigurationService().getConfiguration().getString(DOMO_MAIL_SMTP_SERVER);
			final String emailPort = getConfigurationService().getConfiguration().getString(DOMO_MAIL_SMTP_PORT);
			final String emailUser = getConfigurationService().getConfiguration().getString(DOMO_MAIL_SMTP_USER);
			final String emailPwd = getConfigurationService().getConfiguration().getString(DOMO_MAIL_SMTP_PWD);
			final String emailFrom = getConfigurationService().getConfiguration().getString(DOMO_MAIL_FROM);
			final Email email = new SimpleEmail();
			email.setCharset("UTF-8");
			email.setSmtpPort(Integer.parseInt(emailPort));
			email.setHostName(emailServer);
			email.setTLS(true);
			email.setFrom(emailFrom);
			email.setSocketConnectionTimeout(10000);
			email.setSocketTimeout(10000);
			email.setAuthentication(emailUser, emailPwd);
			email.addTo(emailToAdd);
			email.setSubject("DOMO Error For" + " " + API + " " + pk);
			email.setMsg(exception);
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
