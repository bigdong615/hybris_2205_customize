/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.storefront.security;

import de.hybris.platform.acceleratorstorefrontcommons.security.BruteForceAttackCounter;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Required;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.authentication.SimpleUrlAuthenticationFailureHandler;

import com.bl.core.model.IpVelocityFilterModel;
import com.bl.core.service.ipVelocity.BlIpVelocityService;


public class LoginAuthenticationFailureHandler extends SimpleUrlAuthenticationFailureHandler
{
	private static final Logger LOG = Logger.getLogger(LoginAuthenticationFailureHandler.class);
	private BruteForceAttackCounter bruteForceAttackCounter;
	private ConfigurationService configurationService;
	private ModelService modelService;
	private UserService userService;
	private BlIpVelocityService blIpVelocityService;

	public static final String IP_ADDRESS_RESTRICT_DURATION = "bl.ipaddress.restrict.duration";
	public static final int MAX_LOGIN_FAILURES = 5;

	@Override
	public void onAuthenticationFailure(final HttpServletRequest request, final HttpServletResponse response,
			final AuthenticationException exception) throws IOException, ServletException
	{
		// Register brute attacks
		bruteForceAttackCounter.registerLoginFailure(request.getParameter("j_username"));

		String userIp = getUserIp(request);
		final int ipAddressRestrictDuration = Integer
				.parseInt(getConfigurationService().getConfiguration().getString(IP_ADDRESS_RESTRICT_DURATION));
		registerLoginFailureForIpAddress(request.getParameter("j_username"), userIp, ipAddressRestrictDuration);
		// Store the j_username in the session
		request.getSession().setAttribute("SPRING_SECURITY_LAST_USERNAME", request.getParameter("j_username"));
		
		final IpVelocityFilterModel velocityFilterModel = getBlIpVelocityService().getUserData(userIp, request.getParameter("j_username"));
		if (velocityFilterModel != null)
		{
			getBlIpVelocityService().updateDetails(velocityFilterModel, false);
		}

		super.onAuthenticationFailure(request, response, exception);
	}

	void registerLoginFailureForIpAddress(String userId, String ipAddress, int hours)
	{
		CustomerModel user = (CustomerModel) getUserService().getUserForUID(userId);
		Map<String, Integer> ipAddressCounter = new HashMap<String, Integer>();
		user.getIpCounter().keySet().forEach((key) -> {
			if (key.equalsIgnoreCase(ipAddress))
			{
				int counter = user.getIpCounter().get(ipAddress);
				if (counter > MAX_LOGIN_FAILURES)
				{
					user.setLoginDisabled(true);
				}
				ipAddressCounter.put(ipAddress, counter++);
				user.setIpCounter(ipAddressCounter);
			}
			else
			{
				ipAddressCounter.put(ipAddress, 1);
				user.setIpCounter(ipAddressCounter);
			}
			getModelService().save(user);
		});
	}


	private String getUserIp(final HttpServletRequest request)
	{
		final String trueClient = "True-Client-IP";
		final String cfConnectingIP = "CF-Connecting-IP";
		final String forwardFor = "X-Forwarded-For";
		String shopperIP = request.getHeader(trueClient);
		String headerUsed = trueClient;
		if (shopperIP == null)
		{
			headerUsed = cfConnectingIP;
			shopperIP = request.getHeader(cfConnectingIP);
		}
		if (shopperIP == null)
		{
			headerUsed = forwardFor;
			final String xfowardedHeader = request.getHeader(forwardFor);
			// to be extra sure I don't want this to throw an exception for any reason, so I'll add a try catch
			try
			{
				if (StringUtils.isNotBlank(xfowardedHeader))
				{
					final String[] headerParts = xfowardedHeader.split(",");
					if (headerParts.length > 0)
					{
						shopperIP = request.getHeader(headerParts[0]);
					}
				}
			}
			catch (final Exception ex)
			{
				LOG.info("Could not assign shopper IP from X-Forward-For header ", ex);
			}
		}
		if (shopperIP == null)
		{
			headerUsed = "Remote Address";
			shopperIP = request.getRemoteAddr();
		}
		if (shopperIP == null)
		{
			shopperIP = "";
		}
		return shopperIP;
	}


	protected BruteForceAttackCounter getBruteForceAttackCounter()
	{
		return bruteForceAttackCounter;
	}

	@Required
	public void setBruteForceAttackCounter(final BruteForceAttackCounter bruteForceAttackCounter)
	{
		this.bruteForceAttackCounter = bruteForceAttackCounter;
	}

	public ConfigurationService getConfigurationService()
	{
		return configurationService;
	}

	public void setConfigurationService(ConfigurationService configurationService)
	{
		this.configurationService = configurationService;
	}

	public UserService getUserService()
	{
		return userService;
	}

	public void setUserService(UserService userService)
	{
		this.userService = userService;
	}

	public ModelService getModelService()
	{
		return modelService;
	}

	public void setModelService(ModelService modelService)
	{
		this.modelService = modelService;
	}
	
	/**
	 * @return the blIpVelocityService
	 */
	public BlIpVelocityService getBlIpVelocityService()
	{
		return blIpVelocityService;
	}
	/**
	 * @param blIpVelocityService
	 *           the blIpVelocityService to set
	 */
	public void setBlIpVelocityService(final BlIpVelocityService blIpVelocityService)
	{
		this.blIpVelocityService = blIpVelocityService;
	}

}
