/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.storefront.security;

import de.hybris.platform.acceleratorstorefrontcommons.security.GUIDCookieStrategy;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Required;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.AuthenticationSuccessHandler;

import com.bl.core.model.IpVelocityFilterModel;
import com.bl.core.service.ipVelocity.BlIpVelocityService;


/**
 * Default implementation of {@link AuthenticationSuccessHandler}
 */
public class GUIDAuthenticationSuccessHandler implements AuthenticationSuccessHandler
{
	private static final Logger LOG = Logger.getLogger(GUIDAuthenticationSuccessHandler.class);
	private GUIDCookieStrategy guidCookieStrategy;
	private AuthenticationSuccessHandler authenticationSuccessHandler;
	private BlIpVelocityService blIpVelocityService;


	@Override
	public void onAuthenticationSuccess(final HttpServletRequest request, final HttpServletResponse response,
			final Authentication authentication) throws IOException, ServletException
	{
		getGuidCookieStrategy().setCookie(request, response);
		getAuthenticationSuccessHandler().onAuthenticationSuccess(request, response, authentication);
		final String userIp = getUserIp(request);

		final IpVelocityFilterModel velocityFilterModel = getBlIpVelocityService().getUserData(userIp,
				(String) authentication.getPrincipal());
		if (velocityFilterModel != null)
		{
			getBlIpVelocityService().updateDetails(velocityFilterModel, true);

		}
		else
		{
			getBlIpVelocityService().createNewEntry(userIp, (String) authentication.getPrincipal());

		}

		getBlIpVelocityService().checkIfIpNeedsToBlock(userIp);

		LOG.info("Customer with email id :- " + authentication.getPrincipal() + " Logged in successfully with IP address :- "
				+ userIp);
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

	protected GUIDCookieStrategy getGuidCookieStrategy()
	{
		return guidCookieStrategy;
	}

	/**
	 * @param guidCookieStrategy
	 *           the guidCookieStrategy to set
	 */
	@Required
	public void setGuidCookieStrategy(final GUIDCookieStrategy guidCookieStrategy)
	{
		this.guidCookieStrategy = guidCookieStrategy;
	}

	protected AuthenticationSuccessHandler getAuthenticationSuccessHandler()
	{
		return authenticationSuccessHandler;
	}

	/**
	 * @param authenticationSuccessHandler
	 *           the authenticationSuccessHandler to set
	 */
	@Required
	public void setAuthenticationSuccessHandler(final AuthenticationSuccessHandler authenticationSuccessHandler)
	{
		this.authenticationSuccessHandler = authenticationSuccessHandler;
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
