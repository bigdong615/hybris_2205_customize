/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.storefront.security;

import com.bl.storefront.controllers.pages.BlControllerConstants;
import de.hybris.platform.acceleratorstorefrontcommons.security.BruteForceAttackCounter;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.springframework.beans.factory.annotation.Required;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.authentication.SimpleUrlAuthenticationFailureHandler;


public class LoginAuthenticationFailureHandler extends SimpleUrlAuthenticationFailureHandler
{
	private BruteForceAttackCounter bruteForceAttackCounter;
	private boolean useReferer = false;
	private boolean checkoutLogin = false;

	@Override
	public void onAuthenticationFailure(final HttpServletRequest request, final HttpServletResponse response,
			final AuthenticationException exception) throws IOException, ServletException
	{
		String targetUrl = request.getHeader(BlControllerConstants.REFERER);
		// Register brute attacks
		bruteForceAttackCounter.registerLoginFailure(request.getParameter("j_username"));

		// Store the j_username in the session
		request.getSession().setAttribute("SPRING_SECURITY_LAST_USERNAME", request.getParameter("j_username"));

		HttpSession session = request.getSession(false);
// This is redirect same page for all pages before checkout.
		if (session != null || isAllowSessionCreation())
		{
			
			if (targetUrl == null) {
				super.setDefaultFailureUrl(BlControllerConstants.HOME_PAGE_URL);
			} else {
				super.setDefaultFailureUrl(targetUrl);
			}
		}
		// This is redirect to checkout login page.
		if(this.useReferer && checkoutLogin)
		{
			if (targetUrl == null) {
				super.setDefaultFailureUrl(BlControllerConstants.HOME_PAGE_URL);
			} else {
				super.setDefaultFailureUrl(targetUrl);
			}
		}

		super.onAuthenticationFailure(request, response, exception);
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



	public boolean isUseReferer() {
		return useReferer;
	}



	public void setUseReferer(boolean useReferer) {
		this.useReferer = useReferer;
	}



	public boolean isCheckoutLogin() {
		return checkoutLogin;
	}



	public void setCheckoutLogin(boolean checkoutLogin) {
		this.checkoutLogin = checkoutLogin;
	}

}
