package com.braintree.controllers.handler;

import com.braintree.constants.BraintreeaddonConstants;
import de.hybris.platform.acceleratorstorefrontcommons.constants.WebConstants;
import de.hybris.platform.commercefacades.user.UserFacade;
import de.hybris.platform.order.CartService;
import de.hybris.platform.servicelayer.session.SessionService;
import org.springframework.stereotype.Component;
import org.springframework.ui.Model;
import org.springframework.web.util.CookieGenerator;

import javax.annotation.Resource;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;

import static com.braintree.constants.BraintreeaddonWebConstants.REQUEST_MODEL_ATTRIBUTE_NAME;


@Component
public class PayPalUserLoginHandler
{
	@Resource(name = "userFacade")
	private UserFacade userFacade;

	@Resource(name = "sessionService")
	private SessionService sessionService;

	@Resource(name = "guidCookieGenerator")
	private CookieGenerator cookieGenerator;

	@Resource(name = "cartService")
	private CartService cartService;

	public boolean isHardLogin(final Model model)
	{

		final HttpServletRequest request = (HttpServletRequest) model.asMap().get(REQUEST_MODEL_ATTRIBUTE_NAME);
		return isHardLogin(request, false);
	}

	public boolean isHardLogin(final HttpServletRequest request, final boolean ignoreCookie)
	{
		boolean isHardLogin = false;
		sessionService.setAttribute(WebConstants.ANONYMOUS_CHECKOUT, Boolean.TRUE);

		final String guid = (String) request.getSession().getAttribute(BraintreeaddonConstants.SECURE_GUID_SESSION_KEY);
		if ((!userFacade.isAnonymousUser() && (checkForGUIDCookie(request, guid) || ignoreCookie)))
		{
			isHardLogin = true;
		}
		return isHardLogin;
	}


	protected boolean checkForGUIDCookie(final HttpServletRequest request, final String guid)
	{
		if (guid != null && request.getCookies() != null)
		{
			final String guidCookieName = cookieGenerator.getCookieName();
			if (guidCookieName != null)
			{
				for (final Cookie cookie : request.getCookies())
				{
					if (guidCookieName.equals(cookie.getName()))
					{
						if (guid.equals(cookie.getValue()))
						{
							return true;
						}
					}
				}
			}
		}

		return false;
	}

}
