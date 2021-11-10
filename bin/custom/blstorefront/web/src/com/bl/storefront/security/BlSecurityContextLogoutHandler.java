package com.bl.storefront.security;

import de.hybris.platform.commercefacades.order.data.CartData;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.logout.LogoutHandler;
import org.springframework.util.Assert;

import com.bl.facades.cart.BlCartFacade;




/**
 * Performs a logout by modifying the {@link org.springframework.security.core.context.SecurityContextHolder}.
 * <p>
 * Will also invalidate the {@link HttpSession} if {@link #isInvalidateHttpSession()} is {@code true} and the session is
 * not {@code null}.
 * <p>
 * Will also remove the {@link Authentication} from the current {@link SecurityContext} if {@link #clearAuthentication}
 * is set to true (default).
 *
 * @author Gaurav Patil
 *
 */
public class BlSecurityContextLogoutHandler implements LogoutHandler
{
	protected final Log logger = LogFactory.getLog(this.getClass());

	private boolean invalidateHttpSession = true;
	private boolean clearAuthentication = true;

	@Autowired
	private BlCartFacade blCartFacade;


	/**
	 * Requires the request to be passed in.
	 *
	 * @param request
	 *           from which to obtain a HTTP session (cannot be null)
	 * @param response
	 *           not used (can be <code>null</code>)
	 * @param authentication
	 *           not used (can be <code>null</code>)
	 */
	public void logout(final HttpServletRequest request, final HttpServletResponse response, final Authentication authentication)
	{
		Assert.notNull(request, "HttpServletRequest required");

		//Added condition to remove cart entries for usedGear cart

		final CartData cartData = blCartFacade.getSessionCart();
		if (BooleanUtils.isFalse(cartData.getIsRentalCart()) && BooleanUtils.isFalse(cartData.getHasGiftCart()) )
		{
			blCartFacade.removeCartEntries();
		}
		if (invalidateHttpSession)
		{


			final HttpSession session = request.getSession(false);
			if (session != null)
			{
				logger.debug("Invalidating session: " + session.getId());
				session.invalidate();
			}
		}

		if (clearAuthentication)
		{
			final SecurityContext context = SecurityContextHolder.getContext();
			context.setAuthentication(null);
		}

		SecurityContextHolder.clearContext();
	}

	public boolean isInvalidateHttpSession()
	{
		return invalidateHttpSession;
	}

	/**
	 * Causes the {@link HttpSession} to be invalidated when this {@link LogoutHandler} is invoked. Defaults to true.
	 *
	 * @param invalidateHttpSession
	 *           true if you wish the session to be invalidated (default) or false if it should not be.
	 */
	public void setInvalidateHttpSession(final boolean invalidateHttpSession)
	{
		this.invalidateHttpSession = invalidateHttpSession;
	}

	/**
	 * If true, removes the {@link Authentication} from the {@link SecurityContext} to prevent issues with concurrent
	 * requests.
	 *
	 * @param clearAuthentication
	 *           true if you wish to clear the {@link Authentication} from the {@link SecurityContext} (default) or false
	 *           if the {@link Authentication} should not be removed.
	 */
	public void setClearAuthentication(final boolean clearAuthentication)
	{
		this.clearAuthentication = clearAuthentication;
	}
}
