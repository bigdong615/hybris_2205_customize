package com.bl.storefront.security.cookie;

import com.bl.storefront.controllers.pages.BlControllerConstants;

/**
 * Cookie generator of selected date
 *
 * @author Moumita
 */
public class BlRentalDateCookieGenerator extends EnhancedCookieGenerator
{
	@Override
	public String getCookieName()
	{
		return BlControllerConstants.SELECTED_DATE;
	}
}
