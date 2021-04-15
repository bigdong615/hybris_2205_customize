package com.bl.storefront.security.cookie;

/**
 * Cookie generator of selected date
 *
 * @author Moumita
 */
public class BlDateRestoreCookieGenerator extends EnhancedCookieGenerator
{
	private static final String SELECTED_DATE = "selectedDate";

	@Override
	public String getCookieName()
	{
		return SELECTED_DATE;
	}
}
