/*
 * [y] hybris Platform
 *
 * Copyright (c) 2018 SAP SE or an SAP affiliate company. All rights reserved.
 *
 * This software is the confidential and proprietary information of SAP
 * ("Confidential Information"). You shall not disclose such Confidential
 * Information and shall use it only in accordance with the terms of the
 * license agreement you entered into with SAP.
 */
package com.bl.constants;

/**
 * Global class for all Blassistedservicestorefront constants. You can add global constants for your extension into this
 * class.
 */
public final class BlassistedservicestorefrontConstants extends GeneratedBlassistedservicestorefrontConstants
{
	public static final String EXTENSIONNAME = "blassistedservicestorefront";
	public static final String REDIRECT_WITH_CART = "blassistedservicestorefront.redirect.customer_and_cart";
	public static final String REDIRECT_WITH_ORDER = "blassistedservicestorefront.redirect.order";
	public static final String REDIRECT_CUSTOMER_ONLY = "blassistedservicestorefront.redirect.customer_only";
	public static final String REDIRECT_ERROR = "blassistedservicestorefront.redirect.error";
	public static final String AIF_TIMEOUT = "blassistedservicestorefront.aif.timeout";
	public static final int AIF_DEFAULT_TIMEOUT = 7000; //default timeout in milliseconds
	public static final String AIF_OVERVIEW_CART_ITMES_TO_BE_DISPLAYED = "aif.overview.cart.items.to.display";
	public static final int AIF_OVERVIEW_CART_ITMES_TO_BE_DISPLAYED_DEFAULT = 6;
	public static final String PROFILE_COOKIE_NAME = "blassistedservicestorefront.profile.cookie.name";
	public static final int IMPERSISTENCE_COOKIE_INDEX = -1;
	public static final String ASM_REQUEST_PARAM = "asm";
	public static final String ASM_PROFILE_TRACKING_PAUSE_COOKIE = "profile.tracking.pause";

	// Default parent group id for all AS agents
	public static final String AS_AGENT_GROUP_UID = "asagentgroup";


	private BlassistedservicestorefrontConstants()
	{
		//empty to avoid instantiating this constant class
	}
}
