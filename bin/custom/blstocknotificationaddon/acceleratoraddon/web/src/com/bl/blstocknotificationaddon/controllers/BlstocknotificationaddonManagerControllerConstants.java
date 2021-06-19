/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.blstocknotificationaddon.controllers;

/**
 */
public interface BlstocknotificationaddonManagerControllerConstants
{
	String ADDON_PREFIX = "addon:/blstocknotificationaddon/";

	interface Pages
	{
		String AddNotificationPage = ADDON_PREFIX + "pages/addStockNotification";
		String AddNotificationFromInterestPage = ADDON_PREFIX + "pages/addStockNotificationFromInterest";
	}
}
