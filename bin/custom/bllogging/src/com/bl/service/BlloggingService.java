/*
 * Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.service;

public interface BlloggingService
{
	String getHybrisLogoUrl(String logoCode);

	void createLogo(String logoCode);
}
