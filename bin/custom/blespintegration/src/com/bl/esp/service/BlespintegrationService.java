/*
 * Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.esp.service;

public interface BlespintegrationService
{
	String getHybrisLogoUrl(String logoCode);

	void createLogo(String logoCode);
}
