/*
 * Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.tax.service;

public interface BltaxapiService
{
	String getHybrisLogoUrl(String logoCode);

	void createLogo(String logoCode);
}
