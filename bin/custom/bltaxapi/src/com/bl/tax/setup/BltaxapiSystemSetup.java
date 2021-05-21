/*
 * Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.tax.setup;

import static com.bl.tax.constants.BltaxapiConstants.PLATFORM_LOGO_CODE;

import de.hybris.platform.core.initialization.SystemSetup;

import java.io.InputStream;

import com.bl.tax.constants.BltaxapiConstants;
import com.bl.tax.service.BltaxapiService;


@SystemSetup(extension = BltaxapiConstants.EXTENSIONNAME)
public class BltaxapiSystemSetup
{
	private final BltaxapiService bltaxapiService;

	public BltaxapiSystemSetup(final BltaxapiService bltaxapiService)
	{
		this.bltaxapiService = bltaxapiService;
	}

	@SystemSetup(process = SystemSetup.Process.INIT, type = SystemSetup.Type.ESSENTIAL)
	public void createEssentialData()
	{
		bltaxapiService.createLogo(PLATFORM_LOGO_CODE);
	}

	private InputStream getImageStream()
	{
		return BltaxapiSystemSetup.class.getResourceAsStream("/bltaxapi/sap-hybris-platform.png");
	}
}
