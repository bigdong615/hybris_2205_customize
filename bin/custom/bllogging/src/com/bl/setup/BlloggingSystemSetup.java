/*
 * Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.setup;

import static com.bl.constants.BlloggingConstants.PLATFORM_LOGO_CODE;

import de.hybris.platform.core.initialization.SystemSetup;

import java.io.InputStream;

import com.bl.constants.BlloggingConstants;
import com.bl.service.BlloggingService;


@SystemSetup(extension = BlloggingConstants.EXTENSIONNAME)
public class BlloggingSystemSetup
{
	private final BlloggingService blloggingService;

	public BlloggingSystemSetup(final BlloggingService blloggingService)
	{
		this.blloggingService = blloggingService;
	}

	@SystemSetup(process = SystemSetup.Process.INIT, type = SystemSetup.Type.ESSENTIAL)
	public void createEssentialData()
	{
		blloggingService.createLogo(PLATFORM_LOGO_CODE);
	}

	private InputStream getImageStream()
	{
		return BlloggingSystemSetup.class.getResourceAsStream("/bllogging/sap-hybris-platform.png");
	}
}
