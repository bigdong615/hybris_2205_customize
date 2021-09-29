/*
 * Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.esp.setup;

import static com.bl.esp.constants.BlespintegrationConstants.PLATFORM_LOGO_CODE;

import de.hybris.platform.core.initialization.SystemSetup;

import java.io.InputStream;

import com.bl.esp.constants.BlespintegrationConstants;
import com.bl.esp.service.BlespintegrationService;


@SystemSetup(extension = BlespintegrationConstants.EXTENSIONNAME)
public class BlespintegrationSystemSetup
{
	private final BlespintegrationService blespintegrationService;

	public BlespintegrationSystemSetup(final BlespintegrationService blespintegrationService)
	{
		this.blespintegrationService = blespintegrationService;
	}

	@SystemSetup(process = SystemSetup.Process.INIT, type = SystemSetup.Type.ESSENTIAL)
	public void createEssentialData()
	{
		blespintegrationService.createLogo(PLATFORM_LOGO_CODE);
	}

	private InputStream getImageStream()
	{
		return BlespintegrationSystemSetup.class.getResourceAsStream("/blespintegration/sap-hybris-platform.png");
	}
}
