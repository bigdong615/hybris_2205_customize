/*
 * Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.setup;

import static com.bl.constants.BlhotfolderConstants.PLATFORM_LOGO_CODE;

import de.hybris.platform.core.initialization.SystemSetup;

import java.io.InputStream;

import com.bl.constants.BlhotfolderConstants;
import com.bl.service.BlhotfolderService;


@SystemSetup(extension = BlhotfolderConstants.EXTENSIONNAME)
public class BlhotfolderSystemSetup
{
	private final BlhotfolderService blhotfolderService;

	public BlhotfolderSystemSetup(final BlhotfolderService blhotfolderService)
	{
		this.blhotfolderService = blhotfolderService;
	}

	@SystemSetup(process = SystemSetup.Process.INIT, type = SystemSetup.Type.ESSENTIAL)
	public void createEssentialData()
	{
		blhotfolderService.createLogo(PLATFORM_LOGO_CODE);
	}

	private InputStream getImageStream()
	{
		return BlhotfolderSystemSetup.class.getResourceAsStream("/blhotfolder/sap-hybris-platform.png");
	}
}
