/*
 * Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.patches.setup;

import static com.bl.patches.constants.BlpatchesConstants.PLATFORM_LOGO_CODE;

import de.hybris.platform.commerceservices.dataimport.impl.SampleDataImportService;
import de.hybris.platform.commerceservices.setup.AbstractSystemSetup;
import de.hybris.platform.core.initialization.SystemSetup;
import de.hybris.platform.core.initialization.SystemSetup.Process;
import de.hybris.platform.core.initialization.SystemSetup.Type;
import de.hybris.platform.core.initialization.SystemSetupContext;
import de.hybris.platform.core.initialization.SystemSetupParameter;
import de.hybris.platform.core.initialization.SystemSetupParameterMethod;
import de.hybris.platform.servicelayer.config.ConfigurationService;

import java.io.InputStream;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import com.bl.patches.constants.BlpatchesConstants;
import com.bl.patches.service.BlpatchesService;


@SystemSetup(extension = BlpatchesConstants.EXTENSIONNAME)
public class BlpatchesSystemSetup extends AbstractSystemSetup
{
	/**
	 *
	 */
	private static final String SITE_BL = "bl";
	private static final String IMPORT_PATCHES = "importPatches";
	private static final String SYNC_CATALOGS = "syncCatalogs";
	private static final String PATCHES_ALL = "all";
	private static final String PATCHES_NONE = "none";
	public static final String UNDERSCORE = "_";

	private SampleDataImportService sampleDataImportService;
	private BlpatchesService blpatchesService;
	private ConfigurationService configurationService;

	/**
	 *
	 * Generates the Dropdown and Multi-select boxes for the project data import
	 */
	@Override
	@SystemSetupParameterMethod
	public List<SystemSetupParameter> getInitializationOptions()
	{
		final List<SystemSetupParameter> params = new ArrayList<>();
		params.add(createPatchSystemSetupParameter(IMPORT_PATCHES, "Import Patch"));
		params.add(createBooleanSystemSetupParameter(SYNC_CATALOGS, "Sync Product & Content Catalogs", false));
		return params;
	}

	/**
	 * Creates the patch system setup parameter.
	 *
	 * @param key
	 *           the key
	 * @param label
	 *           the label
	 * @param isSystemInitialize
	 *           the is system initialize
	 * @return the system setup parameter
	 */
	private SystemSetupParameter createPatchSystemSetupParameter(final String key, final String label)
	{
		final SystemSetupParameter param = new SystemSetupParameter(key);
		param.setLabel(label);
		param.addValue(PATCHES_NONE, true);
		param.addValue(PATCHES_ALL, false);
		for (final Path path : getBlpatchesService().getAllPatches().descendingSet())
		{
			param.addValue(path.getFileName().toString().replace(BlpatchesConstants.PATCH_FILE_EXTENSION, ""), false);
		}
		return param;
	}

	/**
	 * Implement this method to create data that is used in your project. This method will be called during the system
	 * initialization.
	 *
	 * @param context
	 *           the context provides the selected parameters and values
	 */
	@SystemSetup(type = Type.PROJECT, process = Process.ALL)
	public void createProjectData(final SystemSetupContext context)
	{
		final String patchId = getPatchIdFromSystemSetupParameter(context);
		if (PATCHES_ALL.equals(patchId))
		{
			logInfo(context, "Begin importing <all> patches");
			getBlpatchesService().importAllPatches();
			logInfo(context, "Done importing <all> patches");
		}
		else if (!PATCHES_NONE.equals(patchId))
		{
			logInfo(context, "Begin importing patch <" + patchId + ">");
			getBlpatchesService().importPatchWithId(patchId);
			logInfo(context, "Done importing patch <" + patchId + ">");
		}

		if (getBooleanSystemSetupParameter(context, SYNC_CATALOGS))
		{
			sampleDataImportService.synchronizeProductCatalog(this, context, SITE_BL, true);
			sampleDataImportService.synchronizeContentCatalog(this, context, SITE_BL, true);
		}
	}

	/**
	 * Get the the patch to import from the {@link SystemSetupContext} parameter. Returns {@link #PATCHES_ALL} if the
	 * parameter was not found (i.e. during initialization from the console).
	 *
	 * @param context
	 *           The setup context
	 * @return The patch to import
	 */
	private String getPatchIdFromSystemSetupParameter(final SystemSetupContext context)
	{
		final String patchId = context.getParameter(context.getExtensionName() + UNDERSCORE + IMPORT_PATCHES);
		return null != patchId ? patchId : PATCHES_ALL;
	}

	/**
	 * Creates the essential data.
	 */
	@SystemSetup(process = SystemSetup.Process.INIT, type = SystemSetup.Type.ESSENTIAL)
	public void createEssentialData()
	{
		getBlpatchesService().createLogo(PLATFORM_LOGO_CODE);
	}

	/**
	 * Gets the image stream.
	 *
	 * @return the image stream
	 */
	private InputStream getImageStream()
	{
		return BlpatchesSystemSetup.class.getResourceAsStream("/blpatches/sap-hybris-platform.png");
	}

	/**
	 * Gets the sample data import service.
	 *
	 * @return the sample data import service
	 */
	public SampleDataImportService getSampleDataImportService()
	{
		return sampleDataImportService;
	}

	/**
	 * Sets the sample data import service.
	 *
	 * @param sampleDataImportService
	 *           the new sample data import service
	 */
	public void setSampleDataImportService(final SampleDataImportService sampleDataImportService)
	{
		this.sampleDataImportService = sampleDataImportService;
	}

	/**
	 * Gets the blpatches service.
	 *
	 * @return the blpatches service
	 */
	public BlpatchesService getBlpatchesService()
	{
		return blpatchesService;
	}

	/**
	 * Sets the blpatches service.
	 *
	 * @param blpatchesService
	 *           the new blpatches service
	 */
	public void setBlpatchesService(final BlpatchesService blpatchesService)
	{
		this.blpatchesService = blpatchesService;
	}

	/**
	 * Gets the configuration service.
	 *
	 * @return the configuration service
	 */
	public ConfigurationService getConfigurationService()
	{
		return configurationService;
	}

	/**
	 * Sets the configuration service.
	 *
	 * @param configurationService
	 *           the new configuration service
	 */
	public void setConfigurationService(final ConfigurationService configurationService)
	{
		this.configurationService = configurationService;
	}
}
