/*
 * Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.patches.service.impl;

import de.hybris.platform.catalog.model.CatalogUnawareMediaModel;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.servicelayer.exceptions.SystemException;
import de.hybris.platform.servicelayer.impex.ImportService;
import de.hybris.platform.servicelayer.impex.impl.FileBasedImpExResource;
import de.hybris.platform.servicelayer.media.MediaService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.util.Config;

import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.Comparator;
import java.util.Optional;
import java.util.TreeSet;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.bl.patches.constants.BlpatchesConstants;
import com.bl.patches.exception.BlPatchImportException;
import com.bl.patches.service.BlpatchesService;
import com.bl.patches.setup.BlpatchesSystemSetup;
import com.bl.patches.util.AlphanumComparator;


// TODO: Auto-generated Javadoc
/**
 * The Class DefaultBlpatchesService.
 */
public class DefaultBlpatchesService implements BlpatchesService
{

	/** The Constant LOG. */
	private static final Logger LOG = LoggerFactory.getLogger(DefaultBlpatchesService.class);

	/** The Constant PROPERTY_PATH. */
	private static final String PROPERTY_PATH = BlpatchesConstants.EXTENSIONNAME + ".import.path";

	/** The Constant PROPERTY_ID_PATTERN. */
	private static final String PROPERTY_ID_PATTERN = BlpatchesConstants.EXTENSIONNAME + ".import.id.regexp";

	/** The Constant FIND_LOGO_QUERY. */
	private static final String FIND_LOGO_QUERY = "SELECT {" + CatalogUnawareMediaModel.PK + "} FROM {"
			+ CatalogUnawareMediaModel._TYPECODE + "} WHERE {" + CatalogUnawareMediaModel.CODE + "}=?code";

	/** The media service. */
	private MediaService mediaService;

	/** The model service. */
	private ModelService modelService;

	/** The flexible search service. */
	private FlexibleSearchService flexibleSearchService;

	/** The import service. */
	private ImportService importService;

	/**
	 * Gets the hybris logo url.
	 *
	 * @param logoCode
	 *           the logo code
	 * @return the hybris logo url
	 */
	@Override
	public String getHybrisLogoUrl(final String logoCode)
	{
		final MediaModel media = mediaService.getMedia(logoCode);

		// Keep in mind that with Slf4j you don't need to check if debug is enabled, it is done under the hood.
		LOG.debug("Found media [code: {}]", media.getCode());

		return media.getURL();
	}

	/**
	 * Creates the logo.
	 *
	 * @param logoCode
	 *           the logo code
	 */
	@Override
	public void createLogo(final String logoCode)
	{
		final Optional<CatalogUnawareMediaModel> existingLogo = findExistingLogo(logoCode);

		final CatalogUnawareMediaModel media = existingLogo.isPresent() ? existingLogo.get()
				: modelService.create(CatalogUnawareMediaModel.class);
		media.setCode(logoCode);
		media.setRealFileName("sap-hybris-platform.png");
		modelService.save(media);

		mediaService.setStreamForMedia(media, getImageStream());
	}

	/**
	 * Find existing logo.
	 *
	 * @param logoCode
	 *           the logo code
	 * @return the optional
	 */
	private Optional<CatalogUnawareMediaModel> findExistingLogo(final String logoCode)
	{
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(FIND_LOGO_QUERY);
		fQuery.addQueryParameter("code", logoCode);

		try
		{
			return Optional.of(flexibleSearchService.searchUnique(fQuery));
		}
		catch (final SystemException e)
		{
			return Optional.empty();
		}
	}

	/**
	 * Gets the image stream.
	 *
	 * @return the image stream
	 */
	private InputStream getImageStream()
	{
		return DefaultBlpatchesService.class.getResourceAsStream("/blpatches/sap-hybris-platform.png");
	}

	/**
	 * Import patch with id.
	 *
	 * @param patchId
	 *           the patch id
	 */
	@Override
	public void importPatchWithId(final String patchId)
	{
		if (!patchId.matches(getProperty(PROPERTY_ID_PATTERN)))
		{
			throw new BlPatchImportException(String.format("Invalid patch format <%s>", patchId));
		}
		final URL resource = BlpatchesSystemSetup.class
				.getResource(getProperty(PROPERTY_PATH) + "/" + patchId + BlpatchesConstants.PATCH_FILE_EXTENSION);
		try
		{
			if (null == resource)
			{
				throw new BlPatchImportException(String.format("Patch <%s> does not exist", patchId));
			}
			importPatchOnPath(Paths.get(resource.toURI()));
		}
		catch (final URISyntaxException e)
		{
			throw new BlPatchImportException(String.format("Error importing patch <%s>", patchId), e);
		}
	}

	/**
	 * Import patch on path.
	 *
	 * @param pathToPatch
	 *           the path to patch
	 */
	@Override
	public void importPatchOnPath(final Path pathToPatch)
	{
		LOG.info(String.format("Importing patch <%s>", pathToPatch.getFileName()));
		importService.importData(new FileBasedImpExResource(pathToPatch.toFile(), "UTF-8"));
	}

	/**
	 * Import all patches.
	 */
	@Override
	public void importAllPatches()
	{
		final Collection<Path> paths = getAllPatches();
		for (final Path path : paths)
		{
			importPatchOnPath(path);
		}
	}

	/**
	 * Gets the all patches.
	 *
	 * @return the all patches
	 */
	@Override
	public TreeSet<Path> getAllPatches()
	{
		final String directory = getProperty(PROPERTY_PATH);
		final URL resource = BlpatchesSystemSetup.class.getResource(directory);
		final TreeSet<Path> paths = new TreeSet<>(new PatchComparator());
		try (DirectoryStream<Path> directoryStream = Files.newDirectoryStream(Paths.get(resource.toURI()), new PatchFilter()))
		{
			for (final Path path : directoryStream)
			{
				paths.add(path);
			}
		}
		catch (IOException | URISyntaxException e)
		{
			throw new BlPatchImportException(String.format("Error reading patch directory <%s>", directory), e);
		}
		return paths;
	}

	/**
	 * The Class PatchFilter.
	 */
	private class PatchFilter implements DirectoryStream.Filter<Path>
	{

		/**
		 * Accept.
		 *
		 * @param path
		 *           the path
		 * @return true, if successful
		 * @throws IOException
		 *            Signals that an I/O exception has occurred.
		 */
		@Override
		public boolean accept(final Path path) throws IOException
		{
			return Files.isRegularFile(path) && path.getFileName().toString().replace(BlpatchesConstants.PATCH_FILE_EXTENSION, "")
					.matches(getProperty(PROPERTY_ID_PATTERN));
		}
	}

	/**
	 * The Class PatchComparator.
	 */
	private class PatchComparator implements Comparator<Path>
	{

		/** The file name comparator. */
		private final Comparator<String> fileNameComparator = new AlphanumComparator();

		/**
		 * Compare.
		 *
		 * @param p1
		 *           the p 1
		 * @param p2
		 *           the p 2
		 * @return the int
		 */
		@Override
		public int compare(final Path p1, final Path p2)
		{
			return fileNameComparator.compare(p1.getFileName().toString(), p2.getFileName().toString());
		}
	}

	/**
	 * Gets the property.
	 *
	 * @param key
	 *           the key
	 * @return the property
	 */
	private String getProperty(final String key)
	{
		final String value = Config.getString(key, null);
		if (value == null)
		{
			throw new BlPatchImportException(String.format("Missing configuration for property <%s>", key));
		}
		return value;
	}

	/**
	 * Gets the media service.
	 *
	 * @return the media service
	 */
	public MediaService getMediaService()
	{
		return mediaService;
	}

	/**
	 * Sets the media service.
	 *
	 * @param mediaService
	 *           the new media service
	 */
	public void setMediaService(final MediaService mediaService)
	{
		this.mediaService = mediaService;
	}

	/**
	 * Gets the model service.
	 *
	 * @return the model service
	 */
	public ModelService getModelService()
	{
		return modelService;
	}

	/**
	 * Sets the model service.
	 *
	 * @param modelService
	 *           the new model service
	 */
	public void setModelService(final ModelService modelService)
	{
		this.modelService = modelService;
	}

	/**
	 * Gets the flexible search service.
	 *
	 * @return the flexible search service
	 */
	public FlexibleSearchService getFlexibleSearchService()
	{
		return flexibleSearchService;
	}

	/**
	 * Sets the flexible search service.
	 *
	 * @param flexibleSearchService
	 *           the new flexible search service
	 */
	public void setFlexibleSearchService(final FlexibleSearchService flexibleSearchService)
	{
		this.flexibleSearchService = flexibleSearchService;
	}

	/**
	 * Gets the import service.
	 *
	 * @return the import service
	 */
	public ImportService getImportService()
	{
		return importService;
	}

	/**
	 * Sets the import service.
	 *
	 * @param importService
	 *           the new import service
	 */
	public void setImportService(final ImportService importService)
	{
		this.importService = importService;
	}
}
