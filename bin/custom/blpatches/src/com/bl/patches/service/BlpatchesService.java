/*
 * Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.patches.service;

import java.nio.file.Path;
import java.util.TreeSet;

import com.bl.patches.exception.BlPatchImportException;


/**
 * The Interface BlpatchesService.
 * <p>
 * Main service facilitating the import of patches. Requires the following configuration properties to be set:
 * <dl>
 * <dt><i>EXTENSIONNAME</i>.import.path</dt>
 * <dd>The path where the patches reside. This must be an absolute resource on the classpath of the
 * {@link com.bl.patches.setup.BlpatchesSystemSetup}, e.g. <code>/<i>projectname</i>patches/import</code>.</dd>
 * <dt><i>EXTENSIONNAME</i>.import.id.pattern</dt>
 * <dd>This RegExp pattern is used to find patches on the file system and validate patch IDs. Usually patches should be
 * named after the release they're deployed with, e.g. <code><i>projectname</i>-1.0.1</code>.</dd>
 * </dl>
 *
 */
public interface BlpatchesService
{

	/**
	 * Gets the hybris logo url.
	 *
	 * @param logoCode
	 *           the logo code
	 * @return the hybris logo url
	 */
	String getHybrisLogoUrl(String logoCode);

	/**
	 * Creates the logo.
	 *
	 * @param logoCode
	 *           the logo code
	 */
	void createLogo(String logoCode);

	/**
	 * Import the patch with the given <code>patchId</code>. The method internally calls
	 * {@link #importPatchOnPath(Path)}.
	 *
	 * @param patchId
	 *           The patch ID
	 * @throws BlPatchImportException
	 *            The patch ID is invalid or the corresponding file was not found
	 */
	void importPatchWithId(String patchId);

	/**
	 * Import the patch file on the given path. The path can point to any file on the filesystem, it does NOT need to lie
	 * on the configured path and does NOT need to follow the configured naming convention.
	 */
	void importPatchOnPath(Path pathToPatch);

	/**
	 * Import all files on the configured path matching the configured ID pattern. The method internally calls
	 * {@link #getAllPatches()} and {@link #importPatchOnPath(Path)}.
	 *
	 * @throws BlPatchImportException
	 *            The configured path cannot be read
	 */
	void importAllPatches();

	/**
	 * Get the paths to all files on the configured path matching the configured ID pattern. The files are sorted with an
	 * alphanumeric algorithm (taking care of the 1,2,..,10 problem) in ascending order.
	 *
	 * @return A sorted set of paths to all patch files
	 * @throws BlPatchImportException
	 *            The configured path cannot be read
	 */
	TreeSet<Path> getAllPatches();
}
