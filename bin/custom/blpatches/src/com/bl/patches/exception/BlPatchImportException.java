/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.patches.exception;

/**
 * The Class BlPatchImportException.
 */
public class BlPatchImportException extends RuntimeException
{

	/**
	 * Instantiates a new bl patch import exception.
	 *
	 * @param message
	 *           the message
	 */
	public BlPatchImportException(final String message)
	{
		super(message);
	}

	/**
	 * Instantiates a new bl patch import exception.
	 *
	 * @param message
	 *           the message
	 * @param cause
	 *           the cause
	 */
	public BlPatchImportException(final String message, final Throwable cause)
	{
		super(message, cause);
	}
}
