/*
 * Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.integration;

import org.apache.log4j.Logger;


/**
 * Simple test class to demonstrate how to include utility classes to your webmodule.
 */
public class BlintegrationWebHelper
{
	/** Edit the local|project.properties to change logging behavior (properties log4j.*). */
	@SuppressWarnings("unused")
	private static final Logger LOG = Logger.getLogger(BlintegrationWebHelper.class.getName());

	private BlintegrationWebHelper()
	{
	}

	public static final String getTestOutput()
	{
		return "testoutput";
	}
}
