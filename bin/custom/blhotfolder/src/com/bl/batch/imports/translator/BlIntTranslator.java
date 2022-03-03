/*
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.batch.imports.translator;

import de.hybris.platform.impex.jalo.translators.AbstractValueTranslator;
import de.hybris.platform.jalo.Item;
import de.hybris.platform.jalo.JaloInvalidParameterException;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;


/**
 *
 */
public class BlIntTranslator extends AbstractValueTranslator
{

	private static final Logger LOG = Logger.getLogger(BlIntTranslator.class);
	@Override
	public String exportValue(final Object value) throws JaloInvalidParameterException
	{
		return value == null ? "" : value.toString();
	}

	@Override
	public Object importValue(final String value, final Item item) throws JaloInvalidParameterException
	{
		try
		{
			if (StringUtils.isNotEmpty(value))
			{
				final int intValue = Integer.parseInt(value);
				return intValue;
			}
		}
		catch (final Exception e)
		{
			LOG.error("Unable to convert Int" + e.getMessage());
		}
		return 0;
	}

}
