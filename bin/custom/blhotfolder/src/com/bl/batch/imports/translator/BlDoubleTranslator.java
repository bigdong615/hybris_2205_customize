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
public class BlDoubleTranslator extends AbstractValueTranslator
{

	private static final Logger LOG = Logger.getLogger(BlDoubleTranslator.class);
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
				final double doubleValue = Double.parseDouble(value);
				return doubleValue;
			}
		}
		catch (final Exception e)
		{
			LOG.error("Unable to convert Double " + e.getMessage());
		}
		return Double.valueOf(0.0D);
	}

}
