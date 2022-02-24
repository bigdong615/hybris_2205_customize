package com.bl.batch.imports.translator;

import de.hybris.platform.impex.jalo.translators.AbstractValueTranslator;
import de.hybris.platform.jalo.Item;
import de.hybris.platform.jalo.JaloInvalidParameterException;

import org.apache.commons.lang.StringUtils;

public class BlBooleanTranslator extends AbstractValueTranslator
{
	private static final String TRUE = "1";
	private static final String FALSE = "0";

	@Override
	public String exportValue(final Object value) throws JaloInvalidParameterException
	{
		return value == null ? "" : value.toString();
	}

	@Override
	public Object importValue(final String value, final Item item) throws JaloInvalidParameterException
	{
		if(StringUtils.isNotEmpty(value)) {
			if (value.equalsIgnoreCase(TRUE))
			{
				return Boolean.TRUE;
			}
			else if (value.equalsIgnoreCase(FALSE))
			{
				return Boolean.FALSE;
			}
		}
		return null;
	}

}
