package com.bl.batch.imports.translator;

import de.hybris.platform.util.CSVCellDecorator;

import java.util.Map;

import com.bl.core.enums.OptionTypeEnum;


public class BlOptionTypeCellDecorator implements CSVCellDecorator
{

	@Override
	public String decorate(final int position, final Map<Integer, String> impexLine)
	{
		final String optionsType = impexLine.get(Integer.valueOf(position));

		if (optionsType != null)
		{

			return OptionTypeEnum.valueOf(optionsType).getCode();
		}
		return "";
	}
}
