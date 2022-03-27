package com.bl.batch.imports.translator;

import de.hybris.platform.util.CSVCellDecorator;

import java.util.Map;

import com.bl.core.enums.NumberingSystemEnum;


public class BlNumberingSystemCellDecorator implements CSVCellDecorator
{

	@Override
	public String decorate(final int position, final Map<Integer, String> impexLine)
	{
		final String numberSystemCode = impexLine.get(Integer.valueOf(position));

		if (numberSystemCode != null)
		{
			return NumberingSystemEnum.valueOf(numberSystemCode).getCode();
		}
		return "";

	}
}
