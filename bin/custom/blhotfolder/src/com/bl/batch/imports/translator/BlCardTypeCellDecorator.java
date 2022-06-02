package com.bl.batch.imports.translator;

import de.hybris.platform.util.CSVCellDecorator;

import java.util.Map;

import com.braintree.enums.BrainTreeCardType;


public class BlCardTypeCellDecorator implements CSVCellDecorator
{

	@Override
	public String decorate(final int position, final Map<Integer, String> impexLine)
	{
		final String cardTypeCode = impexLine.get(Integer.valueOf(position));

		if (cardTypeCode != null)
		{
			return BrainTreeCardType.valueOf(cardTypeCode).getCode();
		}
		return "";
	}
}
