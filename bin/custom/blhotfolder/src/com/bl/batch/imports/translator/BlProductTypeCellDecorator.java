package com.bl.batch.imports.translator;

import de.hybris.platform.util.CSVCellDecorator;

import java.util.Map;

import com.bl.core.enums.ProductTypeEnum;


public class BlProductTypeCellDecorator implements CSVCellDecorator
{

	@Override
	public String decorate(final int position, final Map<Integer, String> impexLine)
	{
		final String productTypeCode = impexLine.get(Integer.valueOf(position));

		if (productTypeCode != null)
		{
			return ProductTypeEnum.valueOf(productTypeCode).getCode();
		}
		return "";
	}
}
