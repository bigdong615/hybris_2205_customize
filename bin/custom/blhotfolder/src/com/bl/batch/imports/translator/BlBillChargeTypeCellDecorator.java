package com.bl.batch.imports.translator;

import de.hybris.platform.util.CSVCellDecorator;

import java.util.Map;

import com.bl.core.enums.ItemBillingChargeTypeEnum;


public class BlBillChargeTypeCellDecorator implements CSVCellDecorator
{

	@Override
	public String decorate(final int position, final Map<Integer, String> impexLine)
	{
		final String billChargeTypeCode = impexLine.get(Integer.valueOf(position));

		if(billChargeTypeCode != null) {

			return ItemBillingChargeTypeEnum.valueOf(billChargeTypeCode).getCode();
		}
		return "";
	}
}
