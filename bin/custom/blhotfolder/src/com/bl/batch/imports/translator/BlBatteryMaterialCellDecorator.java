package com.bl.batch.imports.translator;

import de.hybris.platform.util.CSVCellDecorator;

import java.util.Map;

import com.bl.core.enums.BatteryMaterialEnum;


public class BlBatteryMaterialCellDecorator implements CSVCellDecorator
{

	@Override
	public String decorate(final int position, final Map<Integer, String> impexLine)
	{
		final String batteryMaterialCode = impexLine.get(Integer.valueOf(position));

		if (batteryMaterialCode != null)
		{
			return BatteryMaterialEnum.valueOf(batteryMaterialCode).getCode();
		}
		return "";
	}
}
