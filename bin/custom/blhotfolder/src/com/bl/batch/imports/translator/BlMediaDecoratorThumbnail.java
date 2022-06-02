/*
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.batch.imports.translator;

import de.hybris.platform.impex.jalo.header.AbstractImpExCSVCellDecorator;

import java.util.Map;


public class BlMediaDecoratorThumbnail extends AbstractImpExCSVCellDecorator
{
	private static final String SPLIT_SIGN = ",";

	@Override
	public String decorate(final int position, final Map<Integer, String> srcLine)
	{
		final String name = srcLine.get(Integer.valueOf(position));

		return name.split(SPLIT_SIGN)[0] + "_Small";
	}

}