/*
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.batch.imports.translator;

import de.hybris.platform.core.Registry;
import de.hybris.platform.impex.jalo.translators.AbstractValueTranslator;
import de.hybris.platform.jalo.Item;
import de.hybris.platform.jalo.JaloInvalidParameterException;

import com.bl.core.product.dao.impl.DefaultBlProductDao;


/**
 *
 */
public class BlSubpartsToProductsTranslator extends AbstractValueTranslator
{
	private static final DefaultBlProductDao blProductDao;

	static
	{
		blProductDao = (DefaultBlProductDao) getServiceBean("defaultBlProductDao");
	}

	private static final String SPLIT_SIGN = ":";

	@Override
	public String exportValue(final Object value) throws JaloInvalidParameterException
	{
		return value == null ? "" : value.toString();
	}

	@Override
	public Object importValue(final String value, final Item item) throws JaloInvalidParameterException
	{
		final String productCode = value.split(SPLIT_SIGN)[0];
		final int quantity = Integer.parseInt(value.split(SPLIT_SIGN)[1]);
		return blProductDao.getBlSubPartsPk(productCode, quantity).getPk();
	}

	private static Object getServiceBean(final String beanID)
	{
		return Registry.getApplicationContext().getBean(beanID);
	}
}
