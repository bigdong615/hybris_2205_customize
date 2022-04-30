/*
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.batch.imports.translator;

import de.hybris.platform.core.PK;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.impex.jalo.translators.AbstractValueTranslator;
import de.hybris.platform.jalo.Item;
import de.hybris.platform.jalo.JaloInvalidParameterException;

import java.util.List;

import com.bl.core.product.dao.impl.DefaultBlProductDao;


/**
 *
 */
public class BlOnlineProductsTranslator extends AbstractValueTranslator
{
	private static final DefaultBlProductDao blProductDao;

	static
	{
		blProductDao = (DefaultBlProductDao) getServiceBean("defaultBlProductDao");
	}

	@Override
	public String exportValue(final Object value) throws JaloInvalidParameterException
	{
		return value == null ? "" : value.toString();
	}

	@Override
	public Object importValue(final String value, final Item item) throws JaloInvalidParameterException
	{
		final List<ProductModel> products = blProductDao.findProductsByCode(value);
		PK productPk = null;
		for (final ProductModel product : products)
		{
			if (product.getCatalogVersion().getVersion().equalsIgnoreCase("Online"))
			{

				productPk = product.getPk();
			}
		}
		return productPk;
	}

	private static Object getServiceBean(final String beanID)
	{
		return Registry.getApplicationContext().getBean(beanID);
	}
}
