/*
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.batch.imports.translator;

import de.hybris.platform.core.Registry;
import de.hybris.platform.impex.jalo.translators.AbstractValueTranslator;
import de.hybris.platform.jalo.Item;
import de.hybris.platform.jalo.JaloInvalidParameterException;

import org.apache.commons.lang.StringUtils;

import com.bl.core.dao.BlAddressDao;


/**
 *
 */
public class BlItemChargesTranslator extends AbstractValueTranslator
{

	private static final BlAddressDao blAddressDao;

	static
	{
		blAddressDao = (BlAddressDao) getServiceBean("blAddressDao");
	}

	@Override
	public String exportValue(final Object value) throws JaloInvalidParameterException
	{
		return value == null ? "" : value.toString();
	}

	@Override
	public Object importValue(final String value, final Item item) throws JaloInvalidParameterException
	{
		if (StringUtils.isNotEmpty(value))
		{
			final String[] chargeCodes = value.split(",");
			return blAddressDao.getBillChargeList(chargeCodes);
		}
		return null;
	}

	/**
	 * Get the bean instance from application context
	 *
	 * @param beanID
	 * @return Bean instance
	 */
	private static Object getServiceBean(final String beanID)
	{
		return Registry.getApplicationContext().getBean(beanID);
	}

}
