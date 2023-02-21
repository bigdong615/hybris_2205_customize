/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.core.search.solrfacetsearch.provider.impl;

import de.hybris.platform.solrfacetsearch.config.IndexConfig;
import de.hybris.platform.solrfacetsearch.config.IndexedProperty;
import de.hybris.platform.solrfacetsearch.config.exceptions.FieldValueProviderException;
import de.hybris.platform.solrfacetsearch.provider.FieldNameProvider;
import de.hybris.platform.solrfacetsearch.provider.FieldValue;
import de.hybris.platform.solrfacetsearch.provider.FieldValueProvider;
import de.hybris.platform.solrfacetsearch.provider.impl.AbstractPropertyFieldValueProvider;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Required;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;


/**
 * This ValueProvider will provide the product's image url for the first gallery image that supports the requested media
 * format.
 */
public class BigDecimalValueProvider extends AbstractPropertyFieldValueProvider implements FieldValueProvider
{
	private static final Logger LOG = Logger.getLogger(BigDecimalValueProvider.class);

	private String field;
	private FieldNameProvider fieldNameProvider;


	protected String getField()
	{
		return field;
	}

	@Required
	public void setField(final String field)
	{
		this.field = field;
	}

	protected FieldNameProvider getFieldNameProvider()
	{
		return fieldNameProvider;
	}

	@Required
	public void setFieldNameProvider(final FieldNameProvider fieldNameProvider)
	{
		this.fieldNameProvider = fieldNameProvider;
	}

	@Override
	public Collection<FieldValue> getFieldValues(final IndexConfig indexConfig, final IndexedProperty indexedProperty,
			final Object model) throws FieldValueProviderException
	{
		if (model instanceof BlProductModel)
		{
			final BlProductModel product = (BlProductModel) model;
			if (getField().equalsIgnoreCase("forSaleBasePrice") && product.getForSaleBasePrice() != null)
			{
				return addFieldValues(new ArrayList<>(), indexedProperty, product.getForSaleBasePrice().toString());
			}
			if (getField().equalsIgnoreCase("retailGearPrice") && product.getRetailGearPrice() != null)
			{
				return addFieldValues(new ArrayList<>(), indexedProperty, product.getRetailGearPrice().toString());
			}
			if (model instanceof BlSerialProductModel)
			{
				final BlSerialProductModel serial = (BlSerialProductModel) model;
				if (getField().equalsIgnoreCase("finalSalePrice") && serial.getFinalSalePrice() != null)
				{
					return addFieldValues(new ArrayList<>(), indexedProperty, serial.getFinalSalePrice().toString());
				}
				if (getField().equalsIgnoreCase("incentivizedPrice") && serial.getIncentivizedPrice() != null)
				{
					return addFieldValues(new ArrayList<>(), indexedProperty, serial.getIncentivizedPrice().toString());
				}
			}
			if (getField().equalsIgnoreCase("procurementCost") && product.getProcurementCost() != null)
			{
				return addFieldValues(new ArrayList<>(), indexedProperty, product.getProcurementCost().toString());
			}


			if (getField().equalsIgnoreCase("weight") && product.getWeight() != null)
			{
				return addFieldValues(new ArrayList<>(), indexedProperty, product.getWeight().toString());
			}

		}
		return Collections.emptyList();
	}

	private List<FieldValue> addFieldValues(final List<FieldValue> fieldValues, final IndexedProperty indexedProperty,
			final String value)
	{
		final Collection<String> fieldNames = getFieldNameProvider().getFieldNames(indexedProperty, null);
		for (final String fieldName : fieldNames)
		{
			fieldValues.add(new FieldValue(fieldName, value));
		}
		return fieldValues;
	}

}
