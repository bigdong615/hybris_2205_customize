package com.bl.core.search.solrfacetsearch.provider.impl;

import de.hybris.platform.solrfacetsearch.config.IndexConfig;
import de.hybris.platform.solrfacetsearch.config.IndexedProperty;
import de.hybris.platform.solrfacetsearch.provider.FieldNameProvider;
import de.hybris.platform.solrfacetsearch.provider.FieldValue;
import de.hybris.platform.solrfacetsearch.provider.FieldValueProvider;
import de.hybris.platform.solrfacetsearch.provider.impl.AbstractPropertyFieldValueProvider;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang.BooleanUtils;
import org.apache.log4j.Logger;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;

public class BlProductTypeValueProvider extends AbstractPropertyFieldValueProvider implements FieldValueProvider
{

	private static final Logger LOG = Logger.getLogger(BlProductTypeValueProvider.class);
	private FieldNameProvider fieldNameProvider;

	/**
	 * this method created for get field values in solr property
	 *
	 * @param indexConfig
	 *           indexConfig of solr
	 * @param indexedProperty
	 *           indexedProperty of solr
	 * @param model
	 *           defines product
	 * @return Collection<FieldValue> to solr
	 */
	int blProduct = 0;
	int blSerialProduct = 0;
	@Override
	public Collection<FieldValue> getFieldValues(final IndexConfig indexConfig, final IndexedProperty indexedProperty,
			final Object model)
	{
		return model instanceof BlProductModel
				? addFieldValues(new ArrayList<>(), indexedProperty,
						BooleanUtils.toBoolean(!(model instanceof BlSerialProductModel)))
				: Collections.emptyList();
	}

	/**
	 * this method created for adding field values in solr property
	 */
	private List<FieldValue> addFieldValues(final List<FieldValue> fieldValues, final IndexedProperty indexedProperty,
			final boolean value)
	{
		if (value)
		{
			blProduct++;
			LOG.info("********************blProduct*****************" + blProduct);
		}
		else
		{
			blSerialProduct++;
			LOG.info("********************blSerialProduct*****************" + blSerialProduct);
		}
		final Collection<String> fieldNames = getFieldNameProvider().getFieldNames(indexedProperty, null);
		for (final String fieldName : fieldNames)
		{
			fieldValues.add(new FieldValue(fieldName, value));
		}
		return fieldValues;
	}

	private FieldNameProvider getFieldNameProvider()
	{
		return fieldNameProvider;
	}

	public void setFieldNameProvider(final FieldNameProvider fieldNameProvider)
	{
		this.fieldNameProvider = fieldNameProvider;
	}

}
