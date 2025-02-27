package com.bl.core.search.solrfacetsearch.provider.impl;

import de.hybris.platform.solrfacetsearch.config.IndexConfig;
import de.hybris.platform.solrfacetsearch.config.IndexedProperty;
import de.hybris.platform.solrfacetsearch.provider.FieldNameProvider;
import de.hybris.platform.solrfacetsearch.provider.FieldValue;
import de.hybris.platform.solrfacetsearch.provider.FieldValueProvider;
import de.hybris.platform.solrfacetsearch.provider.impl.AbstractPropertyFieldValueProvider;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.log4j.Logger;

import com.bl.core.model.BlProductModel;


public class BlProductSearchTermValueProvider extends AbstractPropertyFieldValueProvider implements FieldValueProvider
{

	private static final Logger LOG = Logger.getLogger(BlProductSearchTermValueProvider.class);
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
	@Override
	public Collection<FieldValue> getFieldValues(final IndexConfig indexConfig, final IndexedProperty indexedProperty,
			final Object model)
	{

		return addFieldValues(new ArrayList<>(), indexedProperty,
				((BlProductModel) model).getName() + " " + extractProductCode(((BlProductModel) model).getName()));

	}

	/**
	 * this method created for adding field values in solr property
	 */
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

	private String extractProductCode(final String input)
	{
		if (input != null)
		{
			//return input.replaceAll("[^\\d-.]", " ").replaceAll("\\s+", " ").trim();
			return input.replaceAll("[A-Za-z]+|([^-\\d.])|(-)(?=[A-Za-z])", " ").replaceAll("\\s+", " ").trim().replaceAll("^-|-$",
					"");
		}
		else
		{
			return null;
		}
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
