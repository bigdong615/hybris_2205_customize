/**
 *
 */
package com.bl.core.search.solrfacetsearch.provider.impl;

import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.solrfacetsearch.config.IndexConfig;
import de.hybris.platform.solrfacetsearch.config.IndexedProperty;
import de.hybris.platform.solrfacetsearch.provider.FieldNameProvider;
import de.hybris.platform.solrfacetsearch.provider.FieldValue;
import de.hybris.platform.solrfacetsearch.provider.FieldValueProvider;
import de.hybris.platform.solrfacetsearch.provider.impl.AbstractPropertyFieldValueProvider;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.commons.lang.BooleanUtils;

import com.bl.core.model.BlProductModel;


/**
 * @author Aditi Sharma
 *
 */
public class BlRentalGearDiscontinuedProductValueProvider extends AbstractPropertyFieldValueProvider implements FieldValueProvider
{

	private FieldNameProvider fieldNameProvider;

	/**
	 * this method created for creating values for solr property
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
		final Collection<FieldValue> fieldValues = new ArrayList<>();

		if (model instanceof ProductModel)
		{
			final BlProductModel product = (BlProductModel) model;
			fieldValues.addAll(createFieldValue(product, indexedProperty));
		}
		return fieldValues;
	}


	private List<FieldValue> createFieldValue(final BlProductModel product, final IndexedProperty indexedProperty)
	{
		final List<FieldValue> fieldValues = new ArrayList<>();

		addFieldValues(fieldValues, indexedProperty, BooleanUtils.toBoolean(product.getDiscontinued()));
		return fieldValues;
	}

	private void addFieldValues(final List<FieldValue> fieldValues, final IndexedProperty indexedProperty, final boolean value)
	{
		final Collection<String> fieldNames = getFieldNameProvider().getFieldNames(indexedProperty, null);
		for (final String fieldName : fieldNames)
		{
			fieldValues.add(new FieldValue(fieldName, value));
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
