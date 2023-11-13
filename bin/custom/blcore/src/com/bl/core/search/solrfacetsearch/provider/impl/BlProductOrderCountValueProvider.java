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
import com.bl.core.product.dao.impl.DefaultBlProductDao;


public class BlProductOrderCountValueProvider extends AbstractPropertyFieldValueProvider implements FieldValueProvider
{
	private static final Logger LOG = Logger.getLogger(BlProductOrderCountValueProvider.class);

	private DefaultBlProductDao defaultBlProductDao;

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

		return addFieldValues(new ArrayList<>(), indexedProperty, extractProductOrderCount(((BlProductModel) model).getCode()));

	}

	/**
	 * this method created for adding field values in solr property
	 */
	private List<FieldValue> addFieldValues(final List<FieldValue> fieldValues, final IndexedProperty indexedProperty,
			final double value)
	{
		final Collection<String> fieldNames = getFieldNameProvider().getFieldNames(indexedProperty, null);
		for (final String fieldName : fieldNames)
		{
			fieldValues.add(new FieldValue(fieldName, value));
		}
		return fieldValues;
	}

	private double extractProductOrderCount(final String input)
	{
		if (input != null)
		{
			final double count = defaultBlProductDao.getOrderCountByProduct(input);
			return count;
		}
		else
		{
			return 0;
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

	/**
	 * @return the defaultBlProductDao
	 */
	public DefaultBlProductDao getDefaultBlProductDao()
	{
		return defaultBlProductDao;
	}

	/**
	 * @param defaultBlProductDao
	 *           the defaultBlProductDao to set
	 */
	public void setDefaultBlProductDao(final DefaultBlProductDao defaultBlProductDao)
	{
		this.defaultBlProductDao = defaultBlProductDao;
	}


}
