package com.bl.core.search.solrfacetsearch.provider.impl;

import de.hybris.platform.core.HybrisEnumValue;
import de.hybris.platform.servicelayer.model.ModelService;
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


public class BlProductTypeExclusionValueProvider extends AbstractPropertyFieldValueProvider implements FieldValueProvider
{

	private static final Logger LOG = Logger.getLogger(BlProductTypeExclusionValueProvider.class);
	private FieldNameProvider fieldNameProvider;
	private ModelService modelService;

	@Override
	public Collection<FieldValue> getFieldValues(final IndexConfig indexConfig, final IndexedProperty indexedProperty,
			final Object model)
	{
		return addFieldValues(new ArrayList<>(), indexedProperty, getProductTypeExclusion(model));
	}

	/**
	 * @param model
	 * @param indexedProperty
	 * @return
	 */
	private boolean getProductTypeExclusion(final Object model)
	{
		final Object modelProperty = modelService.getAttributeValue(model, "productType");


		if (isHybrisEnum(modelProperty))
		{
			final HybrisEnumValue hybrisEnum = (HybrisEnumValue) modelProperty;
			if (getEnumValue(hybrisEnum).equalsIgnoreCase("SUBPARTS") || getEnumValue(hybrisEnum).equalsIgnoreCase("CONSUMABLES"))
			{
				return true;
			}
		}
		else
		{
			LOG.warn("Resolving value for IndexedProperty: {} has failed because it's not a HybrisEnumValue");
		}
		return false;
	}


	/**
	 * this method created for adding field values in solr property
	 */
	private List<FieldValue> addFieldValues(final List<FieldValue> fieldValues, final IndexedProperty indexedProperty,
			final boolean value)
	{
		final Collection<String> fieldNames = getFieldNameProvider().getFieldNames(indexedProperty, null);
		for (final String fieldName : fieldNames)
		{
			fieldValues.add(new FieldValue(fieldName, value));
		}
		return fieldValues;
	}


	private String getEnumValue(final HybrisEnumValue hybrisEnum)
	{
		return hybrisEnum.getCode();
	}

	private boolean isHybrisEnum(final Object modelProperty)
	{
		return modelProperty instanceof HybrisEnumValue;
	}

	private FieldNameProvider getFieldNameProvider()
	{
		return fieldNameProvider;
	}

	public void setFieldNameProvider(final FieldNameProvider fieldNameProvider)
	{
		this.fieldNameProvider = fieldNameProvider;
	}

	public ModelService getModelService()
	{
		return modelService;
	}

	@Override
	public void setModelService(final ModelService modelService)
	{
		this.modelService = modelService;
	}

}
