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
import com.bl.core.model.ProductVideoModel;


/**
 * This ValueProvider will provide the product's image url for the first gallery image that supports the requested media
 * format.
 */
public class ModelToStringValueProvider extends AbstractPropertyFieldValueProvider implements FieldValueProvider
{
	private static final Logger LOG = Logger.getLogger(ModelToStringValueProvider.class);

	private String model;
	private FieldNameProvider fieldNameProvider;


	protected String getModel()
	{
		return model;
	}

	@Required
	public void setModel(final String model)
	{
		this.model = model;
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
			if (getModel().equalsIgnoreCase("unit") && product.getUnit() != null)
			{
				return addFieldValues(new ArrayList<>(), indexedProperty, product.getUnit().getCode());
			}
			if (getModel().equalsIgnoreCase("catalogVersion") && product.getCatalogVersion() != null)
			{
				return addFieldValues(new ArrayList<>(), indexedProperty, product.getCatalogVersion().getVersion());
			}
			if (getModel().equalsIgnoreCase("contentUnit") && product.getContentUnit() != null)
			{
				return addFieldValues(new ArrayList<>(), indexedProperty, product.getContentUnit().getCode());
			}
			if (getModel().equalsIgnoreCase("productOrderLimit") && product.getProductOrderLimit() != null)
			{
				return addFieldValues(new ArrayList<>(), indexedProperty, product.getProductOrderLimit().getCode());
			}
			if (getModel().equalsIgnoreCase("rentalVideosLink") && product.getRentalVideosLink() != null
					&& !product.getRentalVideosLink().isEmpty())
			{
				final List<String> videos = new ArrayList<String>();
				for (final ProductVideoModel video : product.getRentalVideosLink())
				{
					videos.add(video.getVideoLink());
				}
				return addFieldValues(new ArrayList<>(), indexedProperty, String.join(",", videos));
			}
			if (getModel().equalsIgnoreCase("usedGearVideosLink") && product.getUsedGearVideosLink() != null
					&& !product.getUsedGearVideosLink().isEmpty())
			{
				final List<String> videos = new ArrayList<String>();
				for (final ProductVideoModel video : product.getUsedGearVideosLink())
				{
					videos.add(video.getVideoLink());
				}
				return addFieldValues(new ArrayList<>(), indexedProperty, String.join(",", videos));
			}

			if (model instanceof BlSerialProductModel)
			{
				final BlSerialProductModel serial = (BlSerialProductModel) model;
				if (getModel().equalsIgnoreCase("trackingNumber") && !serial.getTrackingNumber().isEmpty())
				{
					return addFieldValues(new ArrayList<>(), indexedProperty, String.join(",", serial.getTrackingNumber()));
				}
				if (getModel().equalsIgnoreCase("warehouseLocation") && serial.getWarehouseLocation() != null)
				{
					return addFieldValues(new ArrayList<>(), indexedProperty, serial.getWarehouseLocation().getCode());
				}
				if (getModel().equalsIgnoreCase("serialHomeLocation") && serial.getSerialHomeLocation() != null)
				{
					return addFieldValues(new ArrayList<>(), indexedProperty, serial.getSerialHomeLocation().getInventoryLocationID());
				}
				if (getModel().equalsIgnoreCase("ocLocationDetails") && serial.getOcLocationDetails() != null)
				{
					return addFieldValues(new ArrayList<>(), indexedProperty, serial.getOcLocationDetails().getInventoryLocationID());
				}
				if (getModel().equalsIgnoreCase("associatedConsignment") && serial.getAssociatedConsignment() != null)
				{
					return addFieldValues(new ArrayList<>(), indexedProperty, serial.getAssociatedConsignment().getCode());
				}
				if (getModel().equalsIgnoreCase("associatedOrder") && serial.getAssociatedOrder() != null)
				{
					return addFieldValues(new ArrayList<>(), indexedProperty, serial.getAssociatedOrder().getCode());
				}
				if (getModel().equalsIgnoreCase("associatedShippedConsignment") && serial.getAssociatedShippedConsignment() != null)
				{
					return addFieldValues(new ArrayList<>(), indexedProperty, serial.getAssociatedShippedConsignment().getCode());
				}
				if (getModel().equalsIgnoreCase("lastUnboxedOcLocationHistory") && serial.getLastUnboxedOcLocationHistory() != null)
				{
					return addFieldValues(new ArrayList<>(), indexedProperty,
							serial.getLastUnboxedOcLocationHistory().getBlInventoryLocation().getInventoryLocationID());
				}
				if (getModel().equalsIgnoreCase("blProduct") && serial.getBlProduct() != null)
				{
					return addFieldValues(new ArrayList<>(), indexedProperty, serial.getBlProduct().getCode());
				}
				if (getModel().equalsIgnoreCase("associatedUsedGearConsignment") && serial.getAssociatedUsedGearConsignment() != null)
				{
					return addFieldValues(new ArrayList<>(), indexedProperty, serial.getAssociatedUsedGearConsignment().getCode());
				}
				if (getModel().equalsIgnoreCase("consignmentEntry") && serial.getConsignmentEntry() != null)
				{
					return addFieldValues(new ArrayList<>(), indexedProperty, serial.getConsignmentEntry().getConsignment().getCode());
				}
				if (getModel().equalsIgnoreCase("associatedUsedGearOrder") && serial.getAssociatedUsedGearOrder() != null)
				{
					return addFieldValues(new ArrayList<>(), indexedProperty, serial.getAssociatedUsedGearOrder().getCode());
				}








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
