package com.bl.core.search.solrfacetsearch.provider.impl;

import com.bl.core.model.BlProductModel;
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

/**
 * @author Manikandan
 *
 * This Class is created for indexing upcomingProduct Value to solr
 */
public class BlUpComingProductValueProvider extends AbstractPropertyFieldValueProvider implements
    FieldValueProvider {

  private FieldNameProvider fieldNameProvider;

  @Override
  public Collection<FieldValue> getFieldValues(final IndexConfig indexConfig,
      final IndexedProperty indexedProperty, final Object model) {
    final Collection<FieldValue> fieldValues = new ArrayList<>();

    if (model instanceof ProductModel) {
      final BlProductModel product = (BlProductModel) model;
      fieldValues.addAll(createFieldValue(product, indexedProperty));
    }
    return fieldValues;
  }

  /**
   * This Method is created for checking upcomingProducts
   */
  private List<FieldValue> createFieldValue(final BlProductModel product, final IndexedProperty indexedProperty)
  {
    final List<FieldValue> fieldValues = new ArrayList<>();
    boolean upComing = true;
    // Condition added for Blproducts and their respective SerialProducts
    if(product.getSerialProducts().stream().anyMatch(BlProductModel::getForRent) && product.getForRent()) {
      upComing = false;
    }
    addFieldValues(fieldValues, indexedProperty, upComing);
    return fieldValues;
  }

  private void addFieldValues(final List<FieldValue> fieldValues,
      final IndexedProperty indexedProperty, final boolean value)
  {
    final Collection<String> fieldNames = getFieldNameProvider().getFieldNames(indexedProperty, null);
    for (final String fieldName : fieldNames)
    {
      fieldValues.add(new FieldValue(fieldName, value));
    }
  }

  private FieldNameProvider getFieldNameProvider() {
    return fieldNameProvider;
  }

  public void setFieldNameProvider(
      FieldNameProvider fieldNameProvider) {
    this.fieldNameProvider = fieldNameProvider;
  }

}
