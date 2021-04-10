package com.bl.core.search.solrfacetsearch.provider.impl;

import com.bl.core.model.BlProductModel;
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

/**
 * @author Manikandan This Value Provider is created to send forSale values to solr
 */
public class BlUsedGearProductValueProvider extends AbstractPropertyFieldValueProvider implements
    FieldValueProvider {

  private FieldNameProvider fieldNameProvider;

  /**
   * this method created for creating values for solr property
   *
   * @param indexConfig     indexConfig of solr
   * @param indexedProperty indexedProperty of solr
   * @param model           defines product
   * @return Collection<FieldValue> to solr
   */
  @Override
  public Collection<FieldValue> getFieldValues(final IndexConfig indexConfig,
      final IndexedProperty indexedProperty, final Object model) {

    if (model instanceof BlProductModel) {
      return addFieldValues(new ArrayList<>(), indexedProperty,
          ((BlProductModel) model).getSerialProducts().stream()
              .anyMatch(BlProductModel::getForSale) && ((BlProductModel) model).getForSale()
              ? Boolean.TRUE : Boolean.FALSE);
    }
    return Collections.emptyList();
  }

  /**
   * this method is created for adding field values to solr
   *
   * @param fieldValues     field value for solr
   * @param indexedProperty indexedProperty for solr
   * @param value           defines forSale attribute value
   * @return
   */
  private Collection<FieldValue> addFieldValues(final List<FieldValue> fieldValues,
      final IndexedProperty indexedProperty, final boolean value) {
    final Collection<String> fieldNames = getFieldNameProvider()
        .getFieldNames(indexedProperty, null);
    for (final String fieldName : fieldNames) {
      fieldValues.add(new FieldValue(fieldName, value));
    }
    return fieldValues;
  }

  private FieldNameProvider getFieldNameProvider() {
    return fieldNameProvider;
  }

  public void setFieldNameProvider(
      FieldNameProvider fieldNameProvider) {
    this.fieldNameProvider = fieldNameProvider;
  }


}
