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
 * @author Manikandan This Class is created for indexing upcomingProduct Value to solr
 */

public class BlUpComingProductValueProvider extends AbstractPropertyFieldValueProvider implements
    FieldValueProvider {

  private FieldNameProvider fieldNameProvider;

  /**
   * @param indexConfig     indexConfig for solr
   * @param indexedProperty indexed property for solr
   * @param model           defines product
   * @return Collection<FieldValue> to solr
   */
  @Override
  public Collection<FieldValue> getFieldValues(final IndexConfig indexConfig,
      final IndexedProperty indexedProperty, final Object model) {

    if (model instanceof BlProductModel) {
      return addFieldValues(new ArrayList<>(), indexedProperty,
          ((BlProductModel) model).getSerialProducts().stream()
              .anyMatch(BlProductModel::getForRent) && ((BlProductModel) model).getForRent()
              ? Boolean.FALSE : Boolean.TRUE);

    }
    return Collections.emptyList();
  }

  /**
   * This method is created to add the filed values to be index
   *
   * @param fieldValues     list of values
   * @param indexedProperty indexedproperty for solr
   * @param value           determines the upcoming boolean
   */
  private List<FieldValue> addFieldValues(final List<FieldValue> fieldValues,
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
