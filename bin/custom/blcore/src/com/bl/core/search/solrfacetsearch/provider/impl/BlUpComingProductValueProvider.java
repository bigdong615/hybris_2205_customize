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
import java.util.Collections;
import java.util.List;

/**
 * @author Manikandan
 *This Class is created for indexing upcomingProduct Value to solr
 */

public class BlUpComingProductValueProvider extends AbstractPropertyFieldValueProvider implements
    FieldValueProvider {

  private FieldNameProvider fieldNameProvider;

  /**
   *
   * @param indexConfig indexConfig for solr
   * @param indexedProperty indexed property for solr
   * @param model defines product
   * @return Collection<FieldValue> to solr
   */
  @Override
  public Collection<FieldValue> getFieldValues(final IndexConfig indexConfig,
      final IndexedProperty indexedProperty, final Object model) {

    if (model instanceof BlProductModel) {
      return createFieldValue((BlProductModel) model, indexedProperty);
    }
    return Collections.emptyList();
  }

  /**
   * This method is created for getting upcoming products to solr
   * @param product defines product
   * @param indexedProperty indexedproperty of solr
   * @retur  List<FieldValue> to be index to solr
   */
  private List<FieldValue> createFieldValue(final BlProductModel product, final IndexedProperty indexedProperty)
  {
    final List<FieldValue> fieldValues = new ArrayList<>();
    boolean upComing = true;
    // Condition added to check current blproduct and their respective serial products  is forrent
    if(product.getSerialProducts().stream().anyMatch(BlProductModel::getForRent) && product.getForRent()) {
      upComing = false;
    }
    addFieldValues(fieldValues, indexedProperty, upComing);
    return fieldValues;
  }

  /**
   *
   * @param fieldValues list of values
   * @param indexedProperty indexedproperty for solr
   * @param value determines the upcoming boolean
   */
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
