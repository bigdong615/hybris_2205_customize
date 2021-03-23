package com.bl.core.search.solrfacetsearch.provider.impl;

import com.bl.core.constants.BlCoreConstants;
import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.commerceservices.search.solrfacetsearch.provider.CategorySource;
import de.hybris.platform.core.model.c2l.LanguageModel;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.solrfacetsearch.config.IndexConfig;
import de.hybris.platform.solrfacetsearch.config.IndexedProperty;
import de.hybris.platform.solrfacetsearch.config.exceptions.FieldValueProviderException;
import de.hybris.platform.solrfacetsearch.provider.FieldNameProvider;
import de.hybris.platform.solrfacetsearch.provider.FieldValue;
import de.hybris.platform.solrfacetsearch.provider.FieldValueProvider;
import de.hybris.platform.solrfacetsearch.provider.impl.AbstractPropertyFieldValueProvider;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import org.apache.commons.collections.CollectionUtils;

  /**
  *
  * Class is created for providing Global facet values to the solr .
  *
  * @author ManiKandan
  */

public class BlGlobalFacetValueProvider extends AbstractPropertyFieldValueProvider implements
    FieldValueProvider {

    private static final List<String> CATEGORY_LIST = Arrays.asList("lenses", "cameras", "production");

    private CategorySource categorySource;
    private FieldNameProvider fieldNameProvider;
    private CommonI18NService commonI18NService;


   /*
   * This Method check for the categories and add that into result
   */

  @Override
  public Collection<FieldValue> getFieldValues(final IndexConfig indexConfig, final IndexedProperty indexedProperty,
      final Object model) throws FieldValueProviderException
  {
    final Collection<CategoryModel> categories = getCategorySource().getCategoriesForConfigAndProperty(indexConfig,
        indexedProperty, model);
    if (CollectionUtils.isNotEmpty(categories))
    {
      final Collection<FieldValue> fieldValues = new ArrayList<>();

      if (indexedProperty.isLocalized())
      {
        this.getValueForLocalized(indexConfig ,categories ,indexedProperty,fieldValues);
      }
      else {
        this.getValueForNonLocalized(categories,indexedProperty ,fieldValues);
      }
      return fieldValues;
    }
    else
    {
      return Collections.emptyList();
    }
  }

  /*
   * This method is created to populate values when its localized
   */

  private void getValueForLocalized(final IndexConfig indexConfig,Collection<CategoryModel> categories ,final IndexedProperty indexedProperty ,final Collection<FieldValue> fieldValues) {
    final Collection<LanguageModel> languages = indexConfig.getLanguages();
    for (final LanguageModel language : languages)
    {
      for (final CategoryModel category : categories)
      {
        if(!BlCoreConstants.BRANDS.equalsIgnoreCase(category.getName()) && CATEGORY_LIST.contains(category.getName())) {
          fieldValues.addAll(createFieldValue(category, language, indexedProperty));
        }
      }
    }
  }

    /*
     * This method is created to populate values when its non-localized
     */

  private void getValueForNonLocalized(final Collection<CategoryModel> categories ,final IndexedProperty indexedProperty ,final Collection<FieldValue> fieldValues) {
    for (final CategoryModel category : categories)
    {
      if((!BlCoreConstants.BRANDS.equalsIgnoreCase(category.getName()) && CATEGORY_LIST.contains(category.getName().toLowerCase()))) {
        fieldValues.addAll(createFieldValue(category, null, indexedProperty));
      }
    }
  }

  /**
   * This Method get the field name for current indexed property and the result on it .
   */

  private List<FieldValue> createFieldValue(final CategoryModel category,
      final LanguageModel language,
      final IndexedProperty indexedProperty)
  {
    final List<FieldValue> fieldValues = new ArrayList<>();

    if (language != null)
    {
      final Locale locale = i18nService.getCurrentLocale();
      Object value = null;
      try
      {
        i18nService.setCurrentLocale(getCommonI18NService().getLocaleForLanguage(language));
        value = getPropertyValue(category);
      }
      finally
      {
        i18nService.setCurrentLocale(locale);
      }

      final Collection<String> fieldNames = getFieldNameProvider().getFieldNames(indexedProperty, language.getIsocode());
      for (final String fieldName : fieldNames)
      {
        fieldValues.add(new FieldValue(fieldName, value));
      }
    }
    else
    {
      final Object value = getPropertyValue(category);
      final Collection<String> fieldNames = getFieldNameProvider().getFieldNames(indexedProperty, null);
      for (final String fieldName : fieldNames)
      {
        fieldValues.add(new FieldValue(fieldName, value));
      }
    }

    return fieldValues;
  }

  private Object getPropertyValue(final Object model)
  {
    return getPropertyValue(model, BlCoreConstants.CODE);
  }

  private Object getPropertyValue(final Object model, final String propertyName)
  {
    return modelService.getAttributeValue(model, propertyName);
  }

  public void setCategorySource(
      CategorySource categorySource) {
    this.categorySource = categorySource;
  }

  public FieldNameProvider getFieldNameProvider() {
    return fieldNameProvider;
  }

  public void setFieldNameProvider(
      FieldNameProvider fieldNameProvider) {
    this.fieldNameProvider = fieldNameProvider;
  }

  public CommonI18NService getCommonI18NService() {
    return commonI18NService;
  }

  public void setCommonI18NService(
      CommonI18NService commonI18NService) {
    this.commonI18NService = commonI18NService;
  }

  protected CategorySource getCategorySource()
  {
    return categorySource;
  }


}
