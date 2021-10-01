package com.bl.core.resolvers;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlProductModel;
import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.solrfacetsearch.config.IndexedProperty;
import de.hybris.platform.solrfacetsearch.config.exceptions.FieldValueProviderException;
import de.hybris.platform.solrfacetsearch.indexer.IndexerBatchContext;
import de.hybris.platform.solrfacetsearch.indexer.spi.InputDocument;
import de.hybris.platform.solrfacetsearch.provider.impl.AbstractValueResolver;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

/**
 * This ValueProvider created to index Global Facet values to solr
 * @author Manikandan
 */
public class BlGlobalFacetValueResolver extends
    AbstractValueResolver<BlProductModel, Object, Object> {

  private static final List<String> CATEGORY_LIST = Arrays.asList(BlCoreConstants.LENSES, BlCoreConstants.CAMERAS, BlCoreConstants.PRODUCTION,BlCoreConstants.PACKAGE,BlCoreConstants.GEAR_PACKAGE);

  private ModelService modelService;


  /**
   *
   * @param inputDocument inputDocument adds field values to solr
   * @param indexerBatchContext indexerBatchContext
   * @param indexedProperty indexedProperty to be index
   * @param blProductModel blProductModel
   * @param valueResolverContext valueResolverContext
   * @throws FieldValueProviderException throws exception
   */
  @Override
  protected void addFieldValues(final InputDocument inputDocument,
      final IndexerBatchContext indexerBatchContext, final IndexedProperty indexedProperty,
     final BlProductModel blProductModel, final ValueResolverContext<Object, Object> valueResolverContext)
      throws FieldValueProviderException {
      for (final CategoryModel category : getCategoriesForProduct(new ArrayList<>() ,blProductModel))
      {
        if(CATEGORY_LIST.contains(category.getName().toLowerCase())) {
          inputDocument.addField(indexedProperty, createFieldValue(category));
        }
    }

  }

  /**
   * This method created to get the super categories for product
   */
  public Collection<CategoryModel> getCategoriesForProduct(final Collection<CategoryModel> categories , final BlProductModel blProductModel) {
    for (final CategoryModel category : blProductModel.getSupercategories()) {
      categories.add(category);
      categories.addAll(category.getSupercategories());
    }
    return categories;
  }

  private Object createFieldValue(final CategoryModel category)
  {
     return getPropertyValue(category);
  }


  private Object getPropertyValue(final Object model)
  {
    return getPropertyValue(model, BlCoreConstants.NAME);
  }

  private Object getPropertyValue(final Object model, final String propertyName)
  {
    return getModelService().getAttributeValue(model, propertyName);
  }

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }


}

