package com.bl.core.resolvers;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlProductModel;
import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.commerceservices.search.solrfacetsearch.provider.CategorySource;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.solrfacetsearch.config.IndexConfig;
import de.hybris.platform.solrfacetsearch.config.IndexedProperty;
import de.hybris.platform.solrfacetsearch.config.exceptions.FieldValueProviderException;
import de.hybris.platform.solrfacetsearch.indexer.IndexerBatchContext;
import de.hybris.platform.solrfacetsearch.indexer.spi.InputDocument;
import de.hybris.platform.solrfacetsearch.provider.impl.AbstractValueResolver;
import java.util.Collection;
import org.apache.commons.collections4.CollectionUtils;

public class BlCategoryCodeValueResolver extends
    AbstractValueResolver<BlProductModel, Object, Object> {

  private CategorySource categorySource;
  private ModelService modelService;


  @Override
  protected void addFieldValues(InputDocument inputDocument, IndexerBatchContext indexerBatchContext, IndexedProperty indexedProperty,
      BlProductModel blProductModel, ValueResolverContext<Object, Object> valueResolverContext)
      throws FieldValueProviderException {
    final IndexConfig indexConfig = new IndexConfig();
    final Collection<CategoryModel> categories = getCategorySource().getCategoriesForConfigAndProperty(indexConfig,
            indexedProperty, blProductModel);

    if (CollectionUtils.isNotEmpty(categories)) {
        for (final CategoryModel category : categories) {
          if (!category.getName().equalsIgnoreCase(BlCoreConstants.BRANDS)) {
            inputDocument.addField(indexedProperty, getPropertyValue(category));
          }
        }
      }
  }


  protected Object getPropertyValue(final Object model) {
    return getPropertyValue(model, BlCoreConstants.CODE);
  }

  protected Object getPropertyValue(final Object model, final String propertyName) {
    return modelService.getAttributeValue(model, propertyName);
  }



  public CategorySource getCategorySource() {
    return categorySource;
  }

  public void setCategorySource(
      CategorySource categorySource) {
    this.categorySource = categorySource;
  }


  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }


}
