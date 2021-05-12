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

/**
 * This Value Provider created for Category Facet
 * @author Manikandan
 */
public class BlCategoryCodeValueResolver extends
    AbstractValueResolver<BlProductModel, Object, Object> {

  private CategorySource categorySource;
  private ModelService modelService;

  /**
   * this method created to index values for solr for category facet
   * @param inputDocument inputDocument
   * @param indexerBatchContext indexerBatchContext
   * @param indexedProperty ndexedProperty for solr
   * @param blProductModel blProductModel
   * @param valueResolverContext valueResolverContext
   * @throws FieldValueProviderException throws exception
   */

  @Override
  protected void addFieldValues(final InputDocument inputDocument, final IndexerBatchContext indexerBatchContext, final IndexedProperty indexedProperty,
      final BlProductModel blProductModel, final ValueResolverContext<Object, Object> valueResolverContext)
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
