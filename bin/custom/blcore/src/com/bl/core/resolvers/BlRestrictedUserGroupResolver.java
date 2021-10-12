package com.bl.core.resolvers;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlProductModel;
import de.hybris.platform.core.model.security.PrincipalModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.solrfacetsearch.config.IndexedProperty;
import de.hybris.platform.solrfacetsearch.config.exceptions.FieldValueProviderException;
import de.hybris.platform.solrfacetsearch.indexer.IndexerBatchContext;
import de.hybris.platform.solrfacetsearch.indexer.spi.InputDocument;
import de.hybris.platform.solrfacetsearch.provider.impl.AbstractValueResolver;
import java.util.Set;
import org.apache.commons.collections4.CollectionUtils;

public class BlRestrictedUserGroupResolver extends   AbstractValueResolver<BlProductModel, Object, Object> {

  private ModelService modelService;

  /**
   * this method created to index values for solr for restricted groups
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
    final Set<PrincipalModel> restrictedUserGroups = blProductModel.getRestrictedPrincipals();

    if (CollectionUtils.isNotEmpty(restrictedUserGroups)) {
      for (final PrincipalModel userGroup : restrictedUserGroups) {
        inputDocument.addField(indexedProperty, getPropertyValue(userGroup));
      }
    }
  }


  protected Object getPropertyValue(final Object model) {
    return getPropertyValue(model, BlCoreConstants.UID);
  }

  protected Object getPropertyValue(final Object model, final String propertyName) {
    return getModelService().getAttributeValue(model, propertyName);
  }


  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }


}
