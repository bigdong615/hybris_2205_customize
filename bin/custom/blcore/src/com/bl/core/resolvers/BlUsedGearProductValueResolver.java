package com.bl.core.resolvers;

import com.bl.core.model.BlProductModel;
import de.hybris.platform.solrfacetsearch.config.IndexedProperty;
import de.hybris.platform.solrfacetsearch.config.exceptions.FieldValueProviderException;
import de.hybris.platform.solrfacetsearch.indexer.IndexerBatchContext;
import de.hybris.platform.solrfacetsearch.indexer.spi.InputDocument;
import de.hybris.platform.solrfacetsearch.provider.impl.AbstractValueResolver;
import org.apache.commons.lang.BooleanUtils;

/**
 * @author Manikandan
 * This Value Resolver is created to send forSale values to solr
 */
public class BlUsedGearProductValueResolver extends
    AbstractValueResolver<BlProductModel, Object, Object> {

  /**
   * this method created for creating values for solr property
   * @param inputDocument inputDocument
   * @param indexerBatchContext indexerBatchContext
   * @param indexedProperty indexedProperty for solr
   * @param blProductModel blProductModel
   * @param valueResolverContext valueResolverContext
   * @throws FieldValueProviderException throws exception
   */
  @Override
  protected void addFieldValues(final InputDocument inputDocument, final IndexerBatchContext indexerBatchContext,
      final IndexedProperty indexedProperty, final BlProductModel blProductModel,
      final ValueResolverContext<Object, Object> valueResolverContext) throws FieldValueProviderException
  {
    inputDocument.addField(indexedProperty ,blProductModel.getSerialProducts().stream().anyMatch(
        BlProductModel::getForSale) && BooleanUtils.isTrue(blProductModel.getForSale()));
  }

}
