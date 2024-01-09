package com.bl.core.resolvers;

import com.bl.core.model.BlProductModel;
import de.hybris.platform.solrfacetsearch.config.IndexedProperty;
import de.hybris.platform.solrfacetsearch.config.exceptions.FieldValueProviderException;
import de.hybris.platform.solrfacetsearch.indexer.IndexerBatchContext;
import de.hybris.platform.solrfacetsearch.indexer.spi.InputDocument;
import de.hybris.platform.solrfacetsearch.provider.impl.AbstractValueResolver;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;

/**
 * @author Manikandan
 * This value resolver created for Upcoming value of  rental gear product
 */

public class BlUpComingProductValueResolver extends
    AbstractValueResolver<BlProductModel, Object, Object> {


  /**
   *
   * @param inputDocument  inputDocument adds field values to solr
   * @param indexerBatchContext indexerBatchContext
   * @param indexedProperty indexedProperty to be index
   * @param blProductModel blProductModel
   * @param valueResolverContext valueResolverContext
   * @throws FieldValueProviderException throws exception
   */
  @Override
  protected void addFieldValues(final InputDocument inputDocument, final IndexerBatchContext indexerBatchContext,
      final IndexedProperty indexedProperty, final BlProductModel blProductModel,
      final ValueResolverContext<Object, Object> valueResolverContext) throws FieldValueProviderException
  {
    inputDocument.addField(indexedProperty, BooleanUtils.isTrue(blProductModel.getForRent())
        && CollectionUtils.isEmpty(blProductModel.getSerialProducts()));
  }


}
