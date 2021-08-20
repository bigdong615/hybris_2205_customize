package com.bl.core.search.solrfacetsearch.provider.impl;

import com.bl.core.model.BlProductModel;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import de.hybris.platform.solrfacetsearch.config.IndexedProperty;
import de.hybris.platform.solrfacetsearch.config.exceptions.FieldValueProviderException;
import de.hybris.platform.solrfacetsearch.indexer.IndexerBatchContext;
import de.hybris.platform.solrfacetsearch.indexer.spi.InputDocument;
import de.hybris.platform.solrfacetsearch.provider.impl.AbstractValueResolver;
import java.math.BigDecimal;
import org.apache.commons.collections4.PredicateUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * Added Resolver for Retail Gear Price
 * @author Ritika
 */
public class BlRetailGearPriceValueResolver extends AbstractValueResolver<BlProductModel, Object, Object>
{
  private static final Logger LOG = Logger.getLogger(BlRetailGearPriceValueResolver.class);

  /**
   * Adds the field values by getting the minimum final sale price value from the list of BlSerialProducts on SKU
   *
   * @param inputDocument
   *           the input document
   * @param indexerBatchContext
   *           the indexer batch context
   * @param indexedProperty
   *           the indexed property
   * @param blProductModel
   *           the bl product model
   * @param valueResolverContext
   *           the value resolver context
   * @throws FieldValueProviderException
   *            the field value provider exception
   */
  @Override
  protected void addFieldValues(final InputDocument inputDocument, final IndexerBatchContext indexerBatchContext,
      final IndexedProperty indexedProperty, final BlProductModel blProductModel,
      final ValueResolverContext<Object, Object> valueResolverContext) throws FieldValueProviderException
  {
    try
    {
      if (PredicateUtils.notNullPredicate().evaluate(blProductModel) && BooleanUtils.isTrue(blProductModel.getRetailGear()) && PredicateUtils.notNullPredicate().evaluate(blProductModel.getRetailGearPrice()) && blProductModel.getRetailGearPrice().compareTo(
          BigDecimal.ZERO) > 0)
      {
          inputDocument.addField(indexedProperty, blProductModel.getRetailGearPrice().doubleValue());

      }
    }
    catch (final Exception exception)
    {
      BlLogger.logFormattedMessage(LOG, Level.ERROR, LogErrorCodeEnum.SOLR_INDEXING_ERROR.getCode(), exception,
          "Failed to resolve value for retailGearPrice attribute for product with code: {}",
          blProductModel.getCode());
    }
  }
}

