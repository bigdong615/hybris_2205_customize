package com.bl.core.resolvers;

import com.bl.core.model.BlProductModel;
import de.hybris.platform.solrfacetsearch.config.IndexedProperty;
import de.hybris.platform.solrfacetsearch.config.exceptions.FieldValueProviderException;
import de.hybris.platform.solrfacetsearch.indexer.IndexerBatchContext;
import de.hybris.platform.solrfacetsearch.indexer.spi.InputDocument;
import de.hybris.platform.solrfacetsearch.provider.impl.AbstractValueResolver;
import org.apache.commons.lang3.BooleanUtils;

public class BlOnSaleProductTagValueResolver extends
    AbstractValueResolver<BlProductModel, Object, Object> {

  /**
   * This Method populates onSale attribute from blProductModel and blSerialProductModel
   */
  @Override
  protected void addFieldValues(final InputDocument inputDocument,
      final IndexerBatchContext indexerBatchContext, final IndexedProperty indexedProperty,
      final BlProductModel blProductModel,
      final ValueResolverContext<Object, Object> valueResolverContext)
      throws FieldValueProviderException {
    if (BooleanUtils.isTrue(blProductModel.getForSale()) && BooleanUtils
        .isTrue(blProductModel.getOnSale()) &&
        blProductModel.getSerialProducts().stream().anyMatch(
            blSerialProductModel -> BooleanUtils.isTrue(blSerialProductModel.getForSale())
                && BooleanUtils.isTrue(blSerialProductModel
                .getOnSale()))) {
      inputDocument.addField(indexedProperty, true);
    }

  }
}
