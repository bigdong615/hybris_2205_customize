package com.bl.core.resolvers;

import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import de.hybris.platform.solrfacetsearch.config.IndexedProperty;
import de.hybris.platform.solrfacetsearch.config.exceptions.FieldValueProviderException;
import de.hybris.platform.solrfacetsearch.indexer.IndexerBatchContext;
import de.hybris.platform.solrfacetsearch.indexer.spi.InputDocument;
import de.hybris.platform.solrfacetsearch.provider.impl.AbstractValueResolver;
import org.apache.commons.lang.BooleanUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

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
        inputDocument.addField(indexedProperty ,blProductModel.getSerialProducts().stream().anyMatch(BlProductModel::getForSale)
              && BooleanUtils.isTrue(blProductModel.getForSale()) && getSerial(blProductModel.getSerialProducts()));
  }

    /**
     * This method will check if all serials are in sold / parts-needed status
     * @param serials list
     * @return true if all in solr/parts-needed status
     */
    private boolean getSerial(final Collection<BlSerialProductModel> serials) {
        return serials.stream().anyMatch(serial -> serial.getSerialStatus().getCode().equals(SerialStatusEnum.ACTIVE.getCode()) ||
                serial.getSerialStatus().getCode().equals(SerialStatusEnum.ADDED_TO_CART.getCode()) ||
                serial.getSerialStatus().getCode().equals(SerialStatusEnum.RECEIVED_OR_RETURNED.getCode()));
    }

}
