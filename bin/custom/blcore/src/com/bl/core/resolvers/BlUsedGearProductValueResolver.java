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

import java.util.Collection;

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
        inputDocument.addField(indexedProperty , BooleanUtils.isTrue(blProductModel.getForSale()) &&
                getSerial(blProductModel.getSerialProducts()));
  }

    /**
     * This method will check if all serials are in sold / parts-needed status
     * @param serials list
     * @return true if all in solr/parts-needed status
     */
    private boolean getSerial(final Collection<BlSerialProductModel> serials) {
        return serials.stream().anyMatch(serial -> Boolean.TRUE.equals(serial.getForSale()) && (BooleanUtils.isFalse(serial.getSoftAssigned())
            && BooleanUtils.isFalse(serial.getHardAssigned())) &&
                (isActiveStatus(serial.getSerialStatus()))) ;
    }

  /**
   * This method created to check Serial status is active or not
   * @param currentStatus serial status
   * @return boolean based on status
   */
  private boolean isActiveStatus(final SerialStatusEnum currentStatus) {
    switch (currentStatus.getCode()) {
      case "ACTIVE":
      case "PARTIALLY_UNBOXED":
      case "UNBOXED":
      case "RECEIVED_OR_RETURNED":
      case "BOXED":
      case "SHIPPED":
      case "IN_HOUSE":
      case "ADDED_TO_CART":
        return Boolean.TRUE;
      default :
    }
    return Boolean.FALSE;
  }

}
