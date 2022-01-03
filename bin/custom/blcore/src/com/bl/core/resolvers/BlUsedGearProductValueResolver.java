package com.bl.core.resolvers;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.stock.BlCommerceStockService;
import de.hybris.platform.solrfacetsearch.config.IndexedProperty;
import de.hybris.platform.solrfacetsearch.config.exceptions.FieldValueProviderException;
import de.hybris.platform.solrfacetsearch.indexer.IndexerBatchContext;
import de.hybris.platform.solrfacetsearch.indexer.spi.InputDocument;
import de.hybris.platform.solrfacetsearch.provider.impl.AbstractValueResolver;
import java.util.Objects;
import org.apache.commons.lang.BooleanUtils;

import java.util.Collection;

/**
 * @author Manikandan
 * This Value Resolver is created to send forSale values to solr
 */
public class BlUsedGearProductValueResolver extends
    AbstractValueResolver<BlProductModel, Object, Object> {

  private BlCommerceStockService blCommerceStockService;

  /**
   * this method created for creating values for solr property
   *
   * @param inputDocument        inputDocument
   * @param indexerBatchContext  indexerBatchContext
   * @param indexedProperty      indexedProperty for solr
   * @param blProductModel       blProductModel
   * @param valueResolverContext valueResolverContext
   * @throws FieldValueProviderException throws exception
   */
  @Override
  protected void addFieldValues(final InputDocument inputDocument,
      final IndexerBatchContext indexerBatchContext,
      final IndexedProperty indexedProperty, final BlProductModel blProductModel,
      final ValueResolverContext<Object, Object> valueResolverContext)
      throws FieldValueProviderException {
    inputDocument.addField(indexedProperty, BooleanUtils.isTrue(blProductModel.getForSale()) &&
        getSerial(blProductModel.getSerialProducts()));
  }

  /**
   * This method will check if all serials are in sold / parts-needed status
   *
   * @param serials list
   * @return true if all in solr/parts-needed status
   */
  private boolean getSerial(final Collection<BlSerialProductModel> serials) {
    return serials.stream().anyMatch(serial -> Boolean.TRUE.equals(serial.getForSale())
        && (isUsedGearSerialNotAssignedToRentalOrder(serial)) && (isActiveStatus(serial.getSerialStatus())));
  }

  /**
   * This method created to check Serial status is active or not
   *
   * @param currentStatus serial status
   * @return boolean based on status
   */
  private boolean isActiveStatus(final SerialStatusEnum currentStatus) {
    switch (currentStatus.getCode()) {
      case BlCoreConstants.ACTIVE_STATUS:
      case BlCoreConstants.RECEIVED_OR_RETURNED:
      case BlCoreConstants.IN_HOUSE:
      case BlCoreConstants.ADDED_TO_CART:
        return Boolean.TRUE;
      default:
    }
    return Boolean.FALSE;
  }

  /**
   * This method created to check whether the serial is assigned or not
   * @param blSerialProductModel  blSerialProductModel
   * @return boolean
   */
  private boolean isUsedGearSerialNotAssignedToRentalOrder(final BlSerialProductModel blSerialProductModel) {
  return getBlCommerceStockService().isUsedGearSerialNotAssignedToRentalOrder(blSerialProductModel.getProductId() , blSerialProductModel.getCode());
}

  public BlCommerceStockService getBlCommerceStockService() {
    return blCommerceStockService;
  }

  public void setBlCommerceStockService(BlCommerceStockService blCommerceStockService) {
    this.blCommerceStockService = blCommerceStockService;
  }


}
