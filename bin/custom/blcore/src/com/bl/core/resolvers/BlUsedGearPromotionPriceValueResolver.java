package com.bl.core.resolvers;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.promotions.promotionengineservices.service.BlPromotionService;
import com.bl.core.services.calculation.BlPricingService;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import de.hybris.platform.solrfacetsearch.config.IndexedProperty;
import de.hybris.platform.solrfacetsearch.config.exceptions.FieldValueProviderException;
import de.hybris.platform.solrfacetsearch.indexer.IndexerBatchContext;
import de.hybris.platform.solrfacetsearch.indexer.spi.InputDocument;
import de.hybris.platform.solrfacetsearch.provider.impl.AbstractValueResolver;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;
import java.math.BigDecimal;
import java.util.Collection;
import java.util.Comparator;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.PredicateUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This resolver is added to get promotion price for SKU product on PLP and SLP
 *
 * @author Ritika
 */
public class BlUsedGearPromotionPriceValueResolver extends AbstractValueResolver<BlProductModel, Object, Object>
{
  private static final Logger LOG = Logger.getLogger(BlUsedGearPromotionPriceValueResolver.class);
  private BaseStoreService baseStoreService;
  private BlPromotionService blPromotionService;
  private BlPricingService blPricingService;

  /**
   * Adds the field values by getting the minimum incentivized price value from the list of BlSerialProducts on SKU
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
      final BaseStoreModel baseStoreModel = getBaseStoreService().getCurrentBaseStore();
      final Integer ugPromotionDiscount = baseStoreModel.getUsedGearPromotionDiscount();
      final boolean applyUsedGearPromotion = getBlPromotionService().isUsedGearCategoryPromotionActive() && ugPromotionDiscount != null && ugPromotionDiscount > 0;
      if (applyUsedGearPromotion && PredicateUtils.notNullPredicate().evaluate(blProductModel) && CollectionUtils.isNotEmpty(blProductModel.getSerialProducts()))
      {
        final BigDecimal minPromotionPrice = getMinPromotionPrice(getMinIncentiveSerialProduct(blProductModel,blProductModel.getSerialProducts()),getMinFinalPriceSerialProduct(blProductModel,blProductModel.getSerialProducts()),ugPromotionDiscount);
        if(minPromotionPrice.compareTo(BigDecimal.ZERO) > 0){
          inputDocument.addField(indexedProperty, minPromotionPrice.doubleValue());
        }
      }
    }
    catch (final Exception exception)
    {
      BlLogger.logFormattedMessage(LOG, Level.ERROR, LogErrorCodeEnum.SOLR_INDEXING_ERROR.getCode(), exception,
          "Failed to resolve value for minPromotionPrice attribute for product with code: {}", blProductModel.getCode());
    }
  }

  /**
   * Get Minimum promo price for Serial
   * @param minIncentivePriceSerialProduct
   * @param minFinalPriceSerialProduct
   * @param ugPromotionDiscount
   * @return
   */
  private BigDecimal getMinPromotionPrice(final BlSerialProductModel minIncentivePriceSerialProduct,
      final BlSerialProductModel minFinalPriceSerialProduct, final Integer ugPromotionDiscount) {

    if (Objects.nonNull(minIncentivePriceSerialProduct))
    {
      return getBlPricingService().getSerialPromotionPrice(minIncentivePriceSerialProduct.getIncentivizedPrice(),ugPromotionDiscount);
    }
    else if (Objects.nonNull(minFinalPriceSerialProduct))
    {
      return getBlPricingService().getSerialPromotionPrice(minFinalPriceSerialProduct.getIncentivizedPrice(),ugPromotionDiscount);

    }
    return  BigDecimal.ZERO;
  }

  /**
   * Get Minimum Incentive Price
   * @param blProductModel
   * @param serialProducts
   * @return
   */
  private BlSerialProductModel getMinIncentiveSerialProduct(final BlProductModel blProductModel, final Collection<BlSerialProductModel> serialProducts) {
    final Optional<BlSerialProductModel> minSerialIncentivizedPrice = blProductModel.getSerialProducts().stream()
        .filter(serialProductModel -> BooleanUtils.isTrue(serialProductModel.getForSale())
            && PredicateUtils.notNullPredicate().evaluate(serialProductModel.getIncentivizedPrice()))
        .collect(Collectors.minBy(Comparator.comparing(serialProduct -> serialProduct.getIncentivizedPrice())));
     return minSerialIncentivizedPrice.isPresent() ? minSerialIncentivizedPrice.get() : null;
  }

  /**
   * Get Minimum Final Serial Price
   * @param blProductModel
   * @param serialProducts
   * @return
   */
  private BlSerialProductModel getMinFinalPriceSerialProduct(final BlProductModel blProductModel, final Collection<BlSerialProductModel> serialProducts) {
    final Optional<BlSerialProductModel> minSerialfinalSalePrice = blProductModel.getSerialProducts().stream()
        .filter(serialProductModel -> BooleanUtils.isTrue(serialProductModel.getForSale())
            && PredicateUtils.notNullPredicate().evaluate(serialProductModel.getFinalSalePrice()))
        .collect(Collectors.minBy(Comparator.comparing(serialProduct -> serialProduct.getFinalSalePrice())));

    return minSerialfinalSalePrice.isPresent() ? minSerialfinalSalePrice.get() : null;
  }

  public BaseStoreService getBaseStoreService() {
    return baseStoreService;
  }

  public void setBaseStoreService(BaseStoreService baseStoreService) {
    this.baseStoreService = baseStoreService;
  }

  public BlPromotionService getBlPromotionService() {
    return blPromotionService;
  }

  public void setBlPromotionService(
      BlPromotionService blPromotionService) {
    this.blPromotionService = blPromotionService;
  }

  public BlPricingService getBlPricingService() {
    return blPricingService;
  }

  public void setBlPricingService(BlPricingService blPricingService) {
    this.blPricingService = blPricingService;
  }
}
