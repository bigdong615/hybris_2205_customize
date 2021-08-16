package com.bl.core.services.calculation.impl;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.DurationEnum;
import com.bl.core.enums.PricingTierEnum;
import com.bl.core.enums.ProductTypeEnum;
import com.bl.core.model.BlPricingLogicModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.services.calculation.BlPricingService;
import com.bl.logging.BlLogger;
import de.hybris.platform.enumeration.EnumerationService;
import de.hybris.platform.europe1.model.PDTRowModel;
import de.hybris.platform.europe1.model.PriceRowModel;
import de.hybris.platform.product.UnitService;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.internal.dao.GenericDao;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.util.Config;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This class has implementation of methods related to duration prices
 *
 * @author Ritika
 */

public class DefaultBlPricingService implements BlPricingService {

  private static final Logger LOG = Logger.getLogger(DefaultBlPricingService.class);
  private GenericDao<BlPricingLogicModel> blPricingGenericDao;
  private GenericDao<PriceRowModel> priceRowGenericDao;
  private ModelService modelService;
  private CommonI18NService commonI18NService;
  private EnumerationService enumerationService;
  private UnitService unitService;

  /**
   * {@inheritDoc}
   */
  @Override
  public PriceRowModel createOrUpdateSevenDayPrice(final BlProductModel blProductModel,
      final Double retailPrice, final boolean isNew) {
    ProductTypeEnum productType = blProductModel.getProductType();
    validateParameterNotNull(productType, "ProductType must not be null");
    final Map<String, Object> queryParams = Collections
        .singletonMap(BlPricingLogicModel.PRODUCTTYPE, productType);
    final List<BlPricingLogicModel> blPricingLogicModels = getBlPricingGenericDao()
        .find(queryParams);
    if (CollectionUtils.isNotEmpty(blPricingLogicModels) && retailPrice != null) {
      final int pctOfRetail = getPctValueFromBlPricingLogic(blPricingLogicModels, retailPrice);
      return createOrUpdatePrice(pctOfRetail, retailPrice, blProductModel, isNew);
    }
    return null;
  }

  /**
   * Create or update Price row
   *
   * @param pctOfRetail
   * @param retailPrice
   * @param blProductModel
   * @param isNew
   * @return
   */
  private PriceRowModel createOrUpdatePrice(final int pctOfRetail, final Double retailPrice,
      final BlProductModel blProductModel, final boolean isNew) {
    final Double basePrice = (retailPrice * pctOfRetail) / 100;
    if (isNew) {
      return createNewDurationPrice(blProductModel, basePrice, BlCoreConstants.SEVEN_DAY_PRICE);
    } else {
      return updateSevenDayPrice(blProductModel, basePrice);
    }
  }

  /**
   * Update the price column for BasePrice when retail Price is updated
   *
   * @param blProductModel
   * @param price
   * @return
   */
  private PriceRowModel updateSevenDayPrice(final BlProductModel blProductModel,
      final Double price) {
    PriceRowModel priceRow = getPriceRowByDuration(BlCoreConstants.SEVEN_DAY_PRICE, blProductModel);
    if (priceRow != null) {
      priceRow.setPrice(price);
      getModelService().save(priceRow);
    }
    return priceRow;
  }

  /**
   * Get Percentage Value based on RetailPrice
   *
   * @param blPricingLogicModels
   * @param retailPrice
   * @return
   */
  private int getPctValueFromBlPricingLogic(final List<BlPricingLogicModel> blPricingLogicModels,
      final Double retailPrice) {
    int pctOfRetail = 0;
    final BlPricingLogicModel tier1 = getTierPricing(blPricingLogicModels, PricingTierEnum.ONE);
    final BlPricingLogicModel tier2 = getTierPricing(blPricingLogicModels, PricingTierEnum.TWO);
    final BlPricingLogicModel tier3 = getTierPricing(blPricingLogicModels, PricingTierEnum.THREE);
    if (null != tier1 && Double.compare(retailPrice, tier1.getLessThan()) <= 0) {
      pctOfRetail = tier1.getPctOfRetail();
    } else if (null != tier2 && Double.compare(retailPrice, tier2.getGreaterThan()) > 0
        && Double.compare(retailPrice, tier2.getLessThan()) <= 0) {
      pctOfRetail = tier2.getPctOfRetail();
    } else if (null != tier3 && Double.compare(retailPrice, tier3.getGreaterThan()) >= 0) {
      pctOfRetail = tier3.getPctOfRetail();
    }
    return pctOfRetail;
  }

  /**
   * Get Tier pricing by the pricing Tier
   *
   * @param blPricingLogicModels
   * @param tierEnum
   * @return
   */
  private BlPricingLogicModel getTierPricing(final List<BlPricingLogicModel> blPricingLogicModels,
      final PricingTierEnum tierEnum) {

    Optional<BlPricingLogicModel> blpricing = blPricingLogicModels.stream()
        .filter(blPricingLogicModel -> Objects.equals(blPricingLogicModel.getTier(), tierEnum))
        .findAny();
    return blpricing.orElse(null);

  }


  /**
   * Create new price row based on duration and prices.
   *
   * @param blProductModel the bl product model
   * @param price          the price
   * @param duration       the duration
   * @return the price row model
   */
  private PriceRowModel createNewDurationPrice(final BlProductModel blProductModel,
      final Double price, final String duration) {
    PriceRowModel priceRowModel = getModelService().create(PriceRowModel.class);
    priceRowModel.setProduct(blProductModel);
    priceRowModel.setPrice(price);
    priceRowModel.setCurrency(getCommonI18NService().getCurrency(BlCoreConstants.USD));
    priceRowModel.setUnit(getUnitService().getUnitForCode(BlCoreConstants.PIECES));
    priceRowModel
        .setDuration(getEnumerationService().getEnumerationValue(DurationEnum.class, duration));
    return priceRowModel;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public PriceRowModel getPriceRowByDuration(final String duration,
      final BlProductModel blProductModel) {
    validateParameterNotNull(duration, "Duration must not be null");
    validateParameterNotNull(blProductModel, "BlProduct must not be null");
    DurationEnum durationEnum = getEnumerationService()
        .getEnumerationValue(DurationEnum.class, duration);
    final Map<String, Object> queryParams = new HashMap<>();
    queryParams.put(PriceRowModel.DURATION, durationEnum);
    queryParams.put(PDTRowModel.PRODUCT, blProductModel);
    List<PriceRowModel> resultSet = getPriceRowGenericDao().find(queryParams);
    return CollectionUtils.isNotEmpty(resultSet) ? resultSet.get(0) : null;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public BigDecimal calculateFinalSalePriceForSerial(final BigDecimal forSaleBasePrice,
      final Double conditionRatingOverallScore) {
    final int pricePercentage = getPricePercentageByRating(conditionRatingOverallScore);
    return (forSaleBasePrice.setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE)
        .multiply(new BigDecimal(pricePercentage))).divide(BigDecimal.valueOf(BlCoreConstants.DIVIDE_BY_HUNDRED))
        .setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE);
  }

  /**
   * Get price percentage based on rating
   *
   * @param conditionRatingOverallScore
   * @return
   */
  private int getPricePercentageByRating(final Double conditionRatingOverallScore) {
    int pricePercent = 0;
    if (conditionRatingOverallScore > BlCoreConstants.CONDITION_RATING_HIGH) {
      pricePercent = Integer
          .parseInt(Config.getParameter("conditionrating.abovefour.price.percentage"));
    } else if (conditionRatingOverallScore > BlCoreConstants.CONDITION_RATING_MEDIUM
   		 && conditionRatingOverallScore <= BlCoreConstants.CONDITION_RATING_HIGH) {
      pricePercent = Integer
          .parseInt(Config.getParameter("conditionrating.abovethree.price.percentage"));
    } else if (conditionRatingOverallScore >= BlCoreConstants.CONDITION_RATING_LOW
   		 && conditionRatingOverallScore <= BlCoreConstants.CONDITION_RATING_MEDIUM) {
      pricePercent = Integer
          .parseInt(Config.getParameter("conditionrating.belowthree.price.percentage"));
    } else if (conditionRatingOverallScore < BlCoreConstants.CONDITION_RATING_LOW
   		 && conditionRatingOverallScore > 0) {
      pricePercent = Integer
          .parseInt(Config.getParameter("conditionrating.belowtwo.price.percentage"));
    }
    return pricePercent;
  }

  /**
   * calculate conditional rating on the basis of cosmetic and functional rating.
   * @param cosmeticRating
   * @param functionalRating
   * @return
   */
  @Override
  public Double getCalculatedConditionalRating(final double cosmeticRating, final double functionalRating){
    final double calculatedCosmeticValue = cosmeticRating * Integer.parseInt(Config.getParameter("conditioning.cosmetic.rating.percentage"))/ BlCoreConstants.DIVIDE_BY_HUNDRED;
    final double calculatedFunctionalValue = functionalRating * Integer.parseInt(Config.getParameter("conditioning.functional.rating.percentage"))/BlCoreConstants.DIVIDE_BY_HUNDRED;
    BigDecimal bigDecimal = new BigDecimal(Double.toString(calculatedCosmeticValue+calculatedFunctionalValue));
    bigDecimal = bigDecimal.setScale(1, RoundingMode.HALF_DOWN);
    return bigDecimal.doubleValue();
  }

  /**
   * Get the min promotion price for serial
   * @param serialProductPrice
   * @param ugPromotionDiscount
   * @return
   */
  @Override
  public BigDecimal getSerialPromotionPrice(final BigDecimal serialProductPrice, final Integer ugPromotionDiscount) {

    if (serialProductPrice != null && serialProductPrice.compareTo(BigDecimal.ZERO) > 0)
    {
      final BigDecimal serialPromotionPrice = serialProductPrice.subtract(serialProductPrice.multiply(new BigDecimal(ugPromotionDiscount)).divide(new BigDecimal(100))).setScale(BlCoreConstants.DECIMAL_PRECISION, RoundingMode.UP);
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,"Promotion price for serial product is : {}", serialPromotionPrice);
      return serialPromotionPrice;
    }
    return  BigDecimal.ZERO;
  }

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }

  public EnumerationService getEnumerationService() {
    return enumerationService;
  }

  public void setEnumerationService(EnumerationService enumerationService) {
    this.enumerationService = enumerationService;
  }

  public UnitService getUnitService() {
    return unitService;
  }

  public void setUnitService(UnitService unitService) {
    this.unitService = unitService;
  }

  public CommonI18NService getCommonI18NService() {
    return commonI18NService;
  }

  public void setCommonI18NService(CommonI18NService commonI18NService) {
    this.commonI18NService = commonI18NService;
  }

  public GenericDao<BlPricingLogicModel> getBlPricingGenericDao() {
    return blPricingGenericDao;
  }

  public void setBlPricingGenericDao(
      GenericDao<BlPricingLogicModel> blPricingGenericDao) {
    this.blPricingGenericDao = blPricingGenericDao;
  }

  public GenericDao<PriceRowModel> getPriceRowGenericDao() {
    return priceRowGenericDao;
  }

  public void setPriceRowGenericDao(
      GenericDao<PriceRowModel> priceRowGenericDao) {
    this.priceRowGenericDao = priceRowGenericDao;
  }

}
