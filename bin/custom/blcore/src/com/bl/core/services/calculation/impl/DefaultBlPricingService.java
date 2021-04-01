package com.bl.core.services.calculation.impl;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.dao.calculation.BlPricingDao;
import com.bl.core.dao.pricingratio.BlPricingRatioDao;
import com.bl.core.enums.DurationEnum;
import com.bl.core.enums.PricingTierEnum;
import com.bl.core.enums.ProductTypeEnum;
import com.bl.core.model.BlConstrainedPricingRatioModel;
import com.bl.core.model.BlPricingLogicModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlStandardPricingRatioModel;
import com.bl.core.services.calculation.BlPricingService;
import de.hybris.platform.enumeration.EnumerationService;
import de.hybris.platform.europe1.model.PriceRowModel;
import de.hybris.platform.product.UnitService;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import java.util.Collection;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;

/**
 * This class has implementation of methods
 * related to duration prices
 *
 * @author Ritika
 */

public class DefaultBlPricingService implements BlPricingService {

  private BlPricingDao blPricingDao;
  private BlPricingRatioDao blPricingRatioDao;
  private ModelService modelService;
  private CommonI18NService commonI18NService;
  private EnumerationService enumerationService;
  private UnitService unitService;

  @Override
  public PriceRowModel createOrUpdateSevenDayPrice(final BlProductModel blProductModel, final Double retailPrice, final boolean isNew) {
    PriceRowModel basePriceRow;
    ProductTypeEnum productType = blProductModel.getProductType();
    List<BlPricingLogicModel> blPricingLogicModels = getBlPricingDao().getBlPricingByProductType(productType);
    if (CollectionUtils.isNotEmpty(blPricingLogicModels) && retailPrice != null) {
      int pctOfRetail = getPctValueFromBlPricingLogic(blPricingLogicModels, retailPrice);
      Double basePrice = (retailPrice * pctOfRetail) / 100;
      if(isNew) {
        basePriceRow = createNewDurationPrice(blProductModel, basePrice, BlCoreConstants.SEVEN_DAY_PRICE);
      }
      else {
        basePriceRow = updateSevenDayPrice(blProductModel,basePrice);
      }
      return basePriceRow;
    }
    return null;
  }

  /**
   * Update the price column for BasePrice when retail Price is updated
   * @param blProductModel
   * @param price
   * @return
   */
  private PriceRowModel updateSevenDayPrice(final BlProductModel blProductModel, final Double price) {
    PriceRowModel priceRow = getPriceRowByDuration(BlCoreConstants.SEVEN_DAY_PRICE, blProductModel);
    if(priceRow != null) {
      priceRow.setPrice(price);
      getModelService().save(priceRow);
      getModelService().refresh(blProductModel);
    }
    return  priceRow;
  }

  /**
   * Get Percentage Value based on RetailPrice
   * @param blPricingLogicModels
   * @param retailPrice
   * @return
   */
  private int getPctValueFromBlPricingLogic(final List<BlPricingLogicModel> blPricingLogicModels,final Double retailPrice) {
    int pctOfRetail = 0;
    BlPricingLogicModel tier1 = getTierPricing(blPricingLogicModels, PricingTierEnum.ONE);
    BlPricingLogicModel tier2 = getTierPricing(blPricingLogicModels, PricingTierEnum.TWO);
    BlPricingLogicModel tier3 = getTierPricing(blPricingLogicModels, PricingTierEnum.THREE);
    if (null != tier1 &&  Double.compare(retailPrice, tier1.getLessThan()) <= 0) {
      pctOfRetail = tier1.getPctOfRetail();
    } else if (null != tier2 && Double.compare(retailPrice, tier2.getGreaterThan()) > 0 && Double.compare(retailPrice, tier2.getLessThan()) <= 0) {
      pctOfRetail = tier2.getPctOfRetail();
    } else if (null != tier3 && Double.compare(retailPrice, tier3.getGreaterThan()) >= 0) {
      pctOfRetail = tier3.getPctOfRetail();
    }
    return pctOfRetail;
  }

  /**
   * Get Tier pricing by the pricing Tier
   * @param blPricingLogicModels
   * @param tierEnum
   * @return
   */
  private BlPricingLogicModel getTierPricing(final List<BlPricingLogicModel> blPricingLogicModels,final PricingTierEnum tierEnum) {

    Optional<BlPricingLogicModel> blpricing = blPricingLogicModels.stream()
        .filter(blPricingLogicModel -> Objects.equals(blPricingLogicModel.getTier(), tierEnum)).findAny();
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
  private PriceRowModel createNewDurationPrice(final BlProductModel blProductModel, final Double price, final String duration) {
    PriceRowModel priceRowModel = getModelService().create(PriceRowModel.class);
    priceRowModel.setProduct(blProductModel);
    priceRowModel.setPrice(price);
    priceRowModel.setCurrency(getCommonI18NService().getCurrency(BlCoreConstants.USD));
    priceRowModel.setUnit(getUnitService().getUnitForCode(BlCoreConstants.PIECES));
    priceRowModel
        .setDuration(getEnumerationService().getEnumerationValue(DurationEnum.class, duration));
    return priceRowModel;
  }


  @Override
  public PriceRowModel getPriceRowByDuration(final String duration, final BlProductModel blProductModel) {
    validateParameterNotNull(duration, "Duration must not be null");
    validateParameterNotNull(blProductModel, "BlProduct must not be null");
    DurationEnum durationEnum = getEnumerationService().getEnumerationValue(DurationEnum.class, duration);
    return getBlPricingDao().getPriceRowByDuration(durationEnum,blProductModel);
  }

  @Override
  public Collection<PriceRowModel> createOrUpdateFixedDurationPrices(final BlProductModel blProductModel,final Double sevenDayPrice,final boolean isNew) {
    Collection<PriceRowModel> oldPriceRows = blProductModel.getEurope1Prices();
    Collection<PriceRowModel> newPriceRows = new HashSet<>(oldPriceRows);
    if (Boolean.TRUE.equals(blProductModel.getConstrained()) && CollectionUtils.isNotEmpty(getConstrainedPricingRatiosForProduct())) {
      for(BlConstrainedPricingRatioModel pricingRatio: getConstrainedPricingRatiosForProduct()){
        PriceRowModel durationPriceRow = createOrUpdatePriceByPricingRatios(sevenDayPrice,
            pricingRatio.getPricingRatio(), pricingRatio.getDuration(), blProductModel,isNew);
         newPriceRows.add(durationPriceRow);
      }
    }
    else {
      if (CollectionUtils.isNotEmpty(getStandardPricingRatiosForProduct())) {
          for(BlStandardPricingRatioModel pricingRatio: getStandardPricingRatiosForProduct()){
            PriceRowModel durationPriceRow = createOrUpdatePriceByPricingRatios(sevenDayPrice,
                pricingRatio.getPricingRatio(), pricingRatio.getDuration(), blProductModel,isNew);
            newPriceRows.add(durationPriceRow);
          }
        }
      }
    return newPriceRows;
  }


  /**
   * *  Create Or Update Price Row By Duration
   * TO-DO: Update logic is pending for testing
   * @param basePrice
   * @param pricingRatio
   * @param duration
   * @param blProductModel
   * @param isNew
   * @return
   */
  private PriceRowModel createOrUpdatePriceByPricingRatios(final Double basePrice,final Double pricingRatio, final DurationEnum duration, final BlProductModel blProductModel, final boolean isNew) {
    // calculate Rental price rows based on number of days and save it
    Double ratioPrice = basePrice * pricingRatio;
    PriceRowModel durationPriceRow = getPriceRowByDuration(duration.getCode(), blProductModel);
    if (null == durationPriceRow  && BooleanUtils.isTrue(isNew)) {
      durationPriceRow = createNewDurationPrice(blProductModel, ratioPrice, duration.getCode());
    }
    else{
      Objects.requireNonNull(durationPriceRow).setPrice(ratioPrice);
    }
    return durationPriceRow;
  }

  /**
   *
   * Get the ratios from Standard Pricing Ratio Table
   * @return
   */
  private List<BlStandardPricingRatioModel> getStandardPricingRatiosForProduct() {
      List<BlStandardPricingRatioModel> standardPricingRatio = getBlPricingRatioDao().getStandardPricingRatio();
       return  CollectionUtils.isNotEmpty(standardPricingRatio) ? standardPricingRatio : Collections.emptyList();
    }

  /**
   * Get the ratios from Constrained Pricing Ratio Table
   * @return
   */
  private List<BlConstrainedPricingRatioModel> getConstrainedPricingRatiosForProduct() {
      List<BlConstrainedPricingRatioModel> constrainedPricingRatio = getBlPricingRatioDao().getConstrainedPricingRatio();
      return  CollectionUtils.isNotEmpty(constrainedPricingRatio) ? constrainedPricingRatio : Collections.emptyList();

  }


  public BlPricingDao getBlPricingDao() {
    return blPricingDao;
  }

  public void setBlPricingDao(BlPricingDao blPricingDao) {
    this.blPricingDao = blPricingDao;
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

  public BlPricingRatioDao getBlPricingRatioDao() {
    return blPricingRatioDao;
  }

  public void setBlPricingRatioDao(BlPricingRatioDao blPricingRatioDao) {
    this.blPricingRatioDao = blPricingRatioDao;
  }
}
