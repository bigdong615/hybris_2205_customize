package com.bl.facades.productreference.impl;

import com.bl.core.model.BlProductModel;
import com.bl.facades.constants.BlFacadesConstants;
import com.bl.facades.populators.BlSerialProductPopulator;
import com.bl.facades.product.data.SerialProductData;
import com.bl.facades.productreference.BlCommerceProductReferenceService;
import com.bl.facades.productreference.BlProductFacade;
import de.hybris.platform.catalog.enums.ProductReferenceTypeEnum;
import de.hybris.platform.commercefacades.product.ProductOption;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.commercefacades.product.data.ProductReferenceData;
import de.hybris.platform.commercefacades.product.data.PromotionData;
import de.hybris.platform.commercefacades.product.impl.DefaultProductFacade;
import de.hybris.platform.commerceservices.product.data.ReferenceData;
import de.hybris.platform.core.model.product.ProductModel;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;

/**
 * Default implementation of {@link BlProductFacade}.
 *
 * @param <REF_TARGET> generic type parameter for the product model type It is a custom
 * implementation of OOTB class for Product Reference Section
 * @author Sahana SB
 */
public class DefaultBlProductFacade<REF_TARGET> extends DefaultProductFacade implements
    BlProductFacade {


  private BlCommerceProductReferenceService<ProductReferenceTypeEnum, REF_TARGET> blCommerceProductReferenceService;
  private BlSerialProductPopulator blSerialProductPopulator;
  /**
   *
   * {@inheritDoc}
   */
  @Override
  public List<ProductReferenceData> getProductReferencesForCode(final ProductModel currentProduct,
      final List<ProductOption> options, final Integer limit) {
    final List<ReferenceData<ProductReferenceTypeEnum, REF_TARGET>> references = getBlCommerceProductReferenceService()
        .getProductReferencesForCode(currentProduct, limit);

    final List<ProductReferenceData> result = new ArrayList<>();

    for (final ReferenceData<ProductReferenceTypeEnum, REF_TARGET> reference : references) {
      final ProductReferenceData productReferenceData = (ProductReferenceData) getReferenceDataProductReferenceConverter()
          .convert(reference);
      getReferenceProductConfiguredPopulator()
          .populate(reference.getTarget(), productReferenceData.getTarget(), options);
      result.add(productReferenceData);
    }
    return result;
  }

  /**
   * Get promotion Message when used gear serial product promotion is active
   *
   * @param blProductData
   * @return String
   */
  @Override
  public String getPromotionMessageFromUsedGear(final ProductData blProductData) {
    String promoMessage = StringUtils.EMPTY;
    BlProductModel blProductModel = (BlProductModel) getProductService().getProductForCode(blProductData.getCode());
    if(CollectionUtils.isNotEmpty(blProductModel.getSerialProducts())){
      getBlSerialProductPopulator().populate(blProductModel,blProductData);
      if(CollectionUtils.isNotEmpty(blProductData.getSerialproducts())){
        for(Object serialProduct: blProductData.getSerialproducts()){
           return getSerialPromotionMessage((SerialProductData) serialProduct);
        }
      }
    }
    return promoMessage;
  }

  /**
   * Check potential Message for onSale true serial
   * @param serialProduct
   * @return String
   */
  private String getSerialPromotionMessage(final SerialProductData serialProduct) {
     if(CollectionUtils.isNotEmpty(serialProduct.getPotentialPromotions())){
       for(PromotionData p:serialProduct.getPotentialPromotions()) {
         if (StringUtils.containsIgnoreCase(p.getCode(), BlFacadesConstants.POTENTIAL) && serialProduct.isOnSale()) {
            return p.getDescription();
         }
       }
     }
     return StringUtils.EMPTY ;
  }


  public BlCommerceProductReferenceService<ProductReferenceTypeEnum, REF_TARGET> getBlCommerceProductReferenceService() {
    return blCommerceProductReferenceService;
  }

  public void setBlCommerceProductReferenceService(
      BlCommerceProductReferenceService<ProductReferenceTypeEnum, REF_TARGET> blCommerceProductReferenceService) {
    this.blCommerceProductReferenceService = blCommerceProductReferenceService;
  }

  public BlSerialProductPopulator getBlSerialProductPopulator() {
    return blSerialProductPopulator;
  }

  public void setBlSerialProductPopulator(
      BlSerialProductPopulator blSerialProductPopulator) {
    this.blSerialProductPopulator = blSerialProductPopulator;
  }
}


