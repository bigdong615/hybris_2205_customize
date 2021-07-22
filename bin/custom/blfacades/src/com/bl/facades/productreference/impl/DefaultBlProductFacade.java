package com.bl.facades.productreference.impl;

import com.bl.facades.productreference.BlCommerceProductReferenceService;
import com.bl.facades.productreference.BlProductFacade;
import de.hybris.platform.catalog.enums.ProductReferenceTypeEnum;
import de.hybris.platform.commercefacades.product.ProductOption;
import de.hybris.platform.commercefacades.product.data.ProductReferenceData;
import de.hybris.platform.commercefacades.product.impl.DefaultProductFacade;
import de.hybris.platform.commerceservices.product.data.ReferenceData;
import de.hybris.platform.core.model.product.ProductModel;
import java.util.ArrayList;
import java.util.List;

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


  public BlCommerceProductReferenceService<ProductReferenceTypeEnum, REF_TARGET> getBlCommerceProductReferenceService() {
    return blCommerceProductReferenceService;
  }

  public void setBlCommerceProductReferenceService(
      BlCommerceProductReferenceService<ProductReferenceTypeEnum, REF_TARGET> blCommerceProductReferenceService) {
    this.blCommerceProductReferenceService = blCommerceProductReferenceService;
  }

}


