package com.bl.facades.productreference.service;

import com.bl.facades.productreference.BlCommerceProductReferenceService;
import de.hybris.platform.catalog.enums.ProductReferenceTypeEnum;
import de.hybris.platform.commercefacades.product.data.ProductReferenceData;
import de.hybris.platform.core.model.product.ProductModel;
import java.util.List;

public class DefaultBlCommerceProductReferenceService implements BlCommerceProductReferenceService<ProductReferenceTypeEnum, ProductModel> {

  @Override
  public List<ProductReferenceData> getProductReferencesForCode(String code, Integer limit) {
    return null;
  }
}
