package com.bl.facades.productreference;

import de.hybris.platform.commerceservices.product.CommerceProductReferenceService;
import de.hybris.platform.commerceservices.product.data.ReferenceData;
import de.hybris.platform.core.model.product.ProductModel;
import java.util.List;


public interface BlCommerceProductReferenceService<TYPE, TARGET>{

  List<ReferenceData<TYPE, TARGET>> getProductReferencesForCode(final ProductModel currentProduct, final Integer limit);
}




