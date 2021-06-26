package com.bl.facades.productreference;

import de.hybris.platform.commerceservices.product.CommerceProductReferenceService;
import de.hybris.platform.commerceservices.product.data.ReferenceData;
import de.hybris.platform.core.model.product.ProductModel;
import java.util.List;

/**
 * Created to send custom parameter to the product reference section
 * @param <TYPE>
 * @param <TARGET>
 */
public interface BlCommerceProductReferenceService<TYPE, TARGET>{

  List<ReferenceData<TYPE, TARGET>> getProductReferencesForCode(final ProductModel currentProduct, final Integer limit);
}




