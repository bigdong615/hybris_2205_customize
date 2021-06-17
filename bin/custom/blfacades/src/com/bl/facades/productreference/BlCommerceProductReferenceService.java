package com.bl.facades.productreference;

import de.hybris.platform.commercefacades.product.data.ProductReferenceData;
import de.hybris.platform.commerceservices.product.data.ReferenceData;
import de.hybris.platform.core.model.product.ProductModel;
import java.util.List;

/**
 * This class is responsible to handle gift card related functionality.
 * @author Neeraj Singh
 */
public interface BlCommerceProductReferenceService<TYPE, TARGET> {



  List<ReferenceData<TYPE, TARGET>> getProductReferencesForCode(final ProductModel currentProduct, final Integer limit);
}




