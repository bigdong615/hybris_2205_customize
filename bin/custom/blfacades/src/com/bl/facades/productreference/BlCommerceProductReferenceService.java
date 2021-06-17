package com.bl.facades.productreference;

import de.hybris.platform.commercefacades.product.ProductOption;
import de.hybris.platform.commercefacades.product.data.ProductReferenceData;
import java.util.List;

/**
 * This class is responsible to handle gift card related functionality.
 * @author Neeraj Singh
 */
public interface BlCommerceProductReferenceService<TYPE, TARGET> {

  List<ProductReferenceData> getProductReferencesForCode(String code,Integer limit);


}
