package com.bl.facades.productreference;

import de.hybris.platform.commercefacades.product.ProductFacade;
import de.hybris.platform.commercefacades.product.ProductOption;
import de.hybris.platform.commercefacades.product.data.ProductReferenceData;
import de.hybris.platform.core.model.product.ProductModel;
import java.util.List;

/**
 * Created to send custom parameter to the product reference section
 */
public interface BlProductFacade extends ProductFacade{

  /**
   *this method is used to send the custom parameter to get the Product reference section
   *
   */
  List<ProductReferenceData> getProductReferencesForCode(final ProductModel currentProduct, final List<ProductOption> options, final Integer limit);


}
