package com.bl.facades.productreference;

import de.hybris.platform.commercefacades.product.ProductOption;
import de.hybris.platform.commercefacades.product.data.ProductReferenceData;
import de.hybris.platform.core.model.product.ProductModel;
import java.util.List;


public interface BlProductFacade {

  List<ProductReferenceData> getProductReferencesForCode(final ProductModel currentProduct, final List<ProductOption> options, final Integer limit);


}
