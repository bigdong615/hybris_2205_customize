package com.bl.facades.productreference;

import de.hybris.platform.commercefacades.product.ProductOption;
import de.hybris.platform.commercefacades.product.data.ProductReferenceData;
import de.hybris.platform.core.model.product.ProductModel;
import java.util.List;


public interface BlProductFacade {

  List<ProductReferenceData> getProductReferencesForCode(ProductModel currentProduct,List<ProductOption> options, Integer limit);


}
