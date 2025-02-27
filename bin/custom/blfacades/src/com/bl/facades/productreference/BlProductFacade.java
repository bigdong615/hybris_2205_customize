package com.bl.facades.productreference;

import com.bl.core.model.BlProductModel;
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
   *
   * This method fetches all the product references on the current product
   *
   */
  List<ProductReferenceData> getProductReferencesForCode(final ProductModel currentProduct, final List<ProductOption> options, final Integer limit);

  /**
   * This method is used to get product modal.
   * @param code
   * @return BlProductModel
   */
  BlProductModel getProductForCode(final String code);

}
