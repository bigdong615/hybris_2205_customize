package com.bl.facades.productreference;

import com.bl.core.model.GiftCardModel;
import de.hybris.platform.catalog.enums.ProductReferenceTypeEnum;
import de.hybris.platform.commercefacades.product.ProductOption;
import de.hybris.platform.commercefacades.product.data.ProductReferenceData;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.product.ProductModel;
import java.util.List;

/**
 * This class is responsible to handle gift card related functionality.
 * @author Neeraj Singh
 */
public interface BlProductFacade {

  List<ProductReferenceData> getProductReferencesForCode(ProductModel currentProduct,List<ProductOption> options, Integer limit);


}
