package com.bl.facades.resolver;

import com.bl.facades.constants.BlFacadesConstants;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.commercefacades.url.impl.DefaultProductDataUrlResolver;


/*
 * This is created to resolved redirect url.
 * @author  Vijay Vishwakarma
 */
public class DefaultBlProductDataUrlResolver extends DefaultProductDataUrlResolver {

  @Override
  protected String resolveInternal(final ProductData source) {

    if (source.getProductPageType().equals(BlFacadesConstants.RENTAL_PAGE_IDENTIFIER)) {
      return BlFacadesConstants.DEFAULT_REDIRECT_URL + BlFacadesConstants.RENTAL_PAGE_IDENTIFIER + BlFacadesConstants.PRODUCT_URL
          + source.getCode();
    } else if (source.getProductPageType().equals(BlFacadesConstants.USED_PAGE_IDENTIFIER)) {
      return BlFacadesConstants.DEFAULT_REDIRECT_URL + BlFacadesConstants.USED_PAGE_IDENTIFIER + BlFacadesConstants.PRODUCT_URL + source
          .getCode();
    }else{
      return super.resolveInternal(source);
    }
  }


}
