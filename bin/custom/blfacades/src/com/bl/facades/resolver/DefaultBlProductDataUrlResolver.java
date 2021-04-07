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

    if (BlFacadesConstants.RENTAL_PAGE_IDENTIFIER.equals(source.getProductPageType())) {
      return urlProvider( BlFacadesConstants.RENTAL_PAGE_IDENTIFIER ,source.getCode());
    } else if (BlFacadesConstants.USED_PAGE_IDENTIFIER.equals(source.getProductPageType())) {
      return  urlProvider(BlFacadesConstants.USED_PAGE_IDENTIFIER , source.getCode());
    }else{
      return super.resolveInternal(source);
    }
  }

private String urlProvider(String pageIdentifier,String productCode){
    StringBuffer urlPatter = new StringBuffer();
  urlPatter.append(BlFacadesConstants.DEFAULT_REDIRECT_URL).append(pageIdentifier).append(BlFacadesConstants.PRODUCT_URL).append(productCode);
  return urlPatter.toString();
}
}
