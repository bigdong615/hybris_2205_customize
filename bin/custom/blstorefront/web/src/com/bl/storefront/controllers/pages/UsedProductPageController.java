package com.bl.storefront.controllers.pages;

import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.commercefacades.product.ProductFacade;
import de.hybris.platform.commercefacades.product.ProductOption;
import de.hybris.platform.commercefacades.product.data.ProductData;
import java.io.UnsupportedEncodingException;
import java.util.Arrays;
import java.util.List;
import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@Controller
@RequestMapping(value = "/buy/product")
public class UsedProductPageController extends AbstractBlProductPageController {


  @Resource(name = "productVariantFacade")
  private ProductFacade productFacade;

  @RequestMapping(value = BlControllerConstants.PRODUCT_CODE_PATH_VARIABLE_PATTERN, method = RequestMethod.GET)
  public String rentalProductDetail(@PathVariable("productCode") final String encodedProductCode, final Model model,
      final HttpServletRequest request, final HttpServletResponse response)
      throws CMSItemNotFoundException, UnsupportedEncodingException {
    final String productCode = decodeWithScheme(encodedProductCode, UTF_8);
    final List<ProductOption> extraOptions = Arrays
        .asList(ProductOption.VARIANT_MATRIX_BASE, ProductOption.VARIANT_MATRIX_URL,
            ProductOption.VARIANT_MATRIX_MEDIA);

    final ProductData productData = productFacade.getProductForCodeAndOptions(productCode, extraOptions);
    productData.setProductPageType(BlControllerConstants.RENTAL_PAGE_IDENTIFIER);
    model.addAttribute(BlControllerConstants.IS_RENTAL_PAGE, false);
    return  productDetail(encodedProductCode,extraOptions,productData,model,request,response);

  }
}

