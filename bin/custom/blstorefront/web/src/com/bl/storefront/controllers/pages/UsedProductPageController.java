package com.bl.storefront.controllers.pages;

import com.bl.core.constants.BlCoreConstants;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.commercefacades.product.ProductFacade;
import de.hybris.platform.commercefacades.product.ProductOption;
import de.hybris.platform.commercefacades.product.data.ProductData;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
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
  public String rentalProductDetail(@PathVariable("productCode") final String encodedProductCode,
      final Model model,
      final HttpServletRequest request, final HttpServletResponse response)
      throws CMSItemNotFoundException, UnsupportedEncodingException {
    final String productCode = decodeWithScheme(encodedProductCode, UTF_8);
    final ProductData productData = productFacade
        .getProductForCodeAndOptions(productCode, null);
    productData.setProductPageType(BlControllerConstants.USED_PAGE_IDENTIFIER);
    model.addAttribute(BlControllerConstants.IS_RENTAL_PAGE, false);
    model.addAttribute(BlCoreConstants.BL_PAGE_TYPE , BlCoreConstants.USED_GEAR_CODE);
    final List<ProductOption> options = new ArrayList<>(Arrays.asList(ProductOption.VARIANT_FIRST_VARIANT, ProductOption.BASIC,
				ProductOption.URL, ProductOption.SUMMARY, ProductOption.DESCRIPTION, ProductOption.GALLERY,
				ProductOption.CATEGORIES, ProductOption.REVIEW, ProductOption.PROMOTIONS, ProductOption.CLASSIFICATION,
				ProductOption.VARIANT_FULL, ProductOption.DELIVERY_MODE_AVAILABILITY,ProductOption.REQUIRED_DATA,
				ProductOption.REQUIRED_SERIAL_DATA) );		
    return productDetail(encodedProductCode, options, productData, model, request, response);
  }
}

