package com.bl.storefront.controllers.pages;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;

import com.bl.core.utils.BlRentalDateUtils;
import com.bl.facades.product.data.RentalDateDto;
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
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;

/**
 * This is created to render rental product details related data .
 *
 * @author vijay vishwakarma
 */
@Controller
@RequestMapping(value = "/rent/product")
public class RentalProductPageController extends AbstractBlProductPageController {

  @Resource(name = "productVariantFacade")
  private ProductFacade productFacade;

  @Resource(name = "blDatePickerService")
  private BlDatePickerService blDatePickerService;

  /**
   * This common method created to get rental duration for rental products from BlRentalDateUtils class
   */

  @ModelAttribute(name = BlControllerConstants.RENTAL_DATE)
  private RentalDateDto getRentalDuration() {
    final RentalDateDto rentalDates = blDatePickerService.getRentalDatesFromSession();
    return BlRentalDateUtils.getRentalsDuration(rentalDates);
  }

  /*
   * This method is used for render rental pdp.
   */
  @GetMapping(value = BlControllerConstants.PRODUCT_CODE_PATH_VARIABLE_PATTERN)
  public String rentalProductDetailPage(
      @PathVariable("productCode") final String encodedProductCode, final Model model,
      final HttpServletRequest request, final HttpServletResponse response)
      throws CMSItemNotFoundException, UnsupportedEncodingException {

    final String productCode = decodeWithScheme(encodedProductCode, UTF_8);
    final List<ProductOption> extraOptions = Arrays
        .asList(ProductOption.VARIANT_MATRIX_BASE, ProductOption.VARIANT_MATRIX_URL,
            ProductOption.VARIANT_MATRIX_MEDIA);

    final ProductData productData = productFacade
        .getProductForCodeAndOptions(productCode, extraOptions);
    productData.setProductPageType(BlControllerConstants.RENTAL_PAGE_IDENTIFIER);
    model.addAttribute(BlControllerConstants.IS_RENTAL_PAGE, true);
      model.addAttribute(BlCoreConstants.BL_PAGE_TYPE, BlCoreConstants.RENTAL_GEAR);
      return productDetail(encodedProductCode, extraOptions, productData, model, request, response);
  }
}
