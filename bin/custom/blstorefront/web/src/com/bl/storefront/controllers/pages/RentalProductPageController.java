package com.bl.storefront.controllers.pages;

<<<<<<< HEAD
import com.bl.facades.product.data.RentalDateDto;
=======
import com.bl.core.constants.BlCoreConstants;
>>>>>>> develop
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

  /*
   * This method is used for render rental pdp.
   */
  @RequestMapping(value = BlControllerConstants.PRODUCT_CODE_PATH_VARIABLE_PATTERN, method = RequestMethod.GET)
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
    //To show date range on the recommendation section for temporary purpose once local storage is ready this will be replaced.
    getSessionService().setAttribute("selectedFromDate", "Apr 12");
    getSessionService().setAttribute("selectedToDate", "Apr 19");
    getSessionService().setAttribute("numberOfDays", "7 Days Rental");
    RentalDateDto date = new RentalDateDto();
    date.setSelectedFromDate(getSessionService().getAttribute("selectedFromDate"));
    date.setSelectedToDate(getSessionService().getAttribute("selectedToDate"));
    date.setNumberOfDays(getSessionService().getAttribute("numberOfDays"));
    model.addAttribute("datedata", date);
    model.addAttribute(BlCoreConstants.BL_PAGE_TYPE, BlCoreConstants.RENTAL_GEAR);
    return productDetail(encodedProductCode, extraOptions, productData, model, request, response);
  }
}
