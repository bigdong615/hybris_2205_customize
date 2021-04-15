package com.bl.storefront.controllers.pages;

import com.bl.core.constants.BlCoreConstants;
import com.bl.facades.product.data.RentalDateDto;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.commercefacades.product.ProductFacade;
import de.hybris.platform.commercefacades.product.ProductOption;
import de.hybris.platform.commercefacades.product.data.ProductData;
import java.io.UnsupportedEncodingException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.Arrays;
import java.util.List;
import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
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
  //To show date range on the recommendation section for temporary purpose once local storage is ready this will be replaced.
  		final LocalDate startDate = getSessionService().getAttribute("selectedFromDate");
  		final LocalDate endDate = getSessionService().getAttribute("selectedToDate");
       final RentalDateDto date = new RentalDateDto();
  		if (null != startDate && null != endDate) {
				final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("MMM d");
				date.setSelectedFromDate(startDate.format(formatter));
				date.setSelectedToDate(endDate.format(formatter));
				date.setNumberOfDays(ChronoUnit.DAYS.between(startDate, endDate)+BlControllerConstants.DAYS_RENTAL_STRING);
				model.addAttribute(BlControllerConstants.DAYS_DATA, date);
			} else {
				date.setNumberOfDays("7 Days Rental");
				model.addAttribute(BlControllerConstants.DAYS_DATA, date);
			} // Temporary code ends here
    model.addAttribute(BlCoreConstants.BL_PAGE_TYPE, BlCoreConstants.RENTAL_GEAR);
    return productDetail(encodedProductCode, extraOptions, productData, model, request, response);
  }
}
