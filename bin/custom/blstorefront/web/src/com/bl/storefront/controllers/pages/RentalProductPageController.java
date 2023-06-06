package com.bl.storefront.controllers.pages;

import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.commercefacades.product.ProductFacade;
import de.hybris.platform.commercefacades.product.ProductOption;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.stocknotificationfacades.StockNotificationFacade;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.utils.BlRentalDateUtils;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.logging.BlLogger;

/**
 * This is created to render rental product details related data .
 *
 * @author vijay vishwakarma
 */
@Controller
@RequestMapping(value = "/rent/product")
public class RentalProductPageController extends AbstractBlProductPageController {

  private static final Logger LOG = Logger.getLogger(RentalProductPageController.class);

  @Resource(name = "productVariantFacade")
  private ProductFacade productFacade;

  @Resource(name = "stockNotificationFacade")
  private StockNotificationFacade stockNotificationFacade;


  /**
   * This common method created to get rental duration for rental products from BlRentalDateUtils class
   */

  @ModelAttribute(name = BlControllerConstants.RENTAL_DATE)
  private RentalDateDto getRentalDuration() {
    return BlRentalDateUtils.getRentalsDuration();
  }

  /*
   * This method is used for render rental pdp.
   */
  @GetMapping(value = BlControllerConstants.PRODUCT_CODE_PATH_VARIABLE_PATTERN)
  public String rentalProductDetailPage(
      @PathVariable("productCode") final String encodedProductCode, final Model model,
      final HttpServletRequest request, final HttpServletResponse response)
      throws CMSItemNotFoundException, UnsupportedEncodingException {

    try {
      final String productCode = decodeWithScheme(encodedProductCode, UTF_8);
      final ProductData productData = productFacade
          .getProductForCodeAndOptions(productCode, null);
      productData.setProductPageType(BlControllerConstants.RENTAL_PAGE_IDENTIFIER);
      model.addAttribute(BlControllerConstants.IS_RENTAL_PAGE, true);
		BlLogger.logMessage(LOG, Level.INFO, "************ Is rental page flag *** ***********" + true);
      model.addAttribute(BlCoreConstants.BL_PAGE_TYPE, BlCoreConstants.RENTAL_GEAR);
      final RentalDateDto rentalDatesFromSession = getBlDatePickerService()
          .getRentalDatesFromSession();
      if (Objects.nonNull(rentalDatesFromSession)) {
        final String nextAvailableDate = getBlCommerceStockService()
            .getNextAvailabilityDateInPDP(productCode, rentalDatesFromSession);
        if (StringUtils.isNotBlank(nextAvailableDate)) {
          model.addAttribute(BlControllerConstants.NEXT_AVAILABLE_DATE, nextAvailableDate);
          final RentalDateDto rentalDuration = getRentalDuration();
          if (Objects.nonNull(rentalDuration) && StringUtils
              .isNotBlank(rentalDuration.getSelectedFromDate())
              && !nextAvailableDate.equalsIgnoreCase(rentalDuration.getSelectedFromDate())) {
            model.addAttribute(BlControllerConstants.DISABLE_BUTTON, Boolean.TRUE);
          }
        }
      }
      final List<ProductOption> options = new ArrayList<>(
          Arrays.asList(ProductOption.VARIANT_FIRST_VARIANT, ProductOption.BASIC,
              ProductOption.URL, ProductOption.PRICE, ProductOption.SUMMARY,
              ProductOption.DESCRIPTION, ProductOption.GALLERY,
              ProductOption.CATEGORIES, ProductOption.REVIEW, ProductOption.PROMOTIONS,
              ProductOption.CLASSIFICATION,
              ProductOption.VARIANT_FULL, ProductOption.STOCK, ProductOption.VOLUME_PRICES,
              ProductOption.PRICE_RANGE,
              ProductOption.DELIVERY_MODE_AVAILABILITY, ProductOption.REQUIRED_DATA,
              ProductOption.REQUIRED_WISHLIST));
      model.addAttribute(BlControllerConstants.IS_WATCHING,
          stockNotificationFacade.isWatchingProduct(productData));
      return productDetail(encodedProductCode, options, productData, model, request, response);
    } catch(final Exception ex){
      BlLogger.logMessage(LOG, Level.ERROR,"Product Not found for Code{}",encodedProductCode, ex);
		response.setStatus(HttpServletResponse.SC_MOVED_PERMANENTLY);
		response.setHeader("Location", REDIRECT_PREFIX + encodedProductCode);
		return null;
    }
  }

}
