package com.bl.storefront.controllers.pages;

import de.hybris.platform.assistedserviceservices.utils.AssistedServiceSession;
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

import org.apache.commons.lang.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlProductModel;
import com.bl.facades.productreference.BlProductFacade;
import com.bl.logging.BlLogger;

@Controller
@RequestMapping(value = "/buy/product")
public class UsedProductPageController extends AbstractBlProductPageController {

  private static final Logger LOG = Logger.getLogger(UsedProductPageController.class);

  @Resource(name = "productVariantFacade")
  private ProductFacade productFacade;

  @Resource(name = "blProductFacade")
  private BlProductFacade blproductFacade;


  @RequestMapping(value = BlControllerConstants.PRODUCT_CODE_PATH_VARIABLE_PATTERN, method = RequestMethod.GET)
  public String rentalProductDetail(@PathVariable("productCode") final String encodedProductCode,
      final Model model,
      final HttpServletRequest request, final HttpServletResponse response)
      throws CMSItemNotFoundException, UnsupportedEncodingException {
    List<ProductOption> options;
    try {
    final String productCode = decodeWithScheme(encodedProductCode, UTF_8);
    final BlProductModel productModel = blproductFacade.getProductForCode(productCode);

    final ProductData productData = productFacade
        .getProductForCodeAndOptions(productCode, null);
    productData.setProductPageType(BlControllerConstants.USED_PAGE_IDENTIFIER);
    model.addAttribute(BlControllerConstants.IS_RENTAL_PAGE, false);
	 BlLogger.logMessage(LOG, Level.INFO, "************ Is rental page flag *** ***********" + false);

    model.addAttribute(BlCoreConstants.BL_PAGE_TYPE , BlCoreConstants.USED_GEAR_CODE);
    if(BooleanUtils.isTrue(productModel.getRetailGear())){
      if(getSessionService().getAttribute(BlCoreConstants.ASM_SESSION_PARAMETER) == null || ((AssistedServiceSession)getSessionService().getAttribute(BlCoreConstants.ASM_SESSION_PARAMETER)).getAgent()==null){
        return BlControllerConstants.REDIRECT_TO_HOME_URL;
      }
      options = new ArrayList<>(Arrays.asList(ProductOption.VARIANT_FIRST_VARIANT, ProductOption.BASIC,
          ProductOption.URL, ProductOption.SUMMARY, ProductOption.DESCRIPTION, ProductOption.GALLERY,
          ProductOption.CATEGORIES, ProductOption.CLASSIFICATION, ProductOption.VARIANT_FULL, ProductOption.DELIVERY_MODE_AVAILABILITY,ProductOption.REQUIRED_DATA));
    }
    else{
        options = new ArrayList<>(Arrays.asList(ProductOption.VARIANT_FIRST_VARIANT, ProductOption.BASIC,
				ProductOption.URL, ProductOption.SUMMARY, ProductOption.DESCRIPTION, ProductOption.GALLERY,
				ProductOption.CATEGORIES, ProductOption.REVIEW, ProductOption.PROMOTIONS, ProductOption.CLASSIFICATION,
				ProductOption.VARIANT_FULL, ProductOption.DELIVERY_MODE_AVAILABILITY,ProductOption.REQUIRED_DATA,
				ProductOption.REQUIRED_SERIAL_DATA) );
    }
    return productDetail(encodedProductCode, options, productData, model, request, response);
    } catch(final Exception ex){
      BlLogger.logMessage(LOG, Level.ERROR,"Product Not found for Code{}",encodedProductCode, ex);
		response.setStatus(HttpServletResponse.SC_MOVED_PERMANENTLY);
		response.setHeader("Location", REDIRECT_PREFIX + encodedProductCode);
		return null;
    }
  }
}

