package com.bl.storefront.controllers.pages;

import java.io.UnsupportedEncodingException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.bl.logging.BlLogger;

@Controller
@RequestMapping(value = "/Product.do/**")
public class ProductsPageController extends AbstractBlProductPageController {

  private static final Logger LOG = Logger.getLogger(ProductsPageController.class);

  @GetMapping
  public String showProducts(final Model model)
  {
	  BlLogger.logMessage(LOG, Level.ERROR, "Product Not found");
	  return REDIRECT_PREFIX + ROOT;
  }

  @RequestMapping(value = BlControllerConstants.PRODUCT_CODE_PATH_VARIABLE_PATTERN, method = RequestMethod.GET)
  public String productsDetail(@PathVariable("productCode")
  final String encodedProductCode,
      final Model model,
      final HttpServletRequest request, final HttpServletResponse response)
		  throws UnsupportedEncodingException
  {
	  BlLogger.logMessage(LOG, Level.ERROR, "Product Not found for Code{}", encodedProductCode);
	  return REDIRECT_PREFIX + ROOT;
  }
}

