package com.bl.storefront.controllers.pages;


import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * @author Manikandan
 * This controller created to get results for rental gear category
 */
@Controller
@RequestMapping(value = "**/rent/category")
public class BlRentalGearCategoryPageController extends AbstractBlCategoryPageController {


  /**
   * This method created for getting results of level 2 categories
   */
  @RequestMapping(value = BlControllerConstants.CATEGORY_CODE_PATH_VARIABLE_PATTERN, method = RequestMethod.GET)
  public String category(@PathVariable("parentcategory") final String parentcategory, //NOSONAR
      @PathVariable("categoryCode") final String categoryCode, // NOSONAR
      @RequestParam(value = "q", required = false) final String searchQuery,
      @RequestParam(value = "page", defaultValue = "0") final int page,
      @RequestParam(value = "show", defaultValue = "Page") final ShowMode showMode,
      @RequestParam(value = "sort", required = false) final String sortCode, final Model model,
      final HttpServletRequest request, final HttpServletResponse response) throws UnsupportedEncodingException {
    final Map<Object, Object> requestAndResponseMap = new HashMap<>();
    requestAndResponseMap.put(BlControllerConstants.REQUEST, request);
    requestAndResponseMap.put(BlControllerConstants.RESPONSE, response);
    return performSearchAndGetResultsPage(categoryCode, searchQuery, page, showMode, sortCode, model,requestAndResponseMap);
  }

  /**
   * This method created to get level 1 category results
   */

  @RequestMapping(value = "/{categoryCode:.*}", method = RequestMethod.GET)
  public String superCategory(@PathVariable("categoryCode") final String categoryCode, // NOSONAR
      @RequestParam(value = "q", required = false) final String searchQuery,
      @RequestParam(value = "page", defaultValue = "0") final int page,
      @RequestParam(value = "show", defaultValue = "Page") final ShowMode showMode,
      @RequestParam(value = "sort", required = false) final String sortCode, final Model model,
      final HttpServletRequest request, final HttpServletResponse response) throws UnsupportedEncodingException {
    final Map<Object, Object> requestAndResponseMap = new HashMap<>();
    requestAndResponseMap.put(BlControllerConstants.REQUEST, request);
    requestAndResponseMap.put(BlControllerConstants.RESPONSE, response);
    return performSearchAndGetResultsPage(categoryCode, searchQuery, page, showMode, sortCode, model, requestAndResponseMap);
  }

}
