package com.bl.storefront.controllers.pages;


import java.io.UnsupportedEncodingException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * @author ManiKandan
 *
 * This controller added for Used Gear Categories
 */

@Controller
@RequestMapping(value = "/buy/category/")
public class BlUsedCategoryPageController extends AbstractBlCategoryPageController {

  protected static final String CATEGORY_CODE_PATH_VARIABLE_PATTERN = "/{categoryCode:.*}";

  /**
   * This method used to get results for used gear
   */
  @RequestMapping(value = CATEGORY_CODE_PATH_VARIABLE_PATTERN, method = RequestMethod.GET)
  public String category(@PathVariable("categoryCode") final String categoryCode, // NOSONAR
      @RequestParam(value = "q", required = false) final String searchQuery,
      @RequestParam(value = "page", defaultValue = "0") final int page,
      @RequestParam(value = "show", defaultValue = "Page") final ShowMode showMode,
      @RequestParam(value = "sort", required = false) final String sortCode, final Model model,
      final HttpServletRequest request, final HttpServletResponse response) throws UnsupportedEncodingException {
    return performSearchAndGetResultsPage(categoryCode, searchQuery, page, showMode, sortCode, model, request, response);
  }

}
