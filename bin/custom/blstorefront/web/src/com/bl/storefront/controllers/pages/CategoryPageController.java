/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.storefront.controllers.pages;


import com.bl.core.constants.BlCoreConstants;
import de.hybris.platform.acceleratorservices.controllers.page.PageType;
import de.hybris.platform.acceleratorservices.data.RequestContextData;
import de.hybris.platform.acceleratorstorefrontcommons.constants.WebConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.ThirdPartyConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.pages.AbstractCategoryPageController;
import de.hybris.platform.acceleratorstorefrontcommons.util.MetaSanitizerUtil;
import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.cms2.model.pages.CategoryPageModel;
import de.hybris.platform.commercefacades.product.data.CategoryData;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.commercefacades.search.data.SearchStateData;
import de.hybris.platform.commerceservices.search.facetdata.FacetRefinement;
import de.hybris.platform.commerceservices.search.facetdata.ProductCategorySearchPageData;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.util.Config;
import java.io.UnsupportedEncodingException;
import java.util.stream.Collectors;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;


/**
 * Controller for a category page
 */
@Controller
@RequestMapping(value = "**/rent/category")
public class CategoryPageController extends AbstractCategoryPageController {
    private static final String CATEGORY_CODE_PATH_VARIABLE_PATTERN = "/{parentcategory:.*}/{categoryCode:.*}";

    @RequestMapping(value = CATEGORY_CODE_PATH_VARIABLE_PATTERN, method = RequestMethod.GET)
    public String category(@PathVariable("parentcategory") final String parentcategory,
                           @PathVariable("categoryCode") final String categoryCode, // NOSONAR
                           @RequestParam(value = "q", required = false) final String searchQuery,
                           @RequestParam(value = "page", defaultValue = "0") final int page,
                           @RequestParam(value = "show", defaultValue = "Page") final ShowMode showMode,
                           @RequestParam(value = "sort", required = false) final String sortCode, final Model model,
                           final HttpServletRequest request, final HttpServletResponse response) throws UnsupportedEncodingException {
        return performSearchAndGetResultsPage(categoryCode, searchQuery, page, showMode, sortCode, model, request, response);
    }


    @RequestMapping(value = "/{categoryCode:.*}", method = RequestMethod.GET)
    public String superCategory(@PathVariable("categoryCode") final String categoryCode, // NOSONAR
        @RequestParam(value = "q", required = false) final String searchQuery,
        @RequestParam(value = "page", defaultValue = "0") final int page,
        @RequestParam(value = "show", defaultValue = "Page") final ShowMode showMode,
        @RequestParam(value = "sort", required = false) final String sortCode, final Model model,
        final HttpServletRequest request, final HttpServletResponse response) throws UnsupportedEncodingException {
        return performSearchAndGetResultsPage(categoryCode, searchQuery, page, showMode, sortCode, model, request, response);
    }

    @ResponseBody
    @RequestMapping(value = CATEGORY_CODE_PATH_VARIABLE_PATTERN + "/facets", method = RequestMethod.GET)
    public FacetRefinement<SearchStateData> getFacets(@PathVariable("categoryCode") final String categoryCode,
                                                      @RequestParam(value = "q", required = false) final String searchQuery,
                                                      @RequestParam(value = "page", defaultValue = "0") final int page,
                                                      @RequestParam(value = "show", defaultValue = "Page") final ShowMode showMode,
                                                      @RequestParam(value = "sort", required = false) final String sortCode) throws UnsupportedEncodingException {
        return performSearchAndGetFacets(categoryCode, searchQuery, page, showMode, sortCode);
    }

    @ResponseBody
    @RequestMapping(value = CATEGORY_CODE_PATH_VARIABLE_PATTERN + "/results", method = RequestMethod.GET)
    public SearchResultsData<ProductData> getResults(@PathVariable("categoryCode") final String categoryCode,
                                                     @RequestParam(value = "q", required = false) final String searchQuery,
                                                     @RequestParam(value = "page", defaultValue = "0") final int page,
                                                     @RequestParam(value = "show", defaultValue = "Page") final ShowMode showMode,
                                                     @RequestParam(value = "sort", required = false) final String sortCode) throws UnsupportedEncodingException {
        return performSearchAndGetResultsData(categoryCode, searchQuery, page, showMode, sortCode);
    }


    protected String performSearchAndGetResultsPage(final String categoryCode, String searchQuery, final int page, // NOSONAR
        final ShowMode showMode, final String sortCode, final Model model, final HttpServletRequest request,
        final HttpServletResponse response) throws UnsupportedEncodingException
    {
        final CategoryModel category = getCommerceCategoryService().getCategoryForCode(categoryCode);

        // BL-268 Added For Faceted PLP & Default Sorting for PLP
        StringBuilder configParam  = new StringBuilder();
        if(StringUtils.isBlank(searchQuery)) {
            if(CollectionUtils.isEmpty(category.getSupercategories())){
                searchQuery= String.valueOf(configParam.append(Config.getParameter(BlCoreConstants.DEFAULT_SORT_CODE)));
            }
            else {
                for (CategoryModel superCategory : category.getSupercategories()) {
                    if (BlCoreConstants.BRANDS.equalsIgnoreCase(superCategory.getName())) {
                        searchQuery = String.valueOf(
                            configParam
                                .append(Config.getParameter(BlCoreConstants.DEFAULT_SORT_CODE))
                                .append(Config.getParameter(BlCoreConstants.FACTED_CATEGORY_NAME))
                                .append(categoryCode));
                    } else {
                        searchQuery = String.valueOf(configParam
                            .append(Config.getParameter(BlCoreConstants.DEFAULT_SORT_CODE)));
                    }
                }
            }
        }

        final String redirection = checkRequestUrl(request, response, getCategoryModelUrlResolver().resolve(category));
        if (StringUtils.isNotEmpty(redirection))
        {
            return redirection;
        }

        final CategoryPageModel categoryPage = getCategoryPage(category);

        final CategorySearchEvaluator categorySearch = new CategorySearchEvaluator(categoryCode, searchQuery, page, showMode,
            sortCode, categoryPage);

        ProductCategorySearchPageData<SearchStateData, ProductData, CategoryData> searchPageData = null;
        try
        {
            categorySearch.doSearch();
            searchPageData = categorySearch.getSearchPageData();
        }
        catch (final ConversionException e) // NOSONAR
        {
            searchPageData = createEmptySearchResult(categoryCode);
        }

        final boolean showCategoriesOnly = categorySearch.isShowCategoriesOnly();

        storeCmsPageInModel(model, categorySearch.getCategoryPage());
        storeContinueUrl(request);

        populateModel(model, searchPageData, showMode);
        model.addAttribute(WebConstants.BREADCRUMBS_KEY, getSearchBreadcrumbBuilder().getBreadcrumbs(categoryCode, searchPageData));
        model.addAttribute("showCategoriesOnly", showCategoriesOnly);
        model.addAttribute("categoryName", category.getName());
        model.addAttribute("pageType", PageType.CATEGORY.name());
        model.addAttribute("userLocation", getCustomerLocationService().getUserLocation());
        model.addAttribute("footerContent",category.getFooterContent());

        updatePageTitle(category, model);
        // To check whether the category is Rental Gear
        usedGearCategory(category,model);

        final RequestContextData requestContextData = getRequestContextData(request);
        requestContextData.setCategory(category);
        requestContextData.setSearch(searchPageData);

        if (searchQuery != null)
        {
            model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_FOLLOW);
        }

        addClearAllQuery(category,model);
        final String metaKeywords = MetaSanitizerUtil.sanitizeKeywords(
            category.getKeywords().stream().map(keywordModel -> keywordModel.getKeyword()).collect(
                Collectors.toSet()));
        final String metaDescription = MetaSanitizerUtil.sanitizeDescription(category.getDescription());
        setUpMetaData(model, metaKeywords, metaDescription);

        return getViewPage(categorySearch.getCategoryPage());

    }
    /**
     * This method is created to identify whether category belongs to used gear category
     */

    private void usedGearCategory(CategoryModel category, Model model) {
        if(BlCoreConstants.USED_GEAR.equalsIgnoreCase(category.getName())){
            model.addAttribute(BlCoreConstants.BL_PAGE_TYPE , BlCoreConstants.USED_GEAR_PAGE);
        }
        else if(CollectionUtils.isNotEmpty(category.getSupercategories())){
            for (CategoryModel superCategory : category.getSupercategories()) {
                if (BlCoreConstants.USED_GEAR.equalsIgnoreCase(superCategory.getName())) {
                    model
                        .addAttribute(BlCoreConstants.BL_PAGE_TYPE, BlCoreConstants.USED_GEAR_PAGE);
                } else {
                    model.addAttribute(BlCoreConstants.BL_PAGE_TYPE,
                        BlCoreConstants.RENTAL_GEAR_PAGE);
                }
            }
        }
        else if(!BlCoreConstants.USED_CATEGORY_CODE.equalsIgnoreCase(category.getName())){
            model.addAttribute(BlCoreConstants.BL_PAGE_TYPE, BlCoreConstants.RENTAL_GEAR_PAGE);
        }
    }

    private void addClearAllQuery(CategoryModel category, Model model) {
        if(CollectionUtils.isNotEmpty(category.getSupercategories())) {
            for (CategoryModel superCategory : category.getSupercategories()) {
                if (BlCoreConstants.BRANDS.equalsIgnoreCase(superCategory.getName())) {
                    model.addAttribute(BlCoreConstants.CLEAR_ALL_QUERY, "/Rental-Gear/c/rentalgear");
                }
            }
        }
        else if(BlCoreConstants.BRANDS.equalsIgnoreCase(category.getName())) {
            model.addAttribute(BlCoreConstants.CLEAR_ALL_QUERY, "/Rental-Gear/c/rentalgear");
        }
    }

}
