/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.storefront.controllers.pages;


import com.bl.core.constants.BlCoreConstants;
import com.bl.core.utils.BlRentalDateUtils;
import com.bl.facades.cart.BlCartFacade;
import com.google.common.base.Splitter;
import de.hybris.platform.acceleratorservices.controllers.page.PageType;
import de.hybris.platform.acceleratorservices.data.RequestContextData;
import de.hybris.platform.acceleratorstorefrontcommons.constants.WebConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.ThirdPartyConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.pages.AbstractCategoryPageController;
import de.hybris.platform.acceleratorstorefrontcommons.util.MetaSanitizerUtil;
import de.hybris.platform.assistedserviceservices.utils.AssistedServiceSession;
import de.hybris.platform.catalog.model.KeywordModel;
import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.cms2.model.pages.CategoryPageModel;
import de.hybris.platform.commercefacades.product.data.CategoryData;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.commercefacades.search.data.SearchQueryData;
import de.hybris.platform.commercefacades.search.data.SearchStateData;
import de.hybris.platform.commerceservices.search.facetdata.FacetRefinement;
import de.hybris.platform.commerceservices.search.facetdata.ProductCategorySearchPageData;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SortData;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.util.Config;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

/**
 * Since we have seperate controller for used gear and rental gear . Hence making as AbstractBlCategoryPageController
 */


public class AbstractBlCategoryPageController extends AbstractCategoryPageController {

    @Resource(name = "cartFacade")
    private BlCartFacade blCartFacade;

    @ResponseBody
    @GetMapping(value = BlControllerConstants.CATEGORY_CODE_PATH_VARIABLE_PATTERN + "/facets")
    public FacetRefinement<SearchStateData> getFacets(@PathVariable("categoryCode") final String categoryCode,
                                                      @RequestParam(value = "q", required = false) final String searchQuery,
                                                      @RequestParam(value = "page", defaultValue = "0") final int page,
                                                      @RequestParam(value = "show", defaultValue = "Page") final ShowMode showMode,
                                                      @RequestParam(value = "sort", required = false) final String sortCode) throws UnsupportedEncodingException {
        return performSearchAndGetFacets(categoryCode, searchQuery, page, showMode, sortCode);
    }

    @ResponseBody
    @GetMapping(value = BlControllerConstants.CATEGORY_CODE_PATH_VARIABLE_PATTERN + "/results")
    public SearchResultsData<ProductData> getResults(@PathVariable("categoryCode") final String categoryCode,
                                                     @RequestParam(value = "q", required = false) final String searchQuery,
                                                     @RequestParam(value = "page", defaultValue = "0") final int page,
                                                     @RequestParam(value = "show", defaultValue = "Page") final ShowMode showMode,
                                                     @RequestParam(value = "sort", required = false) final String sortCode) throws UnsupportedEncodingException {
        return performSearchAndGetResultsData(categoryCode, searchQuery, page, showMode, sortCode);
    }


    /**
     * this method is created commonly for both rental and used gear categories to fetch the products
     */
    protected String performSearchAndGetResultsPage(final String categoryCode, String searchQuery, final int page, //NOSONAR
        final ShowMode showMode, final String sortCode, final Model model, final Map<Object, Object> requestAndResponseMap) throws UnsupportedEncodingException
    {
        final CategoryModel category = getCommerceCategoryService().getCategoryForCode(categoryCode);

        // BL-268 Added For Faceted PLP & Default Sorting for PLP
        final StringBuilder configParam  = new StringBuilder();
        if(StringUtils.isBlank(searchQuery) && category.isRentalCategory()) {
                searchQuery = getDefaultSort(category, configParam, searchQuery, categoryCode);
        }
        else if(StringUtils.isBlank(searchQuery) && category.isFacetedCategory()) {
                final String categoryParam = Config.getParameter(BlCoreConstants.CATEGORY_MAP);
                if(StringUtils.isNotBlank(categoryParam)) {
                    final Map<String, String> categoryCodeMap = Splitter.on(BlCoreConstants.DELIMETER)
                        .withKeyValueSeparator(BlCoreConstants.RATIO).split(categoryParam);

                    if(StringUtils.isNotBlank(categoryCodeMap.get(categoryCode))) {
                        if(BlControllerConstants.AUDIO_CATEGORY.equalsIgnoreCase(categoryCodeMap.get(categoryCode)) ||
                            BlControllerConstants.LIGHTING_CATEGORY.equalsIgnoreCase(categoryCodeMap.get(categoryCode))) {
                            searchQuery = String.valueOf(configParam.append(getConfigParameters(BlCoreConstants.DEFAULT_SORT_CODE))
                                .append(getConfigParameters(BlCoreConstants.FACTED_USED_GEAR_CATEGORY_NAME))
                                .append(BlControllerConstants.PRODUCTION_CATEGORY));
                        }
                        else {
                        searchQuery = String.valueOf(configParam.append(getConfigParameters(BlCoreConstants.DEFAULT_SORT_CODE))
                            .append(getConfigParameters(BlCoreConstants.FACTED_USED_GEAR_CATEGORY_NAME))
                            .append(categoryCodeMap.get(categoryCode)));}
                    }
                }
        }
        final String redirection = checkRequestUrl((HttpServletRequest) requestAndResponseMap.get(BlControllerConstants.REQUEST),
            (HttpServletResponse) requestAndResponseMap.get(BlControllerConstants.RESPONSE), getCategoryModelUrlResolver().resolve(category));
        if (StringUtils.isNotEmpty(redirection))
        {
            return redirection;
        }
        final CategoryPageModel categoryPage = getCategoryPage(category);

        //BL-80 Added to get default sorting as newest for Used New Arrivals Category
        if(StringUtils.isBlank(searchQuery)) {
            if (category.getCode().startsWith(BlCoreConstants.NEW) || BlCoreConstants.USED_NEW_ARRIVALS.equalsIgnoreCase(category.getCode())) {
                searchQuery = getConfigParameters(BlCoreConstants.DEFAULT_SORT_NEWEST_CODE);
            } else {
                searchQuery = getConfigParameters(BlCoreConstants.DEFAULT_SORT_CODE);
            }
        }
        String blPageType;
        if (category.getCode().equals(BlCoreConstants.NEW_GEAR)){
            if(getSessionService().getAttribute(BlCoreConstants.ASM_SESSION_PARAMETER) == null || ((AssistedServiceSession)getSessionService().getAttribute(BlCoreConstants.ASM_SESSION_PARAMETER)).getAgent()==null){
                return BlControllerConstants.REDIRECT_TO_HOME_URL;
            }
            blPageType=BlCoreConstants.USED_GEAR_CODE;
        }else{
            blPageType= category.isRentalCategory() ? BlCoreConstants.RENTAL_GEAR : BlCoreConstants.USED_GEAR_CODE;
        }
        final CategorySearchEvaluator categorySearch = new CategorySearchEvaluator(categoryCode, searchQuery, page, showMode,
            sortCode, categoryPage , blPageType);

        ProductCategorySearchPageData<SearchStateData, ProductData, CategoryData> searchPageData = null;
        try
        {
            categorySearch.doSearch();
            searchPageData = categorySearch.getSearchPageData();
        }
        catch (final ConversionException e)
        {
            searchPageData = createEmptySearchResult(categoryCode);
        }

        populateModelAttributeForCategory(model ,categorySearch , categoryCode , searchPageData , category, showMode , (HttpServletRequest) requestAndResponseMap.get(BlControllerConstants.REQUEST));
        populateRequiredDataForCategory(model ,category ,searchPageData , (HttpServletRequest) requestAndResponseMap.get(BlControllerConstants.REQUEST) , searchQuery);

        checkNumberOfFacetSelected(searchPageData , model);
        return getViewPage(categorySearch.getCategoryPage());

    }
    /**
     *  this method is created for adding model attribute to rental and used gear category
     */
    private void addModelAttributeForRentalAndUsedCategory(final CategoryModel category, final Model model) {

        if(BlCoreConstants.NEW_GEAR.equals(category.getCode())){
            model.addAttribute(BlCoreConstants.BL_PAGE_TYPE , BlCoreConstants.USED_GEAR_CODE);
            final String currentCartType = blCartFacade.identifyCartType();
            if(StringUtils.isNotEmpty(currentCartType)){
                model.addAttribute(currentCartType,true);
            }
        }
        else if(category.isRentalCategory()){
            model.addAttribute(BlCoreConstants.BL_PAGE_TYPE, BlCoreConstants.RENTAL_GEAR);
            final String currentCartType = blCartFacade.identifyCartType();
            if(StringUtils.isNotEmpty(currentCartType)){
                model.addAttribute(currentCartType,true);
            }
        }
        else {
            model.addAttribute(BlCoreConstants.BL_PAGE_TYPE , BlCoreConstants.USED_GEAR_CODE);
        }
    }

    /**
     * this method is created for adding clear attribute for rental category
     */
    private void addClearAllModelAttribute( final Model model) {
            model.addAttribute(BlCoreConstants.CLEAR_ALL_QUERY,  BlCoreConstants.RENTAL_CLEAR_ALL);
            model.addAttribute(BlCoreConstants.SUPER_CATEGORY, BlCoreConstants.BRANDS);
    }

    /**
     * this method is created for adding clear attribute for used gear category
     */
    private void addClearAllModelAttributeForUsedGear( final Model model) {
        model.addAttribute(BlCoreConstants.CLEAR_ALL_QUERY,  BlCoreConstants.USED_GEAR_CLEAR_ALL);
        model.addAttribute(BlCoreConstants.USED_GEAR_SUPER_CATEGORY, BlCoreConstants.USED_GEAR);
    }

    /**
     * this method is created for getting default sort from properties
     */
    private String getDefaultSort(final CategoryModel category ,final StringBuilder configParam , String searchQuery ,final String categoryCode) {
        if(CollectionUtils.isEmpty(category.getSupercategories())){
            searchQuery= String.valueOf(configParam.append(getConfigParameters(BlCoreConstants.DEFAULT_SORT_CODE)));
        }
        else {
                if (category.isFacetedCategory()) {
                    searchQuery = String.valueOf(configParam.append(getConfigParameters(BlCoreConstants.DEFAULT_SORT_CODE))
                            .append(getConfigParameters(BlCoreConstants.FACTED_CATEGORY_NAME))
                            .append(categoryCode));
            }
        }
        return searchQuery;
    }

    /**
     * this method is created for getting Config parameters from properties file
     * @param configParam property key
     * @return String values
     */
    private String getConfigParameters(final String configParam) {
        final String value = Config.getParameter(configParam);
        if(StringUtils.isNotBlank(value)) {
            return value;
        }
        return BlCoreConstants.EMPTY_STRING;
    }

    // Created to add custom attribute to searchQueryData
    protected class CategorySearchEvaluator
    {
        private final String categoryCode;
        private final SearchQueryData searchQueryData = new SearchQueryData();
        private final int page;
        private final ShowMode showMode;
        private final String sortCode;
        private CategoryPageModel categoryPage;
        private boolean showCategoriesOnly;
        private ProductCategorySearchPageData<SearchStateData, ProductData, CategoryData> searchPageData;

        public CategorySearchEvaluator(final String categoryCode, final String searchQuery, final int page, final ShowMode showMode,
            final String sortCode, final CategoryPageModel categoryPage , final String blPageType)
        {
            this.categoryCode = categoryCode;
            this.searchQueryData.setBlPage(blPageType);
            this.searchQueryData.setValue(searchQuery);
            this.page = page;
            this.showMode = showMode;
            this.sortCode = sortCode;
            this.categoryPage = categoryPage;
            searchQueryData.setBlPage(blPageType);
        }

        public void doSearch()
        {
            showCategoriesOnly = false;
            if (searchQueryData.getValue() == null)
            {
                // Direct category link without filtering
                searchPageData = getProductSearchFacade().categorySearch(categoryCode);
                if (categoryPage != null)
                {
                    showCategoriesOnly = !categoryHasDefaultPage(categoryPage)
                        && CollectionUtils.isNotEmpty(searchPageData.getSubCategories());
                }
            }
            else
            {
                // We have some search filtering
                if (categoryPage == null)
                {
                    // Load the default category page
                    categoryPage = getDefaultCategoryPage();
                }

                final SearchStateData searchState = new SearchStateData();
                searchState.setQuery(searchQueryData);

                final PageableData pageableData = createPageableData(page, getSearchPageSize(), sortCode, showMode); // NOSONAR
                searchPageData = getProductSearchFacade().categorySearch(categoryCode, searchState, pageableData);

            }
            //Encode SearchPageData
            searchPageData = (ProductCategorySearchPageData) encodeSearchPageData(searchPageData);
            // removing newest sorting for used gear PLP
            if(categoryCode.toLowerCase().contains(BlControllerConstants.USED_SUBSTRING)) {
               final List<SortData> newest = searchPageData.getSorts().stream()
                   .filter(sortData -> sortData.getCode().equals(BlControllerConstants.NEWEST_STRING))
                   .collect(Collectors.toList());
               if (CollectionUtils.isNotEmpty(newest)) {
                   searchPageData.getSorts().remove(newest.get(0));
               }
           }


        }

        public int getPage()
        {
            return page;
        }

        public CategoryPageModel getCategoryPage()
        {
            return categoryPage;
        }

        public boolean isShowCategoriesOnly()
        {
            return showCategoriesOnly;
        }

        public ProductCategorySearchPageData<SearchStateData, ProductData, CategoryData> getSearchPageData()
        {
            return searchPageData;
        }
    }


    /*
     * This method created to populate required details for category
     */
    private void populateRequiredDataForCategory(final Model model , final CategoryModel category ,
        final ProductCategorySearchPageData<SearchStateData, ProductData, CategoryData> searchPageData ,final HttpServletRequest request, final String searchQuery) {
        updatePageTitle(category, model);
        // To check whether the category is Rental Gear
        addModelAttributeForRentalAndUsedCategory(category,model);

        final RequestContextData requestContextData = getRequestContextData(request);
        requestContextData.setCategory(category);
        requestContextData.setSearch(searchPageData);

        populateModelForRentalAndUsedGearCategory(model,category ,searchQuery);
        setMetaData(category ,model);
    }

    /**
     * This method create to populate model attributes
     */
    private void populateModelAttributeForCategory(final Model model , final CategorySearchEvaluator categorySearch , final String categoryCode ,
        final ProductCategorySearchPageData<SearchStateData, ProductData, CategoryData> searchPageData , final CategoryModel category , final ShowMode showMode , final HttpServletRequest request ) {
        storeCmsPageInModel(model, categorySearch.getCategoryPage());
        storeContinueUrl(request);
        populateModel(model, searchPageData, showMode);
        final boolean showCategoriesOnly = categorySearch.isShowCategoriesOnly();
        model.addAttribute(WebConstants.BREADCRUMBS_KEY, getSearchBreadcrumbBuilder().getBreadcrumbs(categoryCode, searchPageData));
        model.addAttribute("showCategoriesOnly", showCategoriesOnly);
        model.addAttribute("categoryName", category.getName());
        model.addAttribute("pageType", PageType.CATEGORY.name());
        model.addAttribute("userLocation", getCustomerLocationService().getUserLocation());
        model.addAttribute("footerContent",category.getFooterContent());
        model.addAttribute(BlCoreConstants.CLEAR_BRAND,BlCoreConstants.RENTAL_CLEAR_ALL);
        model.addAttribute(BlCoreConstants.CLEAR_USED_GEAR_CATEGORY,BlCoreConstants.USED_GEAR_CLEAR_ALL);
    }

    /**
     *This method created to populate model attribute for  rental category
     */
    private void populateModelForRentalAndUsedGearCategory(final Model model , final CategoryModel category , final String searchQuery) {

        if (searchQuery != null)
        {
            model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_FOLLOW);
        }
        // If its rental gear
        if(category.isRentalCategory() && category.isFacetedCategory()) {
            addClearAllModelAttribute(model);
        }
        else if(category.isFacetedCategory()) {
            addClearAllModelAttributeForUsedGear(model);
        }
        // Added Model attribute for rental Date duration from BlRentalDateUtils class
        if(category.isRentalCategory()) {
            model.addAttribute(BlControllerConstants.RENTAL_DATE, BlRentalDateUtils.getRentalsDuration());
        }

    }

    /**
     * This method created to set the meta keywords and to set the metdata for category
     */
    private void setMetaData(final CategoryModel category , final Model model) {
        final String metaKeywords = MetaSanitizerUtil.sanitizeKeywords(
            category.getKeywords().stream().map(KeywordModel::getKeyword).collect(
                Collectors.toSet()));
        final String metaDescription = MetaSanitizerUtil.sanitizeDescription(category.getDescription());
        setUpMetaData(model, metaKeywords, metaDescription);
    }


    /**
     * This method created to set model for clear all
     * @param searchPageData searchpagedata
     * @param model model
     */
    private void checkNumberOfFacetSelected(final ProductCategorySearchPageData<SearchStateData, ProductData, CategoryData> searchPageData , final Model model) {
        final AtomicInteger numberOfFacetSelected = new AtomicInteger();
            final List<String> facetNameList = new ArrayList<>();
            searchPageData.getFacets().forEach(searchStateDataFacetData -> searchStateDataFacetData.getValues().forEach(searchStateDataFacetValueData -> {
                if(searchStateDataFacetValueData.isSelected()) {
                    facetNameList.add(searchStateDataFacetValueData.getCode());
                    numberOfFacetSelected.getAndIncrement();
                }
            }));

            if(numberOfFacetSelected.intValue() == 1) {
                model.addAttribute(BlControllerConstants.CLEAR_BRANDS, facetNameList.get(0));
            }
    }

}


