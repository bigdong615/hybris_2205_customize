/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.storefront.controllers.cms;

import de.hybris.platform.acceleratorcms.model.components.ProductReferencesComponentModel;
import de.hybris.platform.catalog.enums.ProductReferenceTypeEnum;
import de.hybris.platform.catalog.model.ProductReferenceModel;
import de.hybris.platform.commercefacades.product.ProductFacade;
import de.hybris.platform.commercefacades.product.ProductOption;
import de.hybris.platform.commercefacades.product.data.ProductReferenceData;
import de.hybris.platform.core.model.product.ProductModel;
import com.bl.storefront.controllers.ControllerConstants;

import java.util.Arrays;
import java.util.List;

import java.util.stream.Collectors;
import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;

import de.hybris.platform.enumeration.EnumerationService;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;


/**
 * Controller for CMS ProductReferencesComponent
 */
@Controller("ProductReferencesComponentController")
@RequestMapping(value = ControllerConstants.Actions.Cms.ProductReferencesComponent)
public class ProductReferencesComponentController extends
        AbstractAcceleratorCMSComponentController<ProductReferencesComponentModel> {
    protected static final List<ProductOption> PRODUCT_OPTIONS = Arrays.asList(ProductOption.BASIC, ProductOption.PRICE, ProductOption.REQUIRED_DATA, ProductOption.GALLERY , ProductOption.STOCK);

    @Resource(name = "productVariantFacade")
    private ProductFacade productFacade;

    @Override
    protected void fillModel(final HttpServletRequest request, final Model model, final ProductReferencesComponentModel component) {
        final ProductModel currentProduct = getRequestContextData(request).getProduct();
        if (currentProduct != null) {
            List<ProductReferenceTypeEnum> productReferenceTypeEnum = currentProduct.getProductReferences().stream()
                .map(ProductReferenceModel::getReferenceType).collect(Collectors.toList());
            final List<ProductReferenceData> productReferences = productFacade.getProductReferencesForCode(currentProduct.getCode(),
                productReferenceTypeEnum, PRODUCT_OPTIONS, component.getMaximumNumberProducts());

            model.addAttribute("title", component.getTitle());
            model.addAttribute("productReferences", productReferences);
        }
    }

}
