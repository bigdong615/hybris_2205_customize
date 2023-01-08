/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.blstocknotificationaddon.controllers.pages;

import com.bl.blstocknotificationaddon.forms.NotificationChannelForm;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.RequireHardLogIn;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.pages.AbstractPageController;
import de.hybris.platform.commercefacades.futurestock.FutureStockFacade;
import de.hybris.platform.commercefacades.product.ProductFacade;
import de.hybris.platform.commercefacades.product.ProductOption;
import de.hybris.platform.commercefacades.product.data.FutureStockData;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.customerinterestsfacades.data.ProductInterestData;
import de.hybris.platform.customerinterestsfacades.productinterest.ProductInterestFacade;
import de.hybris.platform.notificationfacades.facades.NotificationPreferenceFacade;
import de.hybris.platform.notificationservices.enums.NotificationType;
import com.bl.blstocknotificationaddon.controllers.BlstocknotificationaddonManagerControllerConstants;
import com.bl.blstocknotificationaddon.forms.StockNotificationForm;
import com.bl.blstocknotificationaddon.handlers.StockNotificationHandler;
import de.hybris.platform.util.Config;

import java.util.*;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;


@Controller
@Scope("tenant")
@RequestMapping("/my-account/my-stocknotification")
public class StockNotificationPageController extends AbstractPageController
{
	private static final String PRODUCT_CODE_PATH_VARIABLE_PATTERN = "{productCode:.*}";
	private static final String EXPIRY_DAY = "customerinterestsservices.expiryDay";
	private static final String PRODUCT_DETAIL_PAGE = "pdp";
	private static final String[] DISALLOWED_FIELDS = new String[] {};
	private static final String IS_ADDING_NOTIFICATION = "isAddingNotification";
	private static final String PRODUCT_DATA = "productData";
	private static final String CHANNAL_TYPE = "EMAIL";

	@Resource(name = "blProductInterestFacade")
	private ProductInterestFacade productInterestFacade;

	@Resource(name = "productFacade")
	private ProductFacade productFacade;

	@Resource(name = "stockNotificationHandler")
	private StockNotificationHandler stockNotificationHandler;

	@Resource(name = "notificationPreferenceFacade")
	private NotificationPreferenceFacade notificationPreferenceFacade;

	@Resource(name = "futureStockFacade")
	private FutureStockFacade futureStockFacade;


	@InitBinder
	public void initBinder(final WebDataBinder binder) {
	    binder.setDisallowedFields(DISALLOWED_FIELDS);
	}


	@PostMapping(value = "/add/" + PRODUCT_CODE_PATH_VARIABLE_PATTERN)
	@RequireHardLogIn
	public String addStockNotification(@PathVariable final String productCode,
			@ModelAttribute("stockNotificationForm")  StockNotificationForm stockNotificationForm,
									   final HttpServletRequest request,final HttpServletResponse response, final Model model)
	{
		stockNotificationForm = createFormData(); //NOSONAR
		final Optional<ProductInterestData> optional = productInterestFacade
				.getProductInterestDataForCurrentCustomer(productCode, NotificationType.BACK_IN_STOCK);

		ProductInterestData productInterest = new ProductInterestData();

		final ProductData product = productFacade.getProductForCodeAndOptions(productCode,
				Arrays.asList(ProductOption.BASIC, ProductOption.PRICE, ProductOption.CATEGORIES));
		productInterest.setProduct(product);

		if (optional.isPresent())
		{
			productInterest = optional.get();
		}

		stockNotificationHandler.prepareInterestData(stockNotificationForm, productInterest);

		productInterestFacade.saveProductInterest(productInterest);
		model.addAttribute(PRODUCT_DATA,product);
		model.addAttribute(IS_ADDING_NOTIFICATION,true);
		return BlstocknotificationaddonManagerControllerConstants.Pages.AddNotificationPage;
	}

	@PostMapping(value = "/remove/" + PRODUCT_CODE_PATH_VARIABLE_PATTERN)
	@RequireHardLogIn
	public  String removeStockNotification(@PathVariable final String productCode, final HttpServletResponse response,
			final Model model)
	{

		final Optional<ProductInterestData> optional = productInterestFacade.getProductInterestDataForCurrentCustomer(productCode,
				NotificationType.BACK_IN_STOCK);

		optional.ifPresent(x -> productInterestFacade.removeProductInterest(x));

		return BlstocknotificationaddonManagerControllerConstants.Pages.AddNotificationPage;
	}

	/**
	 * This method is created for providing form data.
	 */
	private StockNotificationForm createFormData(){
		NotificationChannelForm notificationChannelForm = new NotificationChannelForm();
		notificationChannelForm.setChannel(CHANNAL_TYPE);
		notificationChannelForm.setEnabled(true);
		List<NotificationChannelForm> notificationChannelFormList = new ArrayList<>();
		notificationChannelFormList.add(notificationChannelForm);
		StockNotificationForm stockNotificationForm = new StockNotificationForm();
		stockNotificationForm.setChannels(notificationChannelFormList);
		return stockNotificationForm;
	}
}
