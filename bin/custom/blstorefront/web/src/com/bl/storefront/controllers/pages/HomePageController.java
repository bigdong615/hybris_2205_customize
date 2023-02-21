/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.storefront.controllers.pages;

import de.hybris.platform.acceleratorservices.storefront.data.MetaElementData;
import de.hybris.platform.acceleratorstorefrontcommons.constants.WebConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.pages.AbstractPageController;
import de.hybris.platform.acceleratorstorefrontcommons.util.XSSFilterUtil;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.cms2.model.pages.AbstractPageModel;
import de.hybris.platform.cms2.model.pages.ContentPageModel;

import java.util.List;
import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.utils.BlRentalDateUtils;
import com.bl.facades.cart.BlCartFacade;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.facades.subscription.BlEmailSubscriptionFacade;

/**
 * Controller for home page
 */
@Controller
@RequestMapping("/")
public class HomePageController extends AbstractPageController
{
	private static final String LOGOUT = "logout";
	private static final String ISRENTAL_CART = "isRentalCart";

	@Resource(name = "cartFacade")
	private BlCartFacade blCartFacade;

	@Resource(name = "blEmailSubscriptionFacade")
	private BlEmailSubscriptionFacade blEmailSubscriptionFacade;

	@Value("${bl.google.site.verification}")
	private String googleSiteVerification;

	@ModelAttribute(name = BlControllerConstants.RENTAL_DATE)
	private RentalDateDto getRentalsDuration()
	{
		return BlRentalDateUtils.getRentalsDuration();
	}

	@GetMapping
	public String home(@RequestParam(value = WebConstants.CLOSE_ACCOUNT, defaultValue = "false") final boolean closeAcc,
			@RequestParam(value = LOGOUT, defaultValue = "false") final boolean logout, final Model model,
			final RedirectAttributes redirectModel,final HttpServletRequest request) throws CMSItemNotFoundException
	{
		if (logout)
		{
			return REDIRECT_PREFIX + ROOT;
		}
		final ContentPageModel contentPage = getContentPageForLabelOrId(null);
		storeCmsPageInModel(model, contentPage);
		setUpMetaDataForContentPage(model, contentPage);
		if(StringUtils.isNotEmpty(googleSiteVerification)){
			final List<MetaElementData> metadata = (List<MetaElementData>) model.getAttribute("metatags");
			metadata.add(createMetaElement("google-site-verification", googleSiteVerification));
		}
		updatePageTitle(model, contentPage);
		model.addAttribute(BlCoreConstants.BL_PAGE_TYPE, BlCoreConstants.RENTAL_GEAR);
		model.addAttribute(blCartFacade.identifyCartType(),true);
		return getViewForPage(model);
	}

	protected void updatePageTitle(final Model model, final AbstractPageModel cmsPage)
	{
		storeContentPageTitleInModel(model, getPageTitleResolver().resolveHomePageTitle(cmsPage.getTitle()));
	}

	@GetMapping(value = "/subscribe-email")
	public void subscribeEmail(@RequestParam("emailId") final String emailId, final Model model,
			final HttpServletResponse response) {

		XSSFilterUtil.filter(emailId);
		blEmailSubscriptionFacade.subscribe(emailId);

	}
}
