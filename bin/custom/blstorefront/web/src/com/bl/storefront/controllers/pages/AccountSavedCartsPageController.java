/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.storefront.controllers.pages;

import com.bl.core.utils.BlRentalDateUtils;
import com.bl.facades.cart.BlSaveCartFacade;
import com.bl.facades.constants.BlFacadesConstants;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.logging.BlLogger;
import com.bl.storefront.controllers.ControllerConstants;
import de.hybris.platform.acceleratorfacades.ordergridform.OrderGridFormFacade;
import de.hybris.platform.acceleratorfacades.product.data.ReadOnlyOrderGridData;
import de.hybris.platform.acceleratorservices.enums.ImportStatus;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.RequireHardLogIn;
import de.hybris.platform.acceleratorstorefrontcommons.breadcrumb.Breadcrumb;
import de.hybris.platform.acceleratorstorefrontcommons.breadcrumb.ResourceBreadcrumbBuilder;
import de.hybris.platform.acceleratorstorefrontcommons.constants.WebConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.ThirdPartyConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.pages.AbstractSearchPageController;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.util.GlobalMessages;
import de.hybris.platform.acceleratorstorefrontcommons.forms.RestoreSaveCartForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.SaveCartForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.validation.RestoreSaveCartFormValidator;
import de.hybris.platform.acceleratorstorefrontcommons.forms.validation.SaveCartFormValidator;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.cms2.model.pages.ContentPageModel;
import de.hybris.platform.commercefacades.order.CartFacade;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commercefacades.order.data.CommerceSaveCartParameterData;
import de.hybris.platform.commercefacades.order.data.CommerceSaveCartResultData;
import de.hybris.platform.commercefacades.product.ProductFacade;
import de.hybris.platform.commercefacades.product.ProductOption;
import de.hybris.platform.commerceservices.order.CommerceSaveCartException;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import javax.annotation.Resource;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.WordUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.validation.ObjectError;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;


/**
 * Controller for saved carts page
 */
@Controller
@RequestMapping("/my-account/saved-carts")
public class AccountSavedCartsPageController extends AbstractSearchPageController
{
	private static final String MY_ACCOUNT_SAVED_CARTS_URL = "/my-account/saved-carts";
	private static final String REDIRECT_TO_SAVED_CARTS_PAGE = REDIRECT_PREFIX + MY_ACCOUNT_SAVED_CARTS_URL;

	private static final String SAVED_CARTS_CMS_PAGE = "saved-carts";
	private static final String SAVED_CART_DETAILS_CMS_PAGE = "savedCartDetailsPage";

	private static final String SAVED_CART_CODE_PATH_VARIABLE_PATTERN = "{cartCode:.*}";

	private static final String REFRESH_UPLOADING_SAVED_CART = "refresh.uploading.saved.cart";
	private static final String REFRESH_UPLOADING_SAVED_CART_INTERVAL = "refresh.uploading.saved.cart.interval";

	private static final Logger LOG = Logger.getLogger(AccountSavedCartsPageController.class);

	@Resource(name = "accountBreadcrumbBuilder")
	private ResourceBreadcrumbBuilder accountBreadcrumbBuilder;

	@Resource(name = "saveCartFacade")
	private BlSaveCartFacade saveCartFacade;

	@Resource(name = "productVariantFacade")
	private ProductFacade productFacade;

	@Resource(name = "orderGridFormFacade")
	private OrderGridFormFacade orderGridFormFacade;

	@Resource(name = "saveCartFormValidator")
	private SaveCartFormValidator saveCartFormValidator;

	@Resource(name = "cartFacade")
	private CartFacade cartFacade;

	@Resource(name = "restoreSaveCartFormValidator")
	private RestoreSaveCartFormValidator restoreSaveCartFormValidator;

	@ModelAttribute(name = "isRentalPage")
	private boolean getRentalDuration() {
		return Boolean.TRUE;
	}

	@ModelAttribute(name = BlControllerConstants.RENTAL_DATE)
	private RentalDateDto getRentalsDuration() {
		return BlRentalDateUtils.getRentalsDuration();
	}

  @GetMapping
	@RequireHardLogIn
	public String savedCarts(@RequestParam(value = "page", defaultValue = "0") final int page,
			@RequestParam(value = "show", defaultValue = "Page") final ShowMode showMode,
			@RequestParam(value = "sort", required = false) final String sortCode, final Model model)
			throws CMSItemNotFoundException
	{
		// Handle paged search results
		final PageableData pageableData = createPageableData(page, 5, sortCode, showMode); // NOSONAR
		final SearchPageData<CartData> searchPageData = saveCartFacade.getSavedCartsForCurrentUser(pageableData, null,model); //NOSONAR
		populateModel(model, searchPageData, showMode);

		model.addAttribute("refreshSavedCart", getSiteConfigService().getBoolean(REFRESH_UPLOADING_SAVED_CART, false));
		model.addAttribute("refreshSavedCartInterval", getSiteConfigService().getLong(REFRESH_UPLOADING_SAVED_CART_INTERVAL, 0));

		final ContentPageModel savedCartsPage = getContentPageForLabelOrId(SAVED_CARTS_CMS_PAGE);
		storeCmsPageInModel(model, savedCartsPage);
		setUpMetaDataForContentPage(model, savedCartsPage);
		model.addAttribute(WebConstants.BREADCRUMBS_KEY, accountBreadcrumbBuilder.getBreadcrumbs("text.account.savedCarts"));
		model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
		model.addAttribute(BlControllerConstants.SAVE_CART_FORM , new SaveCartForm());
		String removedEntries = (String)model.getAttribute(BlFacadesConstants.REMOVE_ENTRIES);
		if(StringUtils.isNotEmpty(removedEntries)) {
			removedEntries = removedEntries.substring(1);
			GlobalMessages
					.addFlashMessage((Map<String, Object>) model, GlobalMessages.CONF_MESSAGES_HOLDER,
							BlControllerConstants.DISCONTINUE_MESSAGE_KEY, new Object[]{removedEntries});
		}
		return getViewForPage(model);
	}

  @GetMapping(value = "/" + SAVED_CART_CODE_PATH_VARIABLE_PATTERN)
	@RequireHardLogIn
	public String savedCart(@PathVariable("cartCode") final String cartCode, final Model model,
			final RedirectAttributes redirectModel) throws CMSItemNotFoundException
	{
		try
		{
			final CommerceSaveCartParameterData parameter = new CommerceSaveCartParameterData();
			parameter.setCartId(cartCode);

			final CommerceSaveCartResultData resultData = saveCartFacade.getCartForCodeAndCurrentUser(parameter);
			final CartData cartData = resultData.getSavedCartData();
			if (ImportStatus.PROCESSING.equals(cartData.getImportStatus()))
			{
				return REDIRECT_TO_SAVED_CARTS_PAGE;
			}
			model.addAttribute("savedCartData", cartData);

			final SaveCartForm saveCartForm = new SaveCartForm();
			saveCartForm.setDescription(cartData.getDescription());
			saveCartForm.setName(cartData.getName());
			model.addAttribute(BlControllerConstants.SAVE_CART_FORM, saveCartForm);

			final List<Breadcrumb> breadcrumbs = accountBreadcrumbBuilder.getBreadcrumbs(null);
			breadcrumbs.add(new Breadcrumb(MY_ACCOUNT_SAVED_CARTS_URL, getMessageSource().getMessage("text.account.savedCarts",
					null, getI18nService().getCurrentLocale()), null));
			breadcrumbs.add(new Breadcrumb("#", getMessageSource().getMessage("text.account.savedCart.savedCartBreadcrumb",
					new Object[]
					{ cartData.getCode() }, "Saved Cart {0}", getI18nService().getCurrentLocale()), null));
			model.addAttribute(WebConstants.BREADCRUMBS_KEY, breadcrumbs);

		}
		catch (final CommerceSaveCartException e)
		{
			BlLogger.logMessage(LOG , Level.ERROR , "Attempted to load a saved cart that does not exist or is not visible", e);
			GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.ERROR_MESSAGES_HOLDER, "system.error.page.not.found", null);
			return REDIRECT_TO_SAVED_CARTS_PAGE;
		}
		final ContentPageModel savedCartDetailsPage = getContentPageForLabelOrId(SAVED_CART_DETAILS_CMS_PAGE);
		storeCmsPageInModel(model, savedCartDetailsPage);
		model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
		setUpMetaDataForContentPage(model, savedCartDetailsPage);
		return getViewForPage(model);
	}

  @GetMapping(value = "/uploadingCarts", produces = "application/json")
	@ResponseBody
	@RequireHardLogIn
	public List<CartData> getUploadingSavedCarts(@RequestParam("cartCodes") final List<String> cartCodes)
			throws CommerceSaveCartException
	{
		final List<CartData> result = new ArrayList<>();
		for (final String cartCode : cartCodes)
		{
			final CommerceSaveCartParameterData parameter = new CommerceSaveCartParameterData();
			parameter.setCartId(cartCode);

			final CommerceSaveCartResultData resultData = saveCartFacade.getCartForCodeAndCurrentUser(parameter);
			final CartData cartData = resultData.getSavedCartData();

			if (ImportStatus.COMPLETED.equals(cartData.getImportStatus()))
			{
				result.add(cartData);
			}
		}

		return result;
	}

  @GetMapping(value = "/" + SAVED_CART_CODE_PATH_VARIABLE_PATTERN + "/getReadOnlyProductVariantMatrix")
	@RequireHardLogIn
	public String getProductVariantMatrixForResponsive(@PathVariable("cartCode") final String cartCode,
			@RequestParam("productCode") final String productCode, final Model model, final RedirectAttributes redirectModel)
	{
		try
		{
			final CommerceSaveCartParameterData parameter = new CommerceSaveCartParameterData();
			parameter.setCartId(cartCode);

			final CommerceSaveCartResultData resultData = saveCartFacade.getCartForCodeAndCurrentUser(parameter);
			final CartData cartData = resultData.getSavedCartData();

			final Map<String, ReadOnlyOrderGridData> readOnlyMultiDMap = orderGridFormFacade.getReadOnlyOrderGridForProductInOrder(
					productCode, Arrays.asList(ProductOption.BASIC, ProductOption.CATEGORIES), cartData);
			model.addAttribute("readOnlyMultiDMap", readOnlyMultiDMap);

			return ControllerConstants.Views.Fragments.Checkout.ReadOnlyExpandedOrderForm;
		}
		catch (final CommerceSaveCartException e)
		{
			BlLogger.logMessage(LOG , Level.ERROR , "Attempted to load a saved cart that does not exist or is not visible", e);
			GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.ERROR_MESSAGES_HOLDER, "system.error.page.not.found", null);
			return REDIRECT_TO_SAVED_CARTS_PAGE + "/" + cartCode;
		}
	}

  @PostMapping(value = "/" + SAVED_CART_CODE_PATH_VARIABLE_PATTERN + "/edit")
	@RequireHardLogIn
	public String savedCartEdit(@PathVariable("cartCode") final String cartCode, final SaveCartForm form,
			final BindingResult bindingResult, final RedirectAttributes redirectModel) throws CommerceSaveCartException
	{
		// Convert cart name to title case
		final  SaveCartForm saveCartForm = new SaveCartForm();
		convertSavedCartNameToTitleCase(saveCartForm , form);
		if (bindingResult.hasErrors())
		{
			for (final ObjectError error : bindingResult.getAllErrors())
			{
				GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.ERROR_MESSAGES_HOLDER, error.getCode());
			}
			redirectModel.addFlashAttribute(BlControllerConstants.SAVE_CART_FORM, saveCartForm);
		}
		else
		{
			final CommerceSaveCartParameterData commerceSaveCartParameterData = new CommerceSaveCartParameterData();
			commerceSaveCartParameterData.setCartId(cartCode);
			commerceSaveCartParameterData.setName(saveCartForm.getName());
			commerceSaveCartParameterData.setDescription(saveCartForm.getDescription());
			commerceSaveCartParameterData.setEnableHooks(false);
			try
			{
				saveCartFacade.saveCart(commerceSaveCartParameterData);
				redirectModel.addFlashAttribute(BlControllerConstants.SAVED_CART_SUCCESS , getMessageSource().getMessage(BlControllerConstants.SAVED_CART_MESSAGE, null,
						getI18nService().getCurrentLocale()));
				redirectModel.addFlashAttribute(BlControllerConstants.RENAMED_CART_CODE , cartCode);
			}
			catch (final CommerceSaveCartException csce)
			{
				BlLogger.logMessage(LOG , Level.ERROR , csce.getMessage(), csce);
				GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.ERROR_MESSAGES_HOLDER,
						"text.account.saveCart.edit.error", new Object[]
						{ saveCartForm.getName() });
			}
		}
		return REDIRECT_TO_SAVED_CARTS_PAGE;
	}

	@GetMapping(value = "/{cartId}/restore")
	@RequireHardLogIn
	public String restoreSaveCartForId(@PathVariable(value = "cartId") final String cartId, final Model model)
			throws CommerceSaveCartException
	{
		final CommerceSaveCartParameterData parameters = new CommerceSaveCartParameterData();
		parameters.setCartId(cartId);
		final CommerceSaveCartResultData commerceSaveCartResultData = saveCartFacade.getCartForCodeAndCurrentUser(parameters);
		final boolean hasSessionCart = cartFacade.hasEntries();
		model.addAttribute("hasSessionCart", hasSessionCart);
		if (hasSessionCart)
		{
			model.addAttribute("autoGeneratedName", System.currentTimeMillis());
		}
		model.addAttribute(commerceSaveCartResultData);
		return ControllerConstants.Views.Fragments.Account.SavedCartRestorePopup;
	}

	@RequireHardLogIn
	@GetMapping(value = "/{cartId}/restorCart")
	public String postRestoreSaveCartForId(@PathVariable(value = "cartId") final String cartId,
			final RestoreSaveCartForm restoreSaveCartForm, final BindingResult bindingResult) {
		try
		{
			restoreSaveCartFormValidator.validate(restoreSaveCartForm, bindingResult);
			if (bindingResult.hasErrors())
			{
				return getMessageSource().getMessage(bindingResult.getFieldError().getCode(), null, //NOSONAR
						getI18nService().getCurrentLocale());
			}

			if (restoreSaveCartForm.getCartName() != null && !restoreSaveCartForm.isPreventSaveActiveCart()
					&& cartFacade.hasEntries())
			{
				final CommerceSaveCartParameterData commerceSaveActiveCart = new CommerceSaveCartParameterData();
				commerceSaveActiveCart.setCartId(cartFacade.getSessionCart().getCode());
				commerceSaveActiveCart.setName(restoreSaveCartForm.getCartName());
				commerceSaveActiveCart.setEnableHooks(true);
				saveCartFacade.saveCart(commerceSaveActiveCart);
			}

			final CommerceSaveCartParameterData commerceSaveCartParameterData = new CommerceSaveCartParameterData();
			commerceSaveCartParameterData.setCartId(cartId);
			commerceSaveCartParameterData.setEnableHooks(true);
			if (restoreSaveCartForm.isKeepRestoredCart())
			{
				saveCartFacade.cloneSavedCart(commerceSaveCartParameterData);
			}
			saveCartFacade.restoreSavedCart(commerceSaveCartParameterData);
		}
		catch (final CommerceSaveCartException ex)
		{
			BlLogger.logMessage(LOG , Level.ERROR , "Error while restoring the cart for cartId " + cartId + " because of " ,ex);
			return getMessageSource().getMessage("text.restore.savedcart.error", null, getI18nService().getCurrentLocale());
		}
		return BlControllerConstants.REDIRECT_CART_URL;
	}

	@GetMapping(value = "/{cartId}/delete")
	@RequireHardLogIn
	public String deleteSaveCartForId(@PathVariable(value = "cartId") final String cartId)
			throws CommerceSaveCartException
	{
		try
		{
			final CommerceSaveCartParameterData parameters = new CommerceSaveCartParameterData();
			parameters.setCartId(cartId);
			saveCartFacade.flagForDeletion(cartId);
		}
		catch (final CommerceSaveCartException ex)
		{
			BlLogger.logMessage(LOG , Level.ERROR , "Error while deleting the saved cart with cartId " + cartId + " because of " + ex);
			return getMessageSource().getMessage("text.delete.savedcart.error", null, getI18nService().getCurrentLocale());
		}
		return REDIRECT_TO_SAVED_CARTS_PAGE;
	}

	/**
	 * This method created to convert saved cart name to title case
	 * @param newForm saveCartForm to be updated
	 * @param actualForm savedCartForm from storefront
	 */
	private void convertSavedCartNameToTitleCase(final SaveCartForm newForm , final SaveCartForm actualForm) {
      newForm.setName(WordUtils.capitalize(actualForm.getName().trim().toLowerCase()));
      newForm.setDescription(actualForm.getDescription());
	}
	
}
