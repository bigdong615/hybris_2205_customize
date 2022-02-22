/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.storefront.controllers.pages;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.enums.BlackoutDateTypeEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.GiftCardModel;
import com.bl.core.promotions.promotionengineservices.service.BlPromotionService;
import com.bl.core.services.cart.BlCartService;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.core.utils.BlRentalDateUtils;
import com.bl.core.utils.BlReplaceMentOrderUtils;
import com.bl.facades.cart.BlCartFacade;
import com.bl.facades.giftcard.BlGiftCardFacade;
import com.bl.facades.product.data.AvailabilityMessage;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.facades.shipping.BlCheckoutFacade;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import com.bl.storefront.controllers.ControllerConstants;
import com.bl.storefront.security.cookie.BlRentalDurationCookieGenerator;
import de.hybris.platform.acceleratorfacades.cart.action.CartEntryAction;
import de.hybris.platform.acceleratorfacades.cart.action.CartEntryActionFacade;
import de.hybris.platform.acceleratorfacades.cart.action.exceptions.CartEntryActionException;
import de.hybris.platform.acceleratorfacades.csv.CsvFacade;
import de.hybris.platform.acceleratorfacades.flow.impl.SessionOverrideCheckoutFlowFacade;
import de.hybris.platform.acceleratorservices.controllers.page.PageType;
import de.hybris.platform.acceleratorservices.enums.CheckoutFlowEnum;
import de.hybris.platform.acceleratorservices.enums.CheckoutPciOptionEnum;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.RequireHardLogIn;
import de.hybris.platform.acceleratorstorefrontcommons.breadcrumb.ResourceBreadcrumbBuilder;
import de.hybris.platform.acceleratorstorefrontcommons.constants.WebConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.pages.AbstractCartPageController;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.util.GlobalMessages;
import de.hybris.platform.acceleratorstorefrontcommons.forms.SaveCartForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.UpdateQuantityForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.VoucherForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.validation.SaveCartFormValidator;
import de.hybris.platform.acceleratorstorefrontcommons.util.XSSFilterUtil;
import de.hybris.platform.assistedserviceservices.utils.AssistedServiceSession;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.cms2.model.pages.ContentPageModel;
import de.hybris.platform.commercefacades.order.SaveCartFacade;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commercefacades.order.data.CartModificationData;
import de.hybris.platform.commercefacades.order.data.CommerceSaveCartParameterData;
import de.hybris.platform.commercefacades.order.data.OrderEntryData;
import de.hybris.platform.commercefacades.product.ProductFacade;
import de.hybris.platform.commercefacades.product.ProductOption;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.commercefacades.quote.data.QuoteData;
import de.hybris.platform.commercefacades.voucher.VoucherFacade;
import de.hybris.platform.commercefacades.voucher.exceptions.VoucherOperationException;
import de.hybris.platform.commerceservices.order.CommerceCartModificationException;
import de.hybris.platform.commerceservices.order.CommerceSaveCartException;
import de.hybris.platform.commerceservices.security.BruteForceAttackHandler;
import de.hybris.platform.core.enums.QuoteState;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.security.PrincipalGroupModel;
import de.hybris.platform.core.model.security.PrincipalModel;
import de.hybris.platform.enumeration.EnumerationService;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.site.BaseSiteService;
import de.hybris.platform.store.services.BaseStoreService;
import de.hybris.platform.util.Config;
import java.io.IOException;
import java.io.StringWriter;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import org.apache.commons.collections.ListUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.util.StreamUtils;
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
 * Controller for cart page
 */
@Controller
@RequestMapping(value = "/cart")
public class CartPageController extends AbstractCartPageController
{
	public static final String SHOW_CHECKOUT_STRATEGY_OPTIONS = "storefront.show.checkout.flows";
	public static final String ERROR_MSG_TYPE = "errorMsg";
	public static final String SUCCESSFUL_MODIFICATION_CODE = "success";
	public static final String SITE_QUOTES_ENABLED = "site.quotes.enabled.";
	private static final String CART_CHECKOUT_ERROR = "cart.checkout.error";

	private static final String ACTION_CODE_PATH_VARIABLE_PATTERN = "{actionCode:.*}";

	private static final String REDIRECT_CART_URL = REDIRECT_PREFIX + "/cart";
	private static final String REDIRECT_QUOTE_EDIT_URL = REDIRECT_PREFIX + "/quote/%s/edit/";
	private static final String REDIRECT_QUOTE_VIEW_URL = REDIRECT_PREFIX + "/my-account/my-quotes/%s/";
	private static final String REDIRECT_EMPTY_CART = REDIRECT_PREFIX+"/cart/emptyCart";


	private static final Logger LOG = Logger.getLogger(CartPageController.class);

	@Resource(name = "simpleBreadcrumbBuilder")
	private ResourceBreadcrumbBuilder resourceBreadcrumbBuilder;

	@Resource(name = "enumerationService")
	private EnumerationService enumerationService;

	@Resource(name = "productVariantFacade")
	private ProductFacade productFacade;

	@Resource(name = "saveCartFacade")
	private SaveCartFacade saveCartFacade;

	@Resource(name = "saveCartFormValidator")
	private SaveCartFormValidator saveCartFormValidator;

	@Resource(name = "csvFacade")
	private CsvFacade csvFacade;

	@Resource(name = "voucherFacade")
	private VoucherFacade voucherFacade;

	@Resource(name = "baseSiteService")
	private BaseSiteService baseSiteService;

	@Resource(name = "cartEntryActionFacade")
	private CartEntryActionFacade cartEntryActionFacade;

	@Resource(name = "bruteForceAttackHandler")
	private BruteForceAttackHandler bruteForceAttackHandler;

	@Resource(name ="cartFacade")
	private BlCartFacade blCartFacade;

	@Resource(name = "checkoutFacade")
	private BlCheckoutFacade checkoutFacade;

	@Resource(name = "blDatePickerService")
	private BlDatePickerService blDatePickerService;

	@Resource(name = "blCommerceStockService")
	private BlCommerceStockService blCommerceStockService;

	@Resource(name = "baseStoreService")
	private BaseStoreService baseStoreService;

	@Resource(name = "cartService")
	private BlCartService blCartService;

	@Resource(name = "blGiftCardFacade")
	private BlGiftCardFacade blGiftCardFacade;

	@Resource(name = "sessionService")
	private SessionService sessionService;

  @Resource(name ="productService")
	ProductService productService;

	@Resource(name = "blRentalDurationCookieGenerator")
	private BlRentalDurationCookieGenerator blRentalDurationCookieGenerator;

	@Resource(name = "blPromotionService")
	private BlPromotionService blPromotionService;

	@ModelAttribute("showCheckoutStrategies")
	public boolean isCheckoutStrategyVisible()
	{
		return getSiteConfigService().getBoolean(SHOW_CHECKOUT_STRATEGY_OPTIONS, false);
	}
	
	@ModelAttribute(name = BlControllerConstants.RENTAL_DATE)
	private RentalDateDto getRentalsDuration() 
	{
		return BlRentalDateUtils.getRentalsDuration();
	}

	@GetMapping
	public String showCart(final Model model) throws CMSItemNotFoundException{
		if(blCartService.isRentalCartOnly())
		{
			checkDatesIsBlackoutDate(model);
		}		
		sessionService.setAttribute(BlInventoryScanLoggingConstants.IS_PAYMENT_PAGE_VISITED, false);
		getCheckoutFacade().removeDeliveryDetails();
		CartModel cartModel = blCartService.getSessionCart();
		if(BooleanUtils.isTrue(cartModel.getIsNewGearOrder())) {
			if (getSessionService().getAttribute(BlCoreConstants.ASM_SESSION_PARAMETER) == null ||
					((AssistedServiceSession) getSessionService()
							.getAttribute(BlCoreConstants.ASM_SESSION_PARAMETER)).getAgent() == null) {
				return REDIRECT_EMPTY_CART;
			}
		}

		if (getSessionService().getAttribute(BlCoreConstants.USER_RESTRICTION) != null && CollectionUtils.isNotEmpty(cartModel.getEntries())){
			final Optional<PrincipalGroupModel> restrictedGroup = cartModel.getUser().getGroups().stream()
					.filter(group -> StringUtils.containsIgnoreCase(group.getUid(), BlCoreConstants.BL_GROUP))
					.findAny();
			if(restrictedGroup.isPresent()){
			final List<ProductModel> entryProductCodes = cartModel.getEntries().stream().map(AbstractOrderEntryModel:: getProduct).collect(Collectors.toList());
				final Set<String> userGroups = getUserGroups(entryProductCodes);
				final List<AbstractOrderEntryModel> nonRestrictedProductEntries = getNonRestrictedProductEntries(
						cartModel.getEntries());
				List<AbstractOrderEntryModel> restrictedEntries = ListUtils.subtract(cartModel.getEntries(),nonRestrictedProductEntries);
				if(userGroups.contains(restrictedGroup.get().getUid()) && CollectionUtils.isEmpty(nonRestrictedProductEntries)) {
					return REDIRECT_EMPTY_CART;
				}
				else if(userGroups.contains(restrictedGroup.get().getUid()) && CollectionUtils.isNotEmpty(restrictedEntries)){
					final String restrictedRemovedEntries = getBlCartFacade().removeRestrictedEntries(restrictedEntries, cartModel, Boolean.TRUE);
					if(StringUtils.isNotEmpty(restrictedRemovedEntries)) {
						GlobalMessages
								.addFlashMessage((Map<String, Object>) model, GlobalMessages.CONF_MESSAGES_HOLDER,
										BlControllerConstants.DISCONTINUE_MESSAGE_KEY, new Object[]{restrictedRemovedEntries});
					}
				}
			}
		}

		if(Objects.nonNull(cartModel)){
		BlReplaceMentOrderUtils.updateCartForReplacementOrder(cartModel);
		}

		String removedEntries = blCartFacade.removeDiscontinueProductFromCart(cartModel,Boolean.TRUE);
		if (cartModel != null) {
			List<GiftCardModel> giftCardModelList = cartModel.getGiftCard();
			if (CollectionUtils.isNotEmpty(giftCardModelList)) {
				blGiftCardFacade.removeAppliedGiftCardFromCartOrShippingPage(cartModel, giftCardModelList);
				model.addAttribute(BlControllerConstants.IS_GIFT_CARD_REMOVE, true);
			}
		}
		if(Objects.nonNull(cartModel) && BooleanUtils.isTrue(isCartForReplacementOrder(cartModel))){
			cartModel.setCalculated(Boolean.TRUE);
			model.addAttribute("isReplacementOrderCart" , true);
		}
		else {
			if(null != getSessionService().getAttribute(BlControllerConstants.RETURN_REQUEST)) {
				getSessionService().removeAttribute(BlControllerConstants.RETURN_REQUEST);
			}
			getBlCartFacade().recalculateCartIfRequired(); //Recalculating cart only if the rental dates has been changed by user
		}
		if(StringUtils.isNotEmpty(removedEntries)) {
			GlobalMessages
					.addFlashMessage((Map<String, Object>) model, GlobalMessages.CONF_MESSAGES_HOLDER,
							BlControllerConstants.DISCONTINUE_MESSAGE_KEY, new Object[]{removedEntries});
		}


		if(null != sessionService.getAttribute(BlControllerConstants.IS_AVALARA_EXCEPTION) && BooleanUtils.isTrue(sessionService.getAttribute(BlControllerConstants.IS_AVALARA_EXCEPTION))) {
			sessionService.removeAttribute(BlControllerConstants.IS_AVALARA_EXCEPTION);
		}

		return prepareCartUrl(model);
	}

	/**
	 * get User groups associated to products
	 * @param entryProductCodes
	 * @return
	 */
	private Set<String> getUserGroups(final List<ProductModel> entryProductCodes) {
		final Set<String> userGroups = new HashSet<>();
		for (ProductModel product : entryProductCodes) {
			if (CollectionUtils.isNotEmpty(((BlProductModel) product).getRestrictedPrincipals())) {
				for(PrincipalModel ug: ((BlProductModel) product).getRestrictedPrincipals()){
					userGroups.add(ug.getUid());
				}
			}
		}
		return userGroups;
	}

	/**
	 * Get Non restricted Entry list
	 * @param entries
	 * @return
	 */
	private List<AbstractOrderEntryModel> getNonRestrictedProductEntries(final List<AbstractOrderEntryModel> entries) {
		List<AbstractOrderEntryModel> nonRestrictedProductEntries = entries.stream().filter(entry ->
				CollectionUtils.isEmpty(((BlProductModel) entry.getProduct()).getRestrictedPrincipals()))
				.collect(Collectors.toList());
		return CollectionUtils.isEmpty(nonRestrictedProductEntries) ? Collections.emptyList() : nonRestrictedProductEntries;
	}

	/**
	 * Check selected rental dates is blackout date.
	 *
	 * @param model the model
	 */
	private void checkDatesIsBlackoutDate(final Model model)
	{
		final RentalDateDto rentalDatesFromSession = blDatePickerService.getRentalDatesFromSession();
		if(Objects.nonNull(rentalDatesFromSession) && StringUtils.isNotBlank(rentalDatesFromSession.getSelectedFromDate())
				&& StringUtils.isNotBlank(rentalDatesFromSession.getSelectedToDate()))
		{
			final Date rentalStartDate = BlDateTimeUtils.getDate(rentalDatesFromSession.getSelectedFromDate(), BlControllerConstants.DATE_FORMAT_PATTERN);
			if(blCartService.isSelectedDateIsBlackoutDate(rentalStartDate, BlackoutDateTypeEnum.RENTAL_START_DATE))
			{
				model.addAttribute(BlControllerConstants.RENTAL_START_MESSAGE,BlControllerConstants.RENTAL_START_MESSAGE_KEY);
			}
			final Date rentalEndDate = BlDateTimeUtils.getDate(rentalDatesFromSession.getSelectedToDate(), BlControllerConstants.DATE_FORMAT_PATTERN);
			if(blCartService.isSelectedDateIsBlackoutDate(rentalEndDate, BlackoutDateTypeEnum.RENTAL_END_DATE))
			{
				model.addAttribute(BlControllerConstants.RENTAL_END_MESSAGE,BlControllerConstants.RENTAL_END_MESSAGE_KEY);
				model.addAttribute(BlControllerConstants.RENTAL_TO_DATE_ARGUMENT,getRentalsDuration().getSelectedToDate());
			}
		}
	}

	protected String prepareCartUrl(final Model model) throws CMSItemNotFoundException
	{
		final Optional<String> quoteEditUrl = getQuoteUrl();
		if (quoteEditUrl.isPresent())
		{
			return quoteEditUrl.get();
		}
		else
		{
			prepareDataForPage(model);

			return getViewForPage(model);
		}
	}

	protected Optional<String> getQuoteUrl()
	{
		final QuoteData quoteData = getCartFacade().getSessionCart().getQuoteData();

		return quoteData != null
				? (QuoteState.BUYER_OFFER.equals(quoteData.getState())
						? Optional.of(String.format(REDIRECT_QUOTE_VIEW_URL, urlEncode(quoteData.getCode())))
						: Optional.of(String.format(REDIRECT_QUOTE_EDIT_URL, urlEncode(quoteData.getCode()))))
				: Optional.empty();
	}

	/**
	 * Handle the '/cart/checkout' request url. This method checks to see if the cart is valid before allowing the
	 * checkout to begin. Note that this method does not require the user to be authenticated and therefore allows us to
	 * validate that the cart is valid without first forcing the user to login. The cart will be checked again once the
	 * user has logged in.
	 *
	 * @return The page to redirect to
	 */
	@GetMapping(value = "/checkout")
	@RequireHardLogIn
	public String cartCheck(final RedirectAttributes redirectModel) throws CommerceCartModificationException
	{
		SessionOverrideCheckoutFlowFacade.resetSessionOverrides();

		if (!getCartFacade().hasEntries())
		{
			LOG.info("Missing or empty cart");

			// No session cart or empty session cart. Bounce back to the cart page.
			GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.ERROR_MESSAGES_HOLDER, "basket.error.checkout.empty.cart",
					null);
			return REDIRECT_CART_URL;
		}


		if (validateCart(redirectModel))
		{
			GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.ERROR_MESSAGES_HOLDER, CART_CHECKOUT_ERROR, null);
			return REDIRECT_CART_URL;
		}

		// Redirect to the start of the checkout flow to begin the checkout process
		// We just redirect to the generic '/checkout' page which will actually select the checkout flow
		// to use. The customer is not necessarily logged in on this request, but will be forced to login
		// when they arrive on the '/checkout' page.
		return REDIRECT_PREFIX + "/checkout";
	}

	@GetMapping(value = "/getProductVariantMatrix")
	public String getProductVariantMatrix(@RequestParam("productCode") final String productCode,
			@RequestParam(value = "readOnly", required = false, defaultValue = "false") final String readOnly, final Model model)
	{

		final ProductData productData = productFacade.getProductForCodeAndOptions(productCode,
				Arrays.asList(ProductOption.BASIC, ProductOption.CATEGORIES, ProductOption.VARIANT_MATRIX_BASE,
						ProductOption.VARIANT_MATRIX_PRICE, ProductOption.VARIANT_MATRIX_MEDIA, ProductOption.VARIANT_MATRIX_STOCK,
						ProductOption.VARIANT_MATRIX_URL));

		model.addAttribute("product", productData);
		model.addAttribute("readOnly", Boolean.valueOf(readOnly));

		return ControllerConstants.Views.Fragments.Cart.ExpandGridInCart;
	}

	// This controller method is used to allow the site to force the visitor through a specified checkout flow.
	// If you only have a static configured checkout flow then you can remove this method.
	@GetMapping(value = "/checkout/select-flow")
	@RequireHardLogIn
	public String initCheck(final Model model, final RedirectAttributes redirectModel,
			@RequestParam(value = "flow", required = false) final String flow,
			@RequestParam(value = "pci", required = false) final String pci) throws CommerceCartModificationException
	{
		SessionOverrideCheckoutFlowFacade.resetSessionOverrides();

		if (!getCartFacade().hasEntries())
		{
			LOG.info("Missing or empty cart");

			// No session cart or empty session cart. Bounce back to the cart page.
			GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.ERROR_MESSAGES_HOLDER, "basket.error.checkout.empty.cart",
					null);
			return REDIRECT_CART_URL;
		}

		// Override the Checkout Flow setting in the session
		if (StringUtils.isNotBlank(flow))
		{
			final CheckoutFlowEnum checkoutFlow = enumerationService.getEnumerationValue(CheckoutFlowEnum.class,
					StringUtils.upperCase(flow));
			SessionOverrideCheckoutFlowFacade.setSessionOverrideCheckoutFlow(checkoutFlow);
		}

		// Override the Checkout PCI setting in the session
		if (StringUtils.isNotBlank(pci))
		{
			final CheckoutPciOptionEnum checkoutPci = enumerationService.getEnumerationValue(CheckoutPciOptionEnum.class,
					StringUtils.upperCase(pci));
			SessionOverrideCheckoutFlowFacade.setSessionOverrideSubscriptionPciOption(checkoutPci);
		}

		// Redirect to the start of the checkout flow to begin the checkout process
		// We just redirect to the generic '/checkout' page which will actually select the checkout flow
		// to use. The customer is not necessarily logged in on this request, but will be forced to login
		// when they arrive on the '/checkout' page.
		return REDIRECT_PREFIX + "/checkout";
	}

	@PostMapping(value = "/entrygroups/{groupNumber}")
	public String removeGroup(@PathVariable("groupNumber") final Integer groupNumber, final Model model,
			final RedirectAttributes redirectModel)
	{
		final CartModificationData cartModification;
		try
		{
			cartModification = getCartFacade().removeEntryGroup(groupNumber);
			if (cartModification != null && !StringUtils.isEmpty(cartModification.getStatusMessage()))
			{
				GlobalMessages.addErrorMessage(model, cartModification.getStatusMessage());
			}
		}
		catch (final CommerceCartModificationException e)
		{
			LOG.error(e.getMessage(), e);
			GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.ERROR_MESSAGES_HOLDER, "basket.error.entrygroup.remove",
					new Object[]
					{ groupNumber });
		}
		return REDIRECT_CART_URL;
	}

	@PostMapping(value = "/update")
	public String updateCartQuantities(@RequestParam("entryNumber") final long entryNumber, @RequestParam("productCode") final String productCode,
			@RequestParam("removeEntry") final boolean removeEntry, final Model model,
			@Valid final UpdateQuantityForm form, final BindingResult bindingResult, final HttpServletRequest request,
			final RedirectAttributes redirectModel) throws CMSItemNotFoundException
	{
		if (bindingResult.hasErrors())
		{
			handleBindingResultError(model, bindingResult);
		}
		else if (getCartFacade().hasEntries())
		{
			try
			{
				final CartModel cartModel = blCartService.getSessionCart();
				if (removeEntry)
				{
					Optional<AbstractOrderEntryModel> findEntry = cartModel.getEntries().stream().filter(entry -> entry.getEntryNumber() == entryNumber).findFirst();
					getCartFacade().updateCartEntry(entryNumber, form.getQuantity().longValue());
					//Added condition to update gift card purchase status when remove from cart
					if(BooleanUtils.isTrue(cartModel.isGiftCardOrder()))
					{
						blCartService.updateGiftCardPurchaseStatus(cartModel);
				  }
          if(BooleanUtils.isTrue(cartModel.getIsNewGearOrder()))
          {
            blCartService.updateNewGearPurchaseStatus(cartModel);
          }
					//Added condition to change serial status when entry remove from cart
					if (BooleanUtils.isFalse(cartModel.getIsRentalCart()) && findEntry.isPresent()) // NOSONAR
					{
						blCartService.setUsedGearSerialProductStatus(null, findEntry.get());
					}
				}else if(BooleanUtils.isTrue(cartModel.getIsNewGearOrder())){
					getCartFacade().updateCartEntry(entryNumber,	form.getQuantity().longValue());
				}
				else
				{
					updateCartEntry(entryNumber, productCode, form, redirectModel);
				}			

				// Redirect to the cart page on update success so that the browser doesn't re-post again
				return getCartPageRedirectUrl();
			}
			catch (final CommerceCartModificationException ex)
			{
				LOG.warn("Couldn't update product with the entry number: " + entryNumber + ".", ex);
			}
		}

		// if could not update cart, display cart/quote page again with error
		return prepareCartUrl(model);
	}
	
	/**
	 * Update cart entry.
	 *
	 * @param entryNumber
	 *           the entry number
	 * @param productCode
	 *           the product code
	 * @param form
	 *           the form
	 * @param redirectModel
	 *           the redirect model
	 * @throws CommerceCartModificationException
	 *            the commerce cart modification exception
	 */
	private void updateCartEntry(final long entryNumber, final String productCode, final UpdateQuantityForm form,
			final RedirectAttributes redirectModel) throws CommerceCartModificationException
	{
		final RentalDateDto rentalDateDto = blDatePickerService.getRentalDatesFromSession();
		if(Objects.nonNull(rentalDateDto))
		{
			final long availableStockForProduct = getAvailableStockForProduct(rentalDateDto, productCode);
			if (availableStockForProduct <= 0)
			{
				setNextAvailableDate(entryNumber, productCode, form, redirectModel, rentalDateDto);
			}
		}
		getCartFacade().updateCartEntry(entryNumber, form.getQuantity().longValue());
	}

	/**
	 * Sets the next available date.
	 *
	 * @param entryNumber
	 *           the entry number
	 * @param productCode
	 *           the product code
	 * @param form
	 *           the form
	 * @param redirectModel
	 *           the redirect model
	 * @param rentalDateDto
	 *           the rental date dto
	 */
	private void setNextAvailableDate(final long entryNumber, final String productCode, final UpdateQuantityForm form,
			final RedirectAttributes redirectModel, final RentalDateDto rentalDateDto)
	{
		final String nextAvailabilityDate = blCommerceStockService.getNextAvailabilityDateInCheckout(productCode, rentalDateDto, null,
				form.getQuantity().intValue());
		if (StringUtils.isNotBlank(nextAvailabilityDate))
		{
			redirectModel.addFlashAttribute("entryNumber", entryNumber);
			redirectModel.addFlashAttribute("entryMessage",
					form.getQuantity().longValue() > 1
							? getMessage("cart.entry.item.availability.qty.no.stock.available",
									Arrays.asList(String.valueOf(form.getQuantity().longValue()), nextAvailabilityDate))
							: getMessage("cart.entry.item.availability.no.stock.available.till", Arrays.asList(nextAvailabilityDate)));
		}
	}

	/**
	 * Gets the available stock for product.
	 *
	 * @param rentalDateDto
	 *           the rental date dto
	 * @param productCode
	 *           the product code
	 * @return the available stock for product
	 */
	private long getAvailableStockForProduct(final RentalDateDto rentalDateDto, final String productCode)
	{
		final List<WarehouseModel> warehouseModelList = baseStoreService.getCurrentBaseStore().getWarehouses();
		final List<Date> blackOutDates = blDatePickerService.getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY);
		final Date startDay = BlDateTimeUtils.subtractDaysInRentalDates(BlControllerConstants.SKIP_TWO_DAYS, rentalDateDto.getSelectedFromDate(), blackOutDates);
		final Date endDay = BlDateTimeUtils.addDaysInRentalDates(BlControllerConstants.SKIP_TWO_DAYS, rentalDateDto.getSelectedToDate(), blackOutDates);
		BlProductModel productModel = (BlProductModel)productService.getProductForCode(productCode);
		if(productModel.isBundleProduct()){
			return blCommerceStockService.getAvailableCountForBundle(
					productModel, warehouseModelList, startDay, endDay);
		}
		return blCommerceStockService.getAvailableCount(productCode, warehouseModelList, startDay, endDay);
	}

	/**
	 * Gets the message.
	 *
	 * @param messageCode
	 *           the message code
	 * @param arguments
	 *           the arguments
	 * @return the message
	 */
	private AvailabilityMessage getMessage(final String messageCode, final List<String> arguments)
	{
		final AvailabilityMessage message = new AvailabilityMessage();
		message.setMessageCode(messageCode);
		message.setArguments(arguments);
		return message;
	}
	
	/**
	 * It handles error for BindingResult.
	 * @param model
	 * @param bindingResult
	 */
	private void handleBindingResultError(Model model, BindingResult bindingResult) {
		for (final ObjectError error : bindingResult.getAllErrors()) {
			if ("typeMismatch".equals(error.getCode())) {
				GlobalMessages.addErrorMessage(model, "basket.error.quantity.invalid");
			} else {
				GlobalMessages.addErrorMessage(model, error.getDefaultMessage());
			}
		}
	}

	@Override
	protected void prepareDataForPage(final Model model) throws CMSItemNotFoundException
	{
		super.prepareDataForPage(model);

		model.addAttribute(BlControllerConstants.VOUCHER_FORM, new VoucherForm());

		// Because DefaultSiteConfigService.getProperty() doesn't set default boolean value for undefined property,
		// this property key was generated to use Config.getBoolean() method
		final String siteQuoteProperty = SITE_QUOTES_ENABLED.concat(getBaseSiteService().getCurrentBaseSite().getUid());
		model.addAttribute("siteQuoteEnabled", Config.getBoolean(siteQuoteProperty, Boolean.FALSE));
		model.addAttribute(WebConstants.BREADCRUMBS_KEY, resourceBreadcrumbBuilder.getBreadcrumbs("breadcrumb.cart"));
		model.addAttribute("pageType", PageType.CART.name());
		final CartData cartData = getCartFacade().getSessionCart();
		model.addAttribute(BlControllerConstants.RENTAL_DATE, getRentalsDuration());
		if(Boolean.TRUE.equals(cartData.getIsRentalCart())){
			model.addAttribute(BlCoreConstants.BL_PAGE_TYPE, BlCoreConstants.RENTAL_SUMMARY_DATE);
		}
	}

	protected void addFlashMessage(final UpdateQuantityForm form, final HttpServletRequest request,
			final RedirectAttributes redirectModel, final CartModificationData cartModification)
	{
		if (cartModification.getQuantity() == form.getQuantity().longValue())
		{
			// Success

			if (cartModification.getQuantity() == 0)
			{
				// Success in removing entry
				GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.CONF_MESSAGES_HOLDER, "basket.page.message.remove");
			}
			else
			{
				// Success in update quantity
				GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.CONF_MESSAGES_HOLDER, "basket.page.message.update");
			}
		}
		else if (cartModification.getQuantity() > 0)
		{
			// Less than successful
			GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.ERROR_MESSAGES_HOLDER,
					"basket.page.message.update.reducedNumberOfItemsAdded.lowStock", new Object[]
					{ XSSFilterUtil.filter(cartModification.getEntry().getProduct().getName()), Long.valueOf(cartModification.getQuantity()), form.getQuantity(), request.getRequestURL().append(cartModification.getEntry().getProduct().getUrl()) });
		}
		else
		{
			// No more stock available
			GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.ERROR_MESSAGES_HOLDER,
					"basket.page.message.update.reducedNumberOfItemsAdded.noStock", new Object[]
					{ XSSFilterUtil.filter(cartModification.getEntry().getProduct().getName()), request.getRequestURL().append(cartModification.getEntry().getProduct().getUrl()) });
		}
	}

	@SuppressWarnings("boxing")
	@ResponseBody
	@PostMapping(value = "/updateMultiD")
	public CartData updateCartQuantitiesMultiD(@RequestParam("entryNumber") final Integer entryNumber,
			@RequestParam("productCode") final String productCode, final Model model, @Valid final UpdateQuantityForm form,
			final BindingResult bindingResult)
	{
		if (bindingResult.hasErrors())
		{
			for (final ObjectError error : bindingResult.getAllErrors())
			{
				if ("typeMismatch".equals(error.getCode()))
				{
					GlobalMessages.addErrorMessage(model, "basket.error.quantity.invalid");
				}
				else
				{
					GlobalMessages.addErrorMessage(model, error.getDefaultMessage());
				}
			}
		}
		else
		{
			try
			{
				final CartModificationData cartModification = getCartFacade()
						.updateCartEntry(getOrderEntryData(form.getQuantity(), productCode, entryNumber));
				if (cartModification.getStatusCode().equals(SUCCESSFUL_MODIFICATION_CODE))
				{
					GlobalMessages.addMessage(model, GlobalMessages.CONF_MESSAGES_HOLDER, cartModification.getStatusMessage(), null);
				}
				else if (!model.containsAttribute(ERROR_MSG_TYPE))
				{
					GlobalMessages.addMessage(model, GlobalMessages.ERROR_MESSAGES_HOLDER, cartModification.getStatusMessage(), null);
				}
			}
			catch (final CommerceCartModificationException ex)
			{
				LOG.warn("Couldn't update product with the entry number: " + entryNumber + ".", ex);
			}

		}
		return getCartFacade().getSessionCart();
	}

	@SuppressWarnings("boxing")
	protected OrderEntryData getOrderEntryData(final long quantity, final String productCode, final Integer entryNumber)
	{
		final OrderEntryData orderEntry = new OrderEntryData();
		orderEntry.setQuantity(quantity);
		orderEntry.setProduct(new ProductData());
		orderEntry.getProduct().setCode(productCode);
		orderEntry.setEntryNumber(entryNumber);
		return orderEntry;
	}

	@PostMapping(value = "/save")
	@RequireHardLogIn
	public String saveCart(final SaveCartForm form, final BindingResult bindingResult, final RedirectAttributes redirectModel)
			throws CommerceSaveCartException
	{
		saveCartFormValidator.validate(form, bindingResult);
		if (bindingResult.hasErrors())
		{
			for (final ObjectError error : bindingResult.getAllErrors())
			{
				GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.ERROR_MESSAGES_HOLDER, error.getCode());
			}
			redirectModel.addFlashAttribute("saveCartForm", form);
		}
		else
		{
			final CommerceSaveCartParameterData commerceSaveCartParameterData = new CommerceSaveCartParameterData();
			commerceSaveCartParameterData.setName(form.getName().trim());
			commerceSaveCartParameterData.setDescription(form.getDescription());
			commerceSaveCartParameterData.setEnableHooks(true);
			try
			{
				saveCartFacade.saveCart(commerceSaveCartParameterData);
			}
			catch (final CommerceSaveCartException csce)
			{
				BlLogger.logMessage(LOG , Level.DEBUG , "Error while saveCart method ", csce);
			}
		}
		return BlControllerConstants.REDIRECT_TO_SAVED_CARTS_PAGE;
	}

	@GetMapping(value = "/export", produces = "text/csv")
	public String exportCsvFile(final HttpServletResponse response, final RedirectAttributes redirectModel) throws IOException
	{
		response.setHeader("Content-Disposition", "attachment;filename=cart.csv");

		try (final StringWriter writer = new StringWriter())
		{
			try
			{
				final List<String> headers = new ArrayList<>();
				headers.add(getMessageSource().getMessage("basket.export.cart.item.sku", null, getI18nService().getCurrentLocale()));
				headers.add(
						getMessageSource().getMessage("basket.export.cart.item.quantity", null, getI18nService().getCurrentLocale()));
				headers.add(getMessageSource().getMessage("basket.export.cart.item.name", null, getI18nService().getCurrentLocale()));
				headers
						.add(getMessageSource().getMessage("basket.export.cart.item.price", null, getI18nService().getCurrentLocale()));

				final CartData cartData = getCartFacade().getSessionCartWithEntryOrdering(false);
				csvFacade.generateCsvFromCart(headers, true, cartData, writer);

				StreamUtils.copy(writer.toString(), StandardCharsets.UTF_8, response.getOutputStream());
			}
			catch (final IOException e)
			{
				LOG.error(e.getMessage(), e);
				GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.ERROR_MESSAGES_HOLDER, "basket.export.cart.error", null);

				return REDIRECT_CART_URL;
			}

		}

		return null;
	}

	@PostMapping(value = "/voucher/apply")
	public String applyVoucherAction(@Valid final VoucherForm form, final BindingResult bindingResult,
			final HttpServletRequest request, final RedirectAttributes redirectAttributes)
	{
		try
		{
			if (bindingResult.hasErrors())
			{
				redirectAttributes.addFlashAttribute(ERROR_MSG_TYPE,
						getMessageSource().getMessage("coupon.invalid.code.provided", null, getI18nService().getCurrentLocale()));
			}
			else
			{
				final String ipAddress = request.getRemoteAddr();
				if (bruteForceAttackHandler.registerAttempt(ipAddress + "_voucher"))
				{
					redirectAttributes.addFlashAttribute("disableUpdate", Boolean.valueOf(true));
					redirectAttributes.addFlashAttribute(ERROR_MSG_TYPE,
							getMessageSource().getMessage("coupon.invalid.code.provided", null, getI18nService().getCurrentLocale()));
				}
				else
				{
					voucherFacade.applyVoucher(form.getVoucherCode());
					redirectAttributes.addFlashAttribute("successMsg",
							getMessageSource().getMessage("text.voucher.apply.applied.success", new Object[]
							{ form.getVoucherCode() }, getI18nService().getCurrentLocale()));
				}
			}
		}
		catch (final VoucherOperationException e)
		{
			redirectAttributes.addFlashAttribute(BlControllerConstants.VOUCHER_FORM, form);
			redirectAttributes.addFlashAttribute(ERROR_MSG_TYPE,
					getMessageSource().getMessage(e.getMessage(), null,
							getMessageSource().getMessage("coupon.invalid.code.provided", null, getI18nService().getCurrentLocale()),
							getI18nService().getCurrentLocale()));
			if (LOG.isDebugEnabled())
			{
				LOG.debug(e.getMessage(), e);
			}

		}

		return getRedirectUrlForCoupon(request);
	}

	@PostMapping(value = "/voucher/remove")
	public String removeVoucher(@Valid final VoucherForm form, final RedirectAttributes redirectModel , final HttpServletRequest request, final HttpServletResponse response)
	{
		try
		{
			if(blPromotionService.isFreeDayCouponPromoApplied(blCartService.getSessionCart())) {
				blRentalDurationCookieGenerator.removeCookie(response);
				blRentalDurationCookieGenerator.addCookie(response, getRentalsDuration().getNumberOfDays());
			}
			voucherFacade.releaseVoucher(form.getVoucherCode());
		}
		catch (final VoucherOperationException e)
		{
			GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.ERROR_MESSAGES_HOLDER, "text.voucher.release.error",
					new Object[]
					{ form.getVoucherCode() });
			if (LOG.isDebugEnabled())
			{
				LOG.debug(e.getMessage(), e);
			}

		}

		return getRedirectUrlForCoupon(request);
	}

	@Override
	public BaseSiteService getBaseSiteService()
	{
		return baseSiteService;
	}

	public void setBaseSiteService(final BaseSiteService baseSiteService)
	{
		this.baseSiteService = baseSiteService;
	}

	@PostMapping(value = "/entry/execute/" + ACTION_CODE_PATH_VARIABLE_PATTERN)
	public String executeCartEntryAction(@PathVariable(value = "actionCode", required = true) final String actionCode,
			final RedirectAttributes redirectModel, @RequestParam("entryNumbers") final Long[] entryNumbers)
	{
		CartEntryAction action = null;
		try
		{
			action = CartEntryAction.valueOf(actionCode);
		}
		catch (final IllegalArgumentException e)
		{
			LOG.error(String.format("Unknown cart entry action %s", actionCode), e);
			GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.ERROR_MESSAGES_HOLDER, "basket.page.entry.unknownAction");
			return getCartPageRedirectUrl();
		}

		try
		{
			final Optional<String> redirectUrl = cartEntryActionFacade.executeAction(action, Arrays.asList(entryNumbers));
			final Optional<String> successMessageKey = cartEntryActionFacade.getSuccessMessageKey(action);
			if (successMessageKey.isPresent())
			{
				GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.CONF_MESSAGES_HOLDER, successMessageKey.get());
			}
			if (redirectUrl.isPresent())
			{
				return redirectUrl.get();
			}
			else
			{
				return getCartPageRedirectUrl();
			}
		}
		catch (final CartEntryActionException e)
		{
			LOG.error(String.format("Failed to execute action %s", action), e);
			final Optional<String> errorMessageKey = cartEntryActionFacade.getErrorMessageKey(action);
			if (errorMessageKey.isPresent())
			{
				GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.ERROR_MESSAGES_HOLDER, errorMessageKey.get());
			}
			return getCartPageRedirectUrl();
		}
	}

	protected String getCartPageRedirectUrl()
	{
		final QuoteData quoteData = getCartFacade().getSessionCart().getQuoteData();
		return quoteData != null ? String.format(REDIRECT_QUOTE_EDIT_URL, urlEncode(quoteData.getCode())) : REDIRECT_CART_URL;
	}

	/**
	 * This method will remove all the cart items from cart page.
	 *
	 * @param model              the model
	 * @param redirectAttributes the redirect attributes
	 * @return the string
	 */
	@GetMapping(value = "/emptyCart")
	public String emptyCart(final Model model, final RedirectAttributes redirectAttributes) {
		try {
			getBlCartFacade().removeCartEntries();
		} catch (final Exception exception) {
			BlLogger.logMessage(LOG, Level.ERROR, "Unable to remove cart entries:", exception);
			GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER,
					"text.page.cart.clear.fail");
		}
		return REDIRECT_CART_URL;
	}

	/**
	 * BL-466 It will return cart/empty cart page based on cart entries.
	 * @param model
	 * @throws CMSItemNotFoundException
	 */
	@Override
	protected void createProductList(final Model model) throws CMSItemNotFoundException {
		final ContentPageModel contentPageModel;
		final CartData cartData = getBlCartFacade().getSessionCartWithEntryOrdering(false);
		if(BooleanUtils.isTrue(cartData.getIsRentalCart()))
		{
			getBlCartFacade().checkAvailabilityForRentalCart(cartData);
			getBlCartFacade().checkAquatechRentalDates(cartData);
		}
		if (CollectionUtils.isEmpty(cartData.getEntries())) {
			contentPageModel = getContentPageForLabelOrId(BlControllerConstants.EMPTY_CART_CMS_PAGE_LABEL);
			model.addAttribute(BlControllerConstants.CART_DATA, cartData);
			model.addAttribute(BlControllerConstants.PICKUP_CART_ENTRIES, Boolean.FALSE);
		} else {
			createProductEntryList(model, cartData);
			contentPageModel = getContentPageForLabelOrId(BlControllerConstants.CART_CMS_PAGE_LABEL);
		}		
		storeCmsPageInModel(model, contentPageModel);
		setUpMetaDataForContentPage(model, contentPageModel);
	}
	
	/**
	 * Update cart entry with the selected damage Waiver on cart page.
	 *
	 * @param entryNumber the entry number
	 * @param damageWaiverType the damage Waiver type
	 * @param model the model
	 * @param request the request
	 * @param redirectModel the redirect model
	 * @return the string
	 * @throws CMSItemNotFoundException the CMS item not found exception
	 */
	@PostMapping(path="/updateDamageWaiver")
	public String updateCartEntryDamageWaiver(@RequestParam("entryNumber") final long entryNumber, 
			@RequestParam("damageWaiverType") final String damageWaiverType, final Model model,
			final HttpServletRequest request, final RedirectAttributes redirectModel) throws CMSItemNotFoundException
	{
		try
		{	
			getBlCartFacade().updateCartEntryDamageWaiver(entryNumber, damageWaiverType);
			return getCartPageRedirectUrl();
		}
		catch (final Exception exception)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, LogErrorCodeEnum.CART_INTERNAL_ERROR.getCode(), exception,
					"Error while updating Damage Waiver with the entry number : {}", entryNumber);
			GlobalMessages.addErrorMessage(model, "text.page.cart.update.damage.waiver.fail");
		}
		return prepareCartUrl(model);
	}
	
	/**
	 * Update cart entry with the selected options on cart page.
	 *
	 * @param entryNumber the entry number
	 * @param bloptions the bloptions
	 * @param model the model
	 * @param request the request
	 * @param redirectModel the redirect model
	 * @return the string
	 * @throws CMSItemNotFoundException the CMS item not found exception
	 */
	@PostMapping(path="/updateBlOptions")
	public String updateCartEntryBlOptions(@RequestParam("entryNumber") final long entryNumber, 
			@RequestParam("bloptions") final String bloptions, final Model model,
			final HttpServletRequest request, final RedirectAttributes redirectModel) throws CMSItemNotFoundException
	{
		try
		{	
			getBlCartFacade().updateCartEntrySelectedOption(entryNumber, bloptions);
			return getCartPageRedirectUrl();
		}
		catch (final Exception exception)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, LogErrorCodeEnum.CART_INTERNAL_ERROR.getCode(), exception,
					"Error while updating Damage Waiver with the entry number : {}", entryNumber);
			GlobalMessages.addErrorMessage(model, "text.page.cart.update.damage.waiver.fail");
		}
		return prepareCartUrl(model);
	}
	
	/**
	 * Update product quantity based on the quantity selected from rental add to cart popup.
	 *
	 * @param entryNumber
	 * @param model
	 * @param form
	 * @param bindingResult
	 * @param request
	 * @return
	 * @throws CMSItemNotFoundException
	 */
	@PostMapping(value = "/updateQuantity")
	@ResponseBody
	public void updateQuantity(@RequestParam("entryNumber") final long entryNumber, final Model model,
			@Valid final UpdateQuantityForm form, final BindingResult bindingResult,
			final HttpServletRequest request) throws CommerceCartModificationException {
		if (bindingResult.hasErrors()) {
			handleBindingResultError(model, bindingResult);
		} else if (getCartFacade().hasEntries()) {
			try {
				getBlCartFacade().updateCartEntryFromPopup(entryNumber,
						form.getQuantity().longValue());

				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
						"Product quantity: {} updated successfully for cart entry: {}", form.getQuantity(),
						entryNumber);

			} catch (final CommerceCartModificationException exception) {
				BlLogger
						.logFormattedMessage(LOG, Level.ERROR, LogErrorCodeEnum.CART_INTERNAL_ERROR.getCode(),
								exception,
								"Couldn't update product with the entry number: {}", entryNumber);
			}
		}
	}

	/**
	 * Check if date range not selected and stock is not available for any product which are present
	 * on current cart, then don't redirect to next(2nd) step.
	 *
	 * @param model
	 * @return success or failure
	 */
	@GetMapping(value = "/checkDateAndStock")
	@ResponseBody
	public String checkDateRangeAndStock(final Model model,final RedirectAttributes redirectModel)
	{
		final	CartModel cartModel = blCartService.getSessionCart();
		List<Integer> entryList = blCartFacade.getDiscontinueEntryList(cartModel,new StringBuilder());
		if(CollectionUtils.isNotEmpty(entryList)) {
			return REDIRECT_CART_URL;
		}
		final RentalDateDto rentalDateDto = blDatePickerService.getRentalDatesFromSession();
		if (rentalDateDto == null)
		{
			return BlControllerConstants.RENTAL_DATE_FAILURE_RESULT;
		}
		else if(blCartService.isRentalCartOnly() && (blCartService.isSelectedDateIsBlackoutDate(BlDateTimeUtils.getDate(rentalDateDto.getSelectedFromDate(),
				BlControllerConstants.DATE_FORMAT_PATTERN), BlackoutDateTypeEnum.RENTAL_START_DATE)
				|| blCartService.isSelectedDateIsBlackoutDate(BlDateTimeUtils.getDate(rentalDateDto.getSelectedToDate(),
						BlControllerConstants.DATE_FORMAT_PATTERN), BlackoutDateTypeEnum.RENTAL_END_DATE)))
		{
			return BlControllerConstants.BLACKOUT_DATE_FOUND;
		}
		else
		{
			final Date startDate = BlDateTimeUtils.convertStringDateToDate(rentalDateDto.getSelectedFromDate(),
					BlControllerConstants.DATE_FORMAT_PATTERN);
			final Date endDate = BlDateTimeUtils.convertStringDateToDate(rentalDateDto.getSelectedToDate(),
					BlControllerConstants.DATE_FORMAT_PATTERN);
			if(null == cartModel.getRentalStartDate() && null == cartModel.getRentalEndDate()) {
				getBlCartFacade().setRentalDatesOnCart(startDate, endDate);
			} else if(!(cartModel.getRentalStartDate().compareTo(startDate) == 0) ||
					!(cartModel.getRentalEndDate().compareTo(endDate) == 0)) {
				getBlCartFacade().setRentalDatesOnCart(startDate, endDate);
			}

			final String currentDateString = BlDateTimeUtils.convertDateToStringDate(new Date(),
					BlCoreConstants.SQL_DATE_FORMAT);
			final int daysDifference = BlDateTimeUtils.getDaysBetweenBusinessDays(currentDateString,
					rentalDateDto.getSelectedFromDate());

			if ((blCartService.isAquatechProductsPresentInCart()
					&& daysDifference < BlCoreConstants.TWO_DAYS) || BooleanUtils
					.negate(getBlCartFacade().checkAvailabilityOnCartContinue(rentalDateDto))) {
				return BlControllerConstants.STOCK_FAILURE_RESULT;
			}
		}
		return BlControllerConstants.SUCCESS;
	}

	/**
	 * This method created to decide url to redirect on apply or remove of coupon using referer
	 *
	 */
	private String getRedirectUrlForCoupon(final HttpServletRequest request) {

		final String referer = request.getHeader(BlControllerConstants.REFERER);

		if (referer.contains(BlControllerConstants.DELIVERY_METHOD_CHECKOUT_URL))
		{
			return REDIRECT_PREFIX + BlControllerConstants.DELIVERY_METHOD_CHECKOUT_URL;
		}
		else if(referer.contains(BlControllerConstants.PAYMENT_METHOD_CHECKOUT_URL)) {

			return REDIRECT_PREFIX + BlControllerConstants.PAYMENT_METHOD_CHECKOUT_URL;
		}

		return REDIRECT_CART_URL;
	}

	/**
	 * Remove cart entries when UsedGear cart timer end
	 *
	 * @param isUsedGearTimerEnd
	 * @return
	 */
	@PostMapping(value = "/cartTimerOut", produces = "application/json")
	public String usedGearCartSessionTimeOut(
			@RequestParam(value = "usedGearTimerEnd", required = false, defaultValue = "true") final boolean isUsedGearTimerEnd)

	{
		final CartData cartData = getCartFacade().getSessionCart();
		if (isUsedGearTimerEnd && BooleanUtils.isFalse(cartData.getIsRentalCart()) && BooleanUtils.isFalse(cartData.getHasGiftCart())
		 && CollectionUtils.isNotEmpty(cartData.getEntries()))
		{
			getBlCartFacade().removeCartEntries();
			return REDIRECT_CART_URL;
		}
			return StringUtils.EMPTY;
	}
	
	@GetMapping(value = "/reviewPrint")
	  public String print(final HttpServletRequest request, final Model model) {
		  try {
			  final CartData cartData = getCartFacade().getSessionCart();
			  getCheckoutFacade().getModifiedTotalForPrintQuote(cartData);
			  model.addAttribute(BlControllerConstants.CART_DATA, cartData);
			  setFormattedRentalDates(model);
			  model.addAttribute(BlControllerConstants.FROM_PAGE, BlControllerConstants.CART_PAGE);
			  return ControllerConstants.Views.Pages.MultiStepCheckout.ReviewPrint;			  
		  }
		  catch(final Exception exception) {
			  BlLogger.logMessage(LOG, Level.ERROR, "Error while creating data for Print Page from Cart page", exception);
		  }
		  return REDIRECT_CART_URL;
	}
	
	/**
	 * Sets the formatted rental dates on print quote page.
	 *
	 * @param model the new formatted rental dates
	 */
	private void setFormattedRentalDates(final Model model)
	{
		final RentalDateDto rentalDateDto = blDatePickerService.getRentalDatesFromSession();
		if(Objects.nonNull(rentalDateDto))
		{
			final String formattedRentalStartDate = getFormattedDate(BlDateTimeUtils.getDate(rentalDateDto.getSelectedFromDate(),
					BlControllerConstants.DATE_FORMAT_PATTERN));
			model.addAttribute(BlControllerConstants.FORMATTED_RENTAL_START_DATE,formattedRentalStartDate);
			final String formattedRentalEndDate = getFormattedDate(BlDateTimeUtils.getDate(rentalDateDto.getSelectedToDate(),
			    BlControllerConstants.DATE_FORMAT_PATTERN));
			model.addAttribute(BlControllerConstants.FORMATTED_RENTAL_END_DATE,formattedRentalEndDate);
		}
	}

	/**
	 * Gets the formatted date in EEEE, MMM d format.
	 * Example - Wednesday, Jan 31
	 *
	 * @param date the date
	 * @return the formatted date
	 */
	private String getFormattedDate(final Date date)
	{
		return BlDateTimeUtils.convertDateToStringDate(date, BlControllerConstants.REVIEW_PAGE_DATE_FORMAT);
	}

	private boolean isCartForReplacementOrder(final CartModel cartModel) {
		return  BlReplaceMentOrderUtils.isReplaceMentOrder() &&
				Objects.nonNull(cartModel.getReturnRequestForOrder()) && null != getSessionService().getAttribute(BlCoreConstants.RETURN_REQUEST);
	}

	/**
	 * @return the blCartFacade
	 */
	public BlCartFacade getBlCartFacade()
	{
		return blCartFacade;
	}

	/**
	 * @param blCartFacade the blCartFacade to set
	 */
	public void setBlCartFacade(BlCartFacade blCartFacade)
	{
		this.blCartFacade = blCartFacade;
	}

	@Override
	public BlCheckoutFacade getCheckoutFacade() {
		return checkoutFacade;
	}

	public void setCheckoutFacade(BlCheckoutFacade checkoutFacade) {
		this.checkoutFacade = checkoutFacade;
	}
}
