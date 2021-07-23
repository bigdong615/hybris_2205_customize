/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.storefront.controllers.pages;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.services.cart.BlCartService;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.utils.BlExtendOrderUtils;
import com.bl.core.utils.BlRentalDateUtils;
import com.bl.facades.coupon.impl.DefaultBlCouponFacade;
import com.bl.facades.order.BlOrderFacade;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.facades.wishlist.BlWishListFacade;
import com.bl.facades.wishlist.data.Wishlist2EntryData;
import com.bl.logging.BlLogger;
import com.bl.storefront.controllers.ControllerConstants;
import com.bl.storefront.controllers.ControllerConstants.Views.Pages.Account;
import com.bl.storefront.forms.BlAddressForm;
import com.braintree.facade.BrainTreeUserFacade;
import com.braintree.facade.impl.BrainTreeCheckoutFacade;
import com.braintree.transaction.service.BrainTreeTransactionService;
import de.hybris.platform.acceleratorfacades.ordergridform.OrderGridFormFacade;
import de.hybris.platform.acceleratorfacades.product.data.ReadOnlyOrderGridData;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.RequireHardLogIn;
import de.hybris.platform.acceleratorstorefrontcommons.breadcrumb.Breadcrumb;
import de.hybris.platform.acceleratorstorefrontcommons.breadcrumb.ResourceBreadcrumbBuilder;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.ThirdPartyConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.pages.AbstractSearchPageController;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.util.GlobalMessages;
import de.hybris.platform.acceleratorstorefrontcommons.forms.AddressForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.UpdateEmailForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.UpdatePasswordForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.UpdateProfileForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.VoucherForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.validation.AddressValidator;
import de.hybris.platform.acceleratorstorefrontcommons.forms.validation.EmailValidator;
import de.hybris.platform.acceleratorstorefrontcommons.forms.validation.PasswordValidator;
import de.hybris.platform.acceleratorstorefrontcommons.forms.validation.ProfileValidator;
import de.hybris.platform.acceleratorstorefrontcommons.forms.verification.AddressVerificationResultHandler;
import de.hybris.platform.acceleratorstorefrontcommons.util.AddressDataUtil;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.cms2.model.pages.ContentPageModel;
import de.hybris.platform.commercefacades.address.AddressVerificationFacade;
import de.hybris.platform.commercefacades.address.data.AddressVerificationResult;
import de.hybris.platform.commercefacades.consent.CustomerConsentDataStrategy;
import de.hybris.platform.commercefacades.customer.CustomerFacade;
import de.hybris.platform.commercefacades.i18n.I18NFacade;
import de.hybris.platform.commercefacades.order.CheckoutFacade;
import de.hybris.platform.commercefacades.order.data.CCPaymentInfoData;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.order.data.OrderHistoryData;
import de.hybris.platform.commercefacades.product.ProductOption;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.commercefacades.user.data.CountryData;
import de.hybris.platform.commercefacades.user.data.CustomerData;
import de.hybris.platform.commercefacades.user.data.TitleData;
import de.hybris.platform.commercefacades.user.exceptions.PasswordMismatchException;
import de.hybris.platform.commercefacades.voucher.VoucherFacade;
import de.hybris.platform.commercefacades.voucher.exceptions.VoucherOperationException;
import de.hybris.platform.commerceservices.address.AddressVerificationDecision;
import de.hybris.platform.commerceservices.consent.exceptions.CommerceConsentGivenException;
import de.hybris.platform.commerceservices.consent.exceptions.CommerceConsentWithdrawnException;
import de.hybris.platform.commerceservices.customer.DuplicateUidException;
import de.hybris.platform.commerceservices.enums.CountryType;
import de.hybris.platform.commerceservices.order.CommerceCartModificationException;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.pagedata.SearchPageData;
import de.hybris.platform.commerceservices.security.BruteForceAttackHandler;
import de.hybris.platform.commerceservices.util.ResponsiveUtils;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.payment.AdapterException;
import de.hybris.platform.servicelayer.exceptions.AmbiguousIdentifierException;
import de.hybris.platform.servicelayer.exceptions.ModelNotFoundException;
import de.hybris.platform.servicelayer.exceptions.UnknownIdentifierException;
import de.hybris.platform.util.Config;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import javax.annotation.Resource;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.http.HttpStatus;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;



/**
 * Controller for home page
 */
@Controller
@RequestMapping("/my-account")
public class AccountPageController extends AbstractSearchPageController
{
	private static final String BREADCRUMBS_ATTR = "breadcrumbs";
	private static final String IS_DEFAULT_ADDRESS_ATTR = "isDefaultAddress";
	private static final String COUNTRY_DATA_ATTR = "countryData";
	private static final String ADDRESS_BOOK_EMPTY_ATTR = "addressBookEmpty";
	private static final String TITLE_DATA_ATTR = "titleData";
	private static final String FORM_GLOBAL_ERROR = "form.global.error";
	private static final String PROFILE_CURRENT_PASSWORD_INVALID = "profile.currentPassword.invalid";
	private static final String TEXT_ACCOUNT_PROFILE = "text.account.profile";
	private static final String ADDRESS_DATA_ATTR = "addressData";
	private static final String ADDRESS_FORM_ATTR = "addressForm";
	private static final String COUNTRY_ATTR = "country";
	private static final String REGIONS_ATTR = "regions";
	private static final String MY_ACCOUNT_ADDRESS_BOOK_URL = "/my-account/address-book";
	private static final String TEXT_ACCOUNT_CONSENT_MANAGEMENT = "text.account.consent.consentManagement";
	private static final String TEXT_ACCOUNT_CONSENT_GIVEN = "text.account.consent.given";
	private static final String TEXT_ACCOUNT_CONSENT_WITHDRAWN = "text.account.consent.withdrawn";
	private static final String TEXT_ACCOUNT_CONSENT_NOT_FOUND = "text.account.consent.notFound";
	private static final String TEXT_ACCOUNT_CONSENT_TEMPLATE_NOT_FOUND = "text.account.consent.template.notFound";
	private static final String TEXT_ACCOUNT_CLOSE = "text.account.close";
	private static final String TEXT_ACCOUNT_CONSENT_ALREADY_GIVEN = "text.account.consent.already.given";
	private static final String TEXT_ACCOUNT_CONSENT_ALREADY_WITHDRAWN = "text.account.consent.already.withdrawn";

	// Internal Redirects
	private static final String REDIRECT_TO_ADDRESS_BOOK_PAGE = REDIRECT_PREFIX + MY_ACCOUNT_ADDRESS_BOOK_URL;
	private static final String REDIRECT_TO_PAYMENT_INFO_PAGE = REDIRECT_PREFIX + "/my-account/payment-details";
	private static final String REDIRECT_TO_UPDATE_EMAIL_PAGE = REDIRECT_PREFIX + "/my-account/update-email";
	private static final String REDIRECT_TO_UPDATE_PROFILE = REDIRECT_PREFIX + "/my-account/update-profile";
	private static final String REDIRECT_TO_PASSWORD_UPDATE_PAGE = REDIRECT_PREFIX + "/my-account/update-password";
	private static final String REDIRECT_TO_ORDER_HISTORY_PAGE = REDIRECT_PREFIX + "/my-account/orders";
	private static final String REDIRECT_TO_CONSENT_MANAGEMENT = REDIRECT_PREFIX + "/my-account/consents";

	/**
	 * We use this suffix pattern because of an issue with Spring 3.1 where a Uri value is incorrectly extracted if it
	 * contains on or more '.' characters. Please see https://jira.springsource.org/browse/SPR-6164 for a discussion on
	 * the issue and future resolution.
	 */
	private static final String ORDER_CODE_PATH_VARIABLE_PATTERN = "{orderCode:.*}";
	private static final String ADDRESS_CODE_PATH_VARIABLE_PATTERN = "{addressCode:.*}";

	// CMS Pages
	private static final String ACCOUNT_CMS_PAGE = "account";
	private static final String PROFILE_CMS_PAGE = "profile";
	private static final String UPDATE_PASSWORD_CMS_PAGE = "updatePassword";
	private static final String UPDATE_PROFILE_CMS_PAGE = "update-profile";
	private static final String UPDATE_EMAIL_CMS_PAGE = "update-email";
	private static final String ADDRESS_BOOK_CMS_PAGE = "address-book";
	private static final String ADD_EDIT_ADDRESS_CMS_PAGE = "add-edit-address";
	private static final String PAYMENT_DETAILS_CMS_PAGE = "payment-details";
	private static final String ORDER_HISTORY_CMS_PAGE = "orders";
	private static final String ORDER_DETAIL_CMS_PAGE = "order";
	private static final String CONSENT_MANAGEMENT_CMS_PAGE = "consents";
	private static final String CLOSE_ACCOUNT_CMS_PAGE = "close-account";
	private static final String BOOKMARKS_CMS_PAGE = "bookmarks";
	private static final String VERIFICATION_IMAGES_CMS_PAGE = "verificationImages";
	private static final String CREDIT_CARTS_CMS_PAGE = "creditCarts";
	private static final String EXTEND_RENTAL_ORDER_DETAILS = "extendRentalOrderDetails";
	private static final String EXTEND_RENTAL_ORDER_CONFIRMATION = "extendRentalOrderConfirmation";
	public static final String ERROR_MSG_TYPE = "errorMsg";


	private static final Logger LOG = Logger.getLogger(AccountPageController.class);

	@Resource(name = "acceleratorCheckoutFacade")
	private CheckoutFacade checkoutFacade;

	@Resource(name = "userFacade")
	private BrainTreeUserFacade userFacade;

	@Resource(name = "customerFacade")
	private CustomerFacade customerFacade;

	@Resource(name = "accountBreadcrumbBuilder")
	private ResourceBreadcrumbBuilder accountBreadcrumbBuilder;

	@Resource(name = "passwordValidator")
	private PasswordValidator passwordValidator;

	@Resource(name = "addressValidator")
	private AddressValidator addressValidator;

	@Resource(name = "profileValidator")
	private ProfileValidator profileValidator;

	@Resource(name = "emailValidator")
	private EmailValidator emailValidator;

	@Resource(name = "i18NFacade")
	private I18NFacade i18NFacade;

	@Resource(name = "addressVerificationFacade")
	private AddressVerificationFacade addressVerificationFacade;

	@Resource(name = "addressVerificationResultHandler")
	private AddressVerificationResultHandler addressVerificationResultHandler;

	@Resource(name = "orderGridFormFacade")
	private OrderGridFormFacade orderGridFormFacade;

	@Resource(name = "customerConsentDataStrategy")
	protected CustomerConsentDataStrategy customerConsentDataStrategy;

	@Resource(name = "blAddressDataUtil")
	private AddressDataUtil addressDataUtil;

	@Resource(name = "blOrderFacade")
	private BlOrderFacade blOrderFacade;

	@Resource(name = "wishlistFacade")
	private BlWishListFacade wishlistFacade;

	@Resource(name = "blDatePickerService")
	private BlDatePickerService blDatePickerService;

	@Resource(name = "bruteForceAttackHandler")
	private BruteForceAttackHandler bruteForceAttackHandler;

	@Resource(name = "voucherFacade")
	private VoucherFacade voucherFacade;

	@Resource(name = "defaultBlCouponFacade")
	private DefaultBlCouponFacade defaultBlCouponFacade;


	@Resource(name = "brainTreeCheckoutFacade")
	private BrainTreeCheckoutFacade brainTreeCheckoutFacade;

	@Resource
	private BrainTreeTransactionService brainTreeTransactionService;

	@Resource(name = "blCommerceStockService")
	private BlCommerceStockService blCommerceStockService;

	@Resource(name = "cartService")
	private BlCartService blCartService;

	@ModelAttribute(name = BlControllerConstants.RENTAL_DATE)
	private RentalDateDto getRentalsDuration() {
		return BlRentalDateUtils.getRentalsDuration();
	}

	protected PasswordValidator getPasswordValidator()
	{
		return passwordValidator;
	}

	protected AddressValidator getAddressValidator()
	{
		return addressValidator;
	}

	protected ProfileValidator getProfileValidator()
	{
		return profileValidator;
	}

	protected EmailValidator getEmailValidator()
	{
		return emailValidator;
	}

	protected I18NFacade getI18NFacade()
	{
		return i18NFacade;
	}

	protected AddressVerificationFacade getAddressVerificationFacade()
	{
		return addressVerificationFacade;
	}

	protected AddressVerificationResultHandler getAddressVerificationResultHandler()
	{
		return addressVerificationResultHandler;
	}

	@ModelAttribute("countries")
	public Collection<CountryData> getCountries()
	{
		return checkoutFacade.getCountries(CountryType.SHIPPING);
	}

	@ModelAttribute("titles")
	public Collection<TitleData> getTitles()
	{
		return userFacade.getTitles();
	}

	@ModelAttribute("countryDataMap")
	public Map<String, CountryData> getCountryDataMap()
	{
		final Map<String, CountryData> countryDataMap = new HashMap<>();
		for (final CountryData countryData : getCountries())
		{
			countryDataMap.put(countryData.getIsocode(), countryData);
		}
		return countryDataMap;
	}


	@RequestMapping(value = "/addressform", method = RequestMethod.GET)
	public String getCountryAddressForm(@RequestParam("addressCode") final String addressCode,
			@RequestParam("countryIsoCode") final String countryIsoCode, final Model model)
	{
		model.addAttribute("supportedCountries", getCountries());
		populateModelRegionAndCountry(model, countryIsoCode);

		final AddressForm addressForm = new AddressForm();
		model.addAttribute(ADDRESS_FORM_ATTR, addressForm);
		for (final AddressData addressData : userFacade.getAddressBook())
		{
			if (addressData.getId() != null && addressData.getId().equals(addressCode)
					&& countryIsoCode.equals(addressData.getCountry().getIsocode()))
			{
				model.addAttribute(ADDRESS_DATA_ATTR, addressData);
				addressDataUtil.convert(addressData, addressForm);
				break;
			}
		}
		return ControllerConstants.Views.Fragments.Account.CountryAddressForm;
	}

	protected void populateModelRegionAndCountry(final Model model, final String countryIsoCode)
	{
		model.addAttribute(REGIONS_ATTR, getI18NFacade().getRegionsForCountryIso(countryIsoCode));
		model.addAttribute(COUNTRY_ATTR, countryIsoCode);
	}

	@RequestMapping(method = RequestMethod.GET)
	@RequireHardLogIn
	public String account(final Model model, final RedirectAttributes redirectModel) throws CMSItemNotFoundException
	{
		if (ResponsiveUtils.isResponsive())
		{
			GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.ERROR_MESSAGES_HOLDER, "system.error.page.not.found", null);
			return REDIRECT_PREFIX + "/";
		}
		final ContentPageModel accountPage = getContentPageForLabelOrId(ACCOUNT_CMS_PAGE);
		storeCmsPageInModel(model, accountPage);
		setUpMetaDataForContentPage(model, accountPage);
		model.addAttribute(BREADCRUMBS_ATTR, accountBreadcrumbBuilder.getBreadcrumbs(null));
		model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
		return getViewForPage(model);
	}

	@GetMapping(value = "/orders")
	@RequireHardLogIn
	public String orders(@RequestParam(value = "page", defaultValue = "0") final int page,
			@RequestParam(value = "show", defaultValue = "Page") final ShowMode showMode,
			@RequestParam(value = "sort", required = false) final String sortCode, final Model model) throws CMSItemNotFoundException
	{
		// Handle paged search results
		final int pageZie = Config.getInt("orderhistory.page.size" , 8);
		final PageableData pageableData = createPageableData(page, pageZie, sortCode, showMode); // NOSONAR
		final SearchPageData<OrderHistoryData> searchPageData = blOrderFacade.getPagedOrderHistoryForStatuses(pageableData);  // NOSONAR
		populateModel(model, searchPageData, showMode);
		final ContentPageModel orderHistoryPage = getContentPageForLabelOrId(ORDER_HISTORY_CMS_PAGE);
		storeCmsPageInModel(model, orderHistoryPage);
			setUpMetaDataForContentPage(model, orderHistoryPage);
		model.addAttribute(BREADCRUMBS_ATTR, accountBreadcrumbBuilder.getBreadcrumbs("text.account.orderHistory"));
		model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
		return getViewForPage(model);
	}

	@GetMapping(value = "/order/" + ORDER_CODE_PATH_VARIABLE_PATTERN)
	@RequireHardLogIn
	public String order(@PathVariable("orderCode") final String orderCode, final Model model,
			final RedirectAttributes redirectModel) throws CMSItemNotFoundException
	{
		try
		{
			final OrderData orderDetails = blOrderFacade.getOrderDetailsForCode(orderCode);
			model.addAttribute(BlControllerConstants.ORDER_DATA, orderDetails);

			final List<Breadcrumb> breadcrumbs = accountBreadcrumbBuilder.getBreadcrumbs(null);
			breadcrumbs.add(new Breadcrumb("/my-account/orders",
					getMessageSource().getMessage("text.account.orderHistory", null, getI18nService().getCurrentLocale()), null));
			breadcrumbs.add(new Breadcrumb("#", getMessageSource().getMessage("text.account.order.orderBreadcrumb", new Object[]
			{ orderDetails.getCode() }, "Order {0}", getI18nService().getCurrentLocale()), null));
			model.addAttribute(BREADCRUMBS_ATTR, breadcrumbs);

		}
		catch (final UnknownIdentifierException e)
		{
			BlLogger.logMessage(LOG, Level.ERROR , "Attempted to load a order that does not exist or is not visible", e);
			GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.ERROR_MESSAGES_HOLDER, "system.error.page.not.found", null);
			return REDIRECT_TO_ORDER_HISTORY_PAGE;
		}
		final ContentPageModel orderDetailPage = getContentPageForLabelOrId(ORDER_DETAIL_CMS_PAGE);
		storeCmsPageInModel(model, orderDetailPage);
		model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
		setUpMetaDataForContentPage(model, orderDetailPage);
		model.addAttribute(BlControllerConstants.PAGE_TYPE, BlControllerConstants.ORDER_DETAILS);
		if(null != blCartService.getSessionCart() && CollectionUtils.isNotEmpty(blCartService.getSessionCart().getEntries())) {
			model.addAttribute(BlControllerConstants.IS_USED_GEAR_CART_ACTIVE,
					BooleanUtils.isFalse(blCartService.getSessionCart().getIsRentalCart()));
		}
		else {
			model.addAttribute(BlControllerConstants.IS_USED_GEAR_CART_ACTIVE, false);
		}
		return getViewForPage(model);
	}

	@RequestMapping(value = "/order/" + ORDER_CODE_PATH_VARIABLE_PATTERN
			+ "/getReadOnlyProductVariantMatrix", method = RequestMethod.GET)
	@RequireHardLogIn
	public String getProductVariantMatrixForResponsive(@PathVariable("orderCode") final String orderCode,
			@RequestParam("productCode") final String productCode, final Model model)
	{
		final OrderData orderData = blOrderFacade.getOrderDetailsForCodeWithoutUser(orderCode);

		final Map<String, ReadOnlyOrderGridData> readOnlyMultiDMap = orderGridFormFacade.getReadOnlyOrderGridForProductInOrder(
				productCode, Arrays.asList(ProductOption.BASIC, ProductOption.CATEGORIES), orderData);
		model.addAttribute("readOnlyMultiDMap", readOnlyMultiDMap);

		return ControllerConstants.Views.Fragments.Checkout.ReadOnlyExpandedOrderForm;
	}

	@RequestMapping(value = "/profile", method = RequestMethod.GET)
	@RequireHardLogIn
	public String profile(final Model model) throws CMSItemNotFoundException
	{
		final List<TitleData> titles = userFacade.getTitles();

		final CustomerData customerData = customerFacade.getCurrentCustomer();
		if (customerData.getTitleCode() != null)
		{
			model.addAttribute("title", findTitleForCode(titles, customerData.getTitleCode()));
		}

		model.addAttribute("customerData", customerData);

		final ContentPageModel profilePage = getContentPageForLabelOrId(PROFILE_CMS_PAGE);
		storeCmsPageInModel(model, profilePage);
		setUpMetaDataForContentPage(model, profilePage);
		model.addAttribute(BREADCRUMBS_ATTR, accountBreadcrumbBuilder.getBreadcrumbs(TEXT_ACCOUNT_PROFILE));
		model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
		return getViewForPage(model);
	}

	protected TitleData findTitleForCode(final List<TitleData> titles, final String code)
	{
		if (code != null && !code.isEmpty() && titles != null && !titles.isEmpty())
		{
			for (final TitleData title : titles)
			{
				if (code.equals(title.getCode()))
				{
					return title;
				}
			}
		}
		return null;
	}

	@RequestMapping(value = "/update-email", method = RequestMethod.GET)
	@RequireHardLogIn
	public String editEmail(final Model model) throws CMSItemNotFoundException
	{
		final CustomerData customerData = customerFacade.getCurrentCustomer();
		final UpdateEmailForm updateEmailForm = new UpdateEmailForm();

		updateEmailForm.setEmail(customerData.getDisplayUid());
		model.addAttribute(BlCoreConstants.BL_PAGE_TYPE,BlControllerConstants.UPDATE_EMAIL_IDENTIFIER);
		model.addAttribute("updateEmailForm", updateEmailForm);
		final ContentPageModel updateEmailPage = getContentPageForLabelOrId(UPDATE_EMAIL_CMS_PAGE);
		storeCmsPageInModel(model, updateEmailPage);
		setUpMetaDataForContentPage(model, updateEmailPage);
		model.addAttribute(BREADCRUMBS_ATTR, accountBreadcrumbBuilder.getBreadcrumbs(TEXT_ACCOUNT_PROFILE));
		model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
		return getViewForPage(model);
	}

	@RequestMapping(value = "/update-email", method = RequestMethod.POST)
	@RequireHardLogIn
	public String updateEmail(final UpdateEmailForm updateEmailForm, final BindingResult bindingResult, final Model model,
			final RedirectAttributes redirectAttributes) throws CMSItemNotFoundException
	{
		getEmailValidator().validate(updateEmailForm, bindingResult);
		String returnAction = REDIRECT_TO_UPDATE_EMAIL_PAGE;

		if (!bindingResult.hasErrors() && !updateEmailForm.getEmail().equals(updateEmailForm.getChkEmail()))
		{
			bindingResult.rejectValue("chkEmail", "validation.checkEmail.equals", new Object[] {}, "validation.checkEmail.equals");
		}

		if (bindingResult.hasErrors())
		{
			returnAction = setErrorMessagesOnAccountCMSPage(model, UPDATE_EMAIL_CMS_PAGE);
		}
		else
		{
			try
			{
				customerFacade.changeUid(updateEmailForm.getEmail(), updateEmailForm.getPassword());
				redirectAttributes.addFlashAttribute("successMsgEmail", getMessageSource().getMessage("text.account.profile.confirmationUpdated", null, getI18nService().getCurrentLocale()));

				// Replace the spring security authentication with the new UID
				final String newUid = customerFacade.getCurrentCustomer().getUid().toLowerCase();  // NOSONAR
				final Authentication oldAuthentication = SecurityContextHolder.getContext().getAuthentication();
				final UsernamePasswordAuthenticationToken newAuthentication = new UsernamePasswordAuthenticationToken(newUid, null,
						oldAuthentication.getAuthorities());
				newAuthentication.setDetails(oldAuthentication.getDetails());
				SecurityContextHolder.getContext().setAuthentication(newAuthentication);
			}
			catch (final DuplicateUidException e)
			{
				bindingResult.rejectValue("email", "profile.email.unique");
				returnAction = setErrorMessagesOnAccountCMSPage(model, UPDATE_EMAIL_CMS_PAGE);
			}
			catch (final PasswordMismatchException passwordMismatchException)
			{
				bindingResult.rejectValue("password", PROFILE_CURRENT_PASSWORD_INVALID);//NOSONAR
				returnAction = setErrorMessagesOnAccountCMSPage(model, UPDATE_EMAIL_CMS_PAGE);
			}
		}

		return returnAction;
	}

	protected String setErrorMessagesAndCMSPage(final Model model, final String cmsPageLabelOrId) throws CMSItemNotFoundException
	{
		GlobalMessages.addErrorMessage(model, FORM_GLOBAL_ERROR);
		final ContentPageModel cmsPage = getContentPageForLabelOrId(cmsPageLabelOrId);
		storeCmsPageInModel(model, cmsPage);
		setUpMetaDataForContentPage(model, cmsPage);
		model.addAttribute(BREADCRUMBS_ATTR, accountBreadcrumbBuilder.getBreadcrumbs(TEXT_ACCOUNT_PROFILE));
		return getViewForPage(model);
	}

	//This method is for displaying the Error message on the Account page rather than using the Global messages.
	protected String setErrorMessagesOnAccountCMSPage(final Model model,
			final String cmsPageLabelOrId) throws CMSItemNotFoundException {
		final ContentPageModel cmsPage = getContentPageForLabelOrId(cmsPageLabelOrId);
		storeCmsPageInModel(model, cmsPage);
		setUpMetaDataForContentPage(model, cmsPage);
		model.addAttribute(BREADCRUMBS_ATTR,
				accountBreadcrumbBuilder.getBreadcrumbs(TEXT_ACCOUNT_PROFILE));
		return getViewForPage(model);
	}

	@RequestMapping(value = "/update-profile", method = RequestMethod.GET)
	@RequireHardLogIn
	public String editProfile(final Model model) throws CMSItemNotFoundException
	{
		model.addAttribute(TITLE_DATA_ATTR, userFacade.getTitles());

		final CustomerData customerData = customerFacade.getCurrentCustomer();
		final UpdateProfileForm updateProfileForm = new UpdateProfileForm();

		updateProfileForm.setTitleCode(customerData.getTitleCode());
		updateProfileForm.setFirstName(customerData.getFirstName());
		updateProfileForm.setLastName(customerData.getLastName());

		model.addAttribute("updateProfileForm", updateProfileForm);

		final ContentPageModel updateProfilePage = getContentPageForLabelOrId(UPDATE_PROFILE_CMS_PAGE);
		storeCmsPageInModel(model, updateProfilePage);
		setUpMetaDataForContentPage(model, updateProfilePage);

		model.addAttribute(BREADCRUMBS_ATTR, accountBreadcrumbBuilder.getBreadcrumbs(TEXT_ACCOUNT_PROFILE));
		model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
		return getViewForPage(model);
	}

	@RequestMapping(value = "/update-profile", method = RequestMethod.POST)
	@RequireHardLogIn
	public String updateProfile(final UpdateProfileForm updateProfileForm, final BindingResult bindingResult, final Model model,
			final RedirectAttributes redirectAttributes) throws CMSItemNotFoundException
	{
		getProfileValidator().validate(updateProfileForm, bindingResult);

		String returnAction = REDIRECT_TO_UPDATE_PROFILE;
		final CustomerData currentCustomerData = customerFacade.getCurrentCustomer();
		final CustomerData customerData = new CustomerData();
		customerData.setTitleCode(updateProfileForm.getTitleCode());
		customerData.setFirstName(updateProfileForm.getFirstName());
		customerData.setLastName(updateProfileForm.getLastName());
		customerData.setUid(currentCustomerData.getUid());
		customerData.setDisplayUid(currentCustomerData.getDisplayUid());

		model.addAttribute(TITLE_DATA_ATTR, userFacade.getTitles());

		final ContentPageModel updateProfilePage = getContentPageForLabelOrId(UPDATE_PROFILE_CMS_PAGE);
		storeCmsPageInModel(model, updateProfilePage);
		setUpMetaDataForContentPage(model, updateProfilePage);

		if (bindingResult.hasErrors())
		{
			returnAction = setErrorMessagesAndCMSPage(model, UPDATE_PROFILE_CMS_PAGE);
		}
		else
		{
			try
			{
				customerFacade.updateProfile(customerData);
				GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.CONF_MESSAGES_HOLDER,
						"text.account.profile.confirmationUpdated", null);

			}
			catch (final DuplicateUidException e)
			{
				bindingResult.rejectValue("email", "registration.error.account.exists.title");
				returnAction = setErrorMessagesAndCMSPage(model, UPDATE_PROFILE_CMS_PAGE);
			}
		}


		model.addAttribute(BREADCRUMBS_ATTR, accountBreadcrumbBuilder.getBreadcrumbs(TEXT_ACCOUNT_PROFILE));
		return returnAction;
	}

	@RequestMapping(value = "/update-password", method = RequestMethod.GET)
	@RequireHardLogIn
	public String updatePassword(final Model model) throws CMSItemNotFoundException
	{
		final UpdatePasswordForm updatePasswordForm = new UpdatePasswordForm();
		model.addAttribute(BlCoreConstants.BL_PAGE_TYPE,BlControllerConstants.UPDATE_PASSWORD_PAGE_IDENTIFIER);
		model.addAttribute("updatePasswordForm", updatePasswordForm);

		final ContentPageModel updatePasswordPage = getContentPageForLabelOrId(UPDATE_PASSWORD_CMS_PAGE);
		storeCmsPageInModel(model, updatePasswordPage);
		setUpMetaDataForContentPage(model, updatePasswordPage);

		model.addAttribute(BREADCRUMBS_ATTR, accountBreadcrumbBuilder.getBreadcrumbs("text.account.profile.updatePasswordForm"));
		model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
		return getViewForPage(model);
	}

	@RequestMapping(value = "/update-password", method = RequestMethod.POST)
	@RequireHardLogIn
	public String updatePassword(final UpdatePasswordForm updatePasswordForm, final BindingResult bindingResult, final Model model,
			final RedirectAttributes redirectAttributes) throws CMSItemNotFoundException
	{
		getPasswordValidator().validate(updatePasswordForm, bindingResult);
		if (!bindingResult.hasErrors())
		{
			if (updatePasswordForm.getNewPassword().equals(updatePasswordForm.getCheckNewPassword()))
			{
				try
				{
					customerFacade.changePassword(updatePasswordForm.getCurrentPassword(), updatePasswordForm.getNewPassword());
				}
				catch (final PasswordMismatchException localException)
				{
					bindingResult.rejectValue("currentPassword", PROFILE_CURRENT_PASSWORD_INVALID, new Object[] {},
							PROFILE_CURRENT_PASSWORD_INVALID);
				}
			}
			else
			{
				bindingResult.rejectValue("checkNewPassword", "validation.checkPwd.equals", new Object[] {},
						"validation.checkPwd.equals");
			}
		}

		if (bindingResult.hasErrors())
		{
			final ContentPageModel updatePasswordPage = getContentPageForLabelOrId(UPDATE_PASSWORD_CMS_PAGE);
			storeCmsPageInModel(model, updatePasswordPage);
			setUpMetaDataForContentPage(model, updatePasswordPage);

			model.addAttribute(BREADCRUMBS_ATTR, accountBreadcrumbBuilder.getBreadcrumbs("text.account.profile.updatePasswordForm"));
			return getViewForPage(model);
		}
		else
		{
			redirectAttributes.addFlashAttribute("successMsg", getMessageSource().getMessage("text.account.confirmation.password.updated", null, getI18nService().getCurrentLocale()));
			return REDIRECT_TO_PASSWORD_UPDATE_PAGE;
		}
	}

	@RequestMapping(value = "/address-book", method = RequestMethod.GET)
	@RequireHardLogIn
	public String getAddressBook(final Model model) throws CMSItemNotFoundException
	{
		model.addAttribute(ADDRESS_DATA_ATTR, userFacade.getAddressBook());
		model.addAttribute(BlCoreConstants.BL_PAGE_TYPE,BlControllerConstants.ADDRESS_PAGE_IDENTIFIER);
		final ContentPageModel addressBookPage = getContentPageForLabelOrId(ADDRESS_BOOK_CMS_PAGE);
		storeCmsPageInModel(model, addressBookPage);
		setUpMetaDataForContentPage(model, addressBookPage);
		model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
		return getViewForPage(model);
	}

	@RequestMapping(value = "/add-address", method = RequestMethod.GET)
	@RequireHardLogIn
	public String addAddress(final Model model) throws CMSItemNotFoundException
	{
		model.addAttribute(BlCoreConstants.BL_PAGE_TYPE,BlControllerConstants.ADDRESS_PAGE_IDENTIFIER);
		model.addAttribute(COUNTRY_DATA_ATTR, checkoutFacade.getCountries(CountryType.SHIPPING));
		populateModelRegionAndCountry(model, Locale.US.getCountry());
		final BlAddressForm addressForm = getPreparedAddressForm();
		model.addAttribute(ADDRESS_FORM_ATTR, addressForm);
		model.addAttribute(ADDRESS_BOOK_EMPTY_ATTR, Boolean.valueOf(CollectionUtils.isEmpty(userFacade.getAddressBook())));
		model.addAttribute(IS_DEFAULT_ADDRESS_ATTR, Boolean.FALSE);
		final ContentPageModel addEditAddressPage = getContentPageForLabelOrId(ADD_EDIT_ADDRESS_CMS_PAGE);
		storeCmsPageInModel(model, addEditAddressPage);
		setUpMetaDataForContentPage(model, addEditAddressPage);
		model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
		return getViewForPage(model);
	}

	protected BlAddressForm getPreparedAddressForm()
	{
		final CustomerData currentCustomerData = customerFacade.getCurrentCustomer();
		final BlAddressForm addressForm = new BlAddressForm();
		addressForm.setFirstName(currentCustomerData.getFirstName());
		addressForm.setLastName(currentCustomerData.getLastName());
		addressForm.setCountryIso(Locale.US.getCountry());
		return addressForm;
	}

	@RequestMapping(value = "/add-address", method = RequestMethod.POST)
	@RequireHardLogIn
	public String addAddress(final BlAddressForm addressForm, final BindingResult bindingResult, final Model model,
			final RedirectAttributes redirectModel) throws CMSItemNotFoundException
	{
		addressForm.setCountryIso(Locale.US.getCountry());
		getAddressValidator().validate(addressForm, bindingResult);
		if (bindingResult.hasErrors())
		{
			GlobalMessages.addErrorMessage(model, FORM_GLOBAL_ERROR);
			final ContentPageModel addEditAddressPage = getContentPageForLabelOrId(ADD_EDIT_ADDRESS_CMS_PAGE);
			storeCmsPageInModel(model, addEditAddressPage);
			setUpMetaDataForContentPage(model, addEditAddressPage);
			setUpAddressFormAfterError(addressForm, model);
			return getViewForPage(model);
		}
		final AddressData newAddress = addressDataUtil.convertToVisibleAddressData(addressForm);
		if (CollectionUtils.isEmpty(userFacade.getAddressBook()))
		{
			newAddress.setDefaultAddress(Boolean.TRUE);
			newAddress.setShippingAddress(Boolean.TRUE);
		}
		final AddressVerificationResult<AddressVerificationDecision> verificationResult = getAddressVerificationFacade()
				.verifyAddressData(newAddress);
		final boolean addressRequiresReview = getAddressVerificationResultHandler().handleResult(verificationResult, newAddress,
				model, redirectModel, bindingResult, getAddressVerificationFacade().isCustomerAllowedToIgnoreAddressSuggestions(),
				"checkout.multi.address.added");

		populateModelRegionAndCountry(model, addressForm.getCountryIso());
		model.addAttribute("edit", Boolean.FALSE);
		model.addAttribute(IS_DEFAULT_ADDRESS_ATTR, Boolean.valueOf(userFacade.isDefaultAddress(addressForm.getAddressId())));

		if (addressRequiresReview)
		{
			storeCmsPageInModel(model, getContentPageForLabelOrId(ADD_EDIT_ADDRESS_CMS_PAGE));
			setUpMetaDataForContentPage(model, getContentPageForLabelOrId(ADD_EDIT_ADDRESS_CMS_PAGE));
			return getViewForPage(model);
		}

		userFacade.addAddress(newAddress);


		GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.CONF_MESSAGES_HOLDER, "account.confirmation.address.added",
				null);

		return REDIRECT_TO_ADDRESS_BOOK_PAGE;
	}

	protected void setUpAddressFormAfterError(final BlAddressForm addressForm, final Model model)
	{
		model.addAttribute(COUNTRY_DATA_ATTR, checkoutFacade.getCountries(CountryType.SHIPPING));
		model.addAttribute(ADDRESS_BOOK_EMPTY_ATTR, Boolean.valueOf(CollectionUtils.isEmpty(userFacade.getAddressBook())));
		model.addAttribute(IS_DEFAULT_ADDRESS_ATTR, Boolean.valueOf(userFacade.isDefaultAddress(addressForm.getAddressId())));
		model.addAttribute(ADDRESS_FORM_ATTR, addressForm);
		if (addressForm.getCountryIso() != null) //NOSONAR
		{
			populateModelRegionAndCountry(model, addressForm.getCountryIso());
		}
	}

	@RequestMapping(value = "/edit-address/" + ADDRESS_CODE_PATH_VARIABLE_PATTERN, method = RequestMethod.GET)
	@RequireHardLogIn
	public String editAddress(@PathVariable("addressCode") final String addressCode, final Model model)
			throws CMSItemNotFoundException
	{
		model.addAttribute(BlCoreConstants.BL_PAGE_TYPE,BlControllerConstants.ADDRESS_PAGE_IDENTIFIER);
		final BlAddressForm addressForm = new BlAddressForm();
		model.addAttribute(COUNTRY_DATA_ATTR, checkoutFacade.getCountries(CountryType.SHIPPING));
		model.addAttribute(ADDRESS_FORM_ATTR, addressForm);
		final List<AddressData> addressBook = userFacade.getAddressBook();
		model.addAttribute(ADDRESS_BOOK_EMPTY_ATTR, Boolean.valueOf(CollectionUtils.isEmpty(addressBook)));


		for (final AddressData addressData : addressBook)
		{
			if (addressData.getId() != null && addressData.getId().equals(addressCode))
			{
				model.addAttribute(REGIONS_ATTR, getI18NFacade().getRegionsForCountryIso(addressData.getCountry().getIsocode()));
				model.addAttribute(COUNTRY_ATTR, addressData.getCountry().getIsocode());
				model.addAttribute(ADDRESS_DATA_ATTR, addressData);
				addressDataUtil.convert(addressData, addressForm);
				break;
			}
		}
		final ContentPageModel addEditAddressPage = getContentPageForLabelOrId(ADD_EDIT_ADDRESS_CMS_PAGE);
		storeCmsPageInModel(model, addEditAddressPage);
		setUpMetaDataForContentPage(model, addEditAddressPage);
		model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
		model.addAttribute("edit", Boolean.TRUE);
		return getViewForPage(model);
	}

	@RequestMapping(value = "/edit-address/" + ADDRESS_CODE_PATH_VARIABLE_PATTERN, method = RequestMethod.POST)
	@RequireHardLogIn
	public String editAddress(final BlAddressForm addressForm, final BindingResult bindingResult, final Model model,
			final RedirectAttributes redirectModel) throws CMSItemNotFoundException
	{
		model.addAttribute(BlCoreConstants.BL_PAGE_TYPE,BlControllerConstants.ADDRESS_PAGE_IDENTIFIER);
		addressForm.setCountryIso(Locale.US.getCountry());
		getAddressValidator().validate(addressForm, bindingResult);
		final ContentPageModel addEditAddressPage = getContentPageForLabelOrId(ADD_EDIT_ADDRESS_CMS_PAGE);
		if (bindingResult.hasErrors())
		{
			GlobalMessages.addErrorMessage(model, FORM_GLOBAL_ERROR);
			storeCmsPageInModel(model, addEditAddressPage);
			setUpMetaDataForContentPage(model, addEditAddressPage);
			setUpAddressFormAfterError(addressForm, model);
			return getViewForPage(model);
		}

		model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
		final AddressData newAddress = addressDataUtil.convertToVisibleAddressData(addressForm);
		if (Boolean.TRUE.equals(addressForm.getDefaultAddress()) || userFacade.getAddressBook().size() <= 1)
		{
			newAddress.setDefaultAddress(Boolean.TRUE);
			newAddress.setShippingAddress(Boolean.TRUE);
		}
		final AddressData defaultBillingAddress = userFacade.getDefaultBillingAddress();
		if(defaultBillingAddress != null && defaultBillingAddress.getId().equals(addressForm.getAddressId())){
			newAddress.setBillingAddress(Boolean.TRUE);
		}
	   final AddressData defaultShippingAddress = userFacade.getDefaultAddress();
		if(defaultShippingAddress != null && defaultShippingAddress.getId().equals(addressForm.getAddressId())){
			newAddress.setShippingAddress(Boolean.TRUE);
		}
		final AddressVerificationResult<AddressVerificationDecision> verificationResult = getAddressVerificationFacade()
				.verifyAddressData(newAddress);
		final boolean addressRequiresReview = getAddressVerificationResultHandler().handleResult(verificationResult, newAddress,
				model, redirectModel, bindingResult, getAddressVerificationFacade().isCustomerAllowedToIgnoreAddressSuggestions(),
				"checkout.multi.address.updated");

		model.addAttribute(REGIONS_ATTR, getI18NFacade().getRegionsForCountryIso(addressForm.getCountryIso()));
		model.addAttribute(COUNTRY_ATTR, addressForm.getCountryIso());
		model.addAttribute("edit", Boolean.TRUE);
		model.addAttribute(IS_DEFAULT_ADDRESS_ATTR, Boolean.valueOf(userFacade.isDefaultAddress(addressForm.getAddressId())));

		if (addressRequiresReview)
		{
			storeCmsPageInModel(model, addEditAddressPage);
			setUpMetaDataForContentPage(model, addEditAddressPage);
			return getViewForPage(model);
		}

		userFacade.editAddress(newAddress);

		GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.CONF_MESSAGES_HOLDER, "account.confirmation.address.updated",
				null);
		return REDIRECT_TO_ADDRESS_BOOK_PAGE;
	}

	@RequestMapping(value = "/select-suggested-address", method = RequestMethod.POST)
	public String doSelectSuggestedAddress(final AddressForm addressForm, final RedirectAttributes redirectModel)
	{
		final Set<String> resolveCountryRegions = org.springframework.util.StringUtils
				.commaDelimitedListToSet(Config.getParameter("resolve.country.regions"));

		final AddressData selectedAddress = addressDataUtil.convertToVisibleAddressData(addressForm);

		final CountryData countryData = selectedAddress.getCountry();

		if (!resolveCountryRegions.contains(countryData.getIsocode()))
		{
			selectedAddress.setRegion(null);
		}

		if (Boolean.TRUE.equals(addressForm.getEditAddress()))
		{
			userFacade.editAddress(selectedAddress);
		}
		else
		{
			userFacade.addAddress(selectedAddress);
		}

		GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.CONF_MESSAGES_HOLDER, "account.confirmation.address.added");

		return REDIRECT_TO_ADDRESS_BOOK_PAGE;
	}

	@RequestMapping(value = "/remove-address/" + ADDRESS_CODE_PATH_VARIABLE_PATTERN, method =
	{ RequestMethod.GET, RequestMethod.POST }) //NOSONAR
	@RequireHardLogIn
	public String removeAddress(@PathVariable("addressCode") final String addressCode, final RedirectAttributes redirectModel)
	{
		final AddressData addressData = new AddressData();
		addressData.setId(addressCode);
		userFacade.removeAddress(addressData);

		GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.CONF_MESSAGES_HOLDER, "account.confirmation.address.removed");
		return REDIRECT_TO_ADDRESS_BOOK_PAGE;
	}

	@RequestMapping(value = "/set-default-address/" + ADDRESS_CODE_PATH_VARIABLE_PATTERN, method = RequestMethod.GET)
	@RequireHardLogIn
	public @ResponseBody
	String setDefaultAddress(@PathVariable("addressCode") final String addressCode, final RedirectAttributes redirectModel)
	{
		final AddressData addressData = new AddressData();
		addressData.setDefaultAddress(true);
		addressData.setVisibleInAddressBook(true);
		addressData.setId(addressCode);
		userFacade.setDefaultAddress(addressData);
		GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.CONF_MESSAGES_HOLDER,
				"account.confirmation.default.address.changed");
		return BlControllerConstants.SUCCESS;
	}

	/**
	 * This method is responsible for setting default billing address.
	 */
	@RequestMapping(value = "/set-default-billing-address/" + ADDRESS_CODE_PATH_VARIABLE_PATTERN, method = RequestMethod.GET)
	@RequireHardLogIn
	public @ResponseBody
	String setDefaultBillingAddress(@PathVariable("addressCode") final String addressCode, final RedirectAttributes redirectModel)
	{
		final AddressData addressData = new AddressData();
		addressData.setDefaultBillingAddress(true);
		addressData.setVisibleInAddressBook(true);
		addressData.setId(addressCode);
		userFacade.setDefaultBillingAddress(addressData);
		GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.CONF_MESSAGES_HOLDER,
				"account.confirmation.default.address.changed");
		return BlControllerConstants.SUCCESS;
	}

	@RequestMapping(value = "/payment-details", method = RequestMethod.GET)
	@RequireHardLogIn
	public String paymentDetails(final Model model) throws CMSItemNotFoundException
	{
		model.addAttribute("customerData", customerFacade.getCurrentCustomer());
		model.addAttribute("paymentInfoData", userFacade.getCCPaymentInfos(true));
		storeCmsPageInModel(model, getContentPageForLabelOrId(PAYMENT_DETAILS_CMS_PAGE));
		setUpMetaDataForContentPage(model, getContentPageForLabelOrId(ADD_EDIT_ADDRESS_CMS_PAGE));
		model.addAttribute(BREADCRUMBS_ATTR, accountBreadcrumbBuilder.getBreadcrumbs("text.account.paymentDetails"));
		model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
		return getViewForPage(model);
	}

	@RequestMapping(value = "/set-default-payment-details", method = RequestMethod.POST)
	@RequireHardLogIn
	public String setDefaultPaymentDetails(@RequestParam final String paymentInfoId)
	{
		CCPaymentInfoData paymentInfoData = null;
		if (StringUtils.isNotBlank(paymentInfoId))
		{
			paymentInfoData = userFacade.getCCPaymentInfoForCode(paymentInfoId);
		}
		userFacade.setDefaultPaymentInfo(paymentInfoData);
		return REDIRECT_TO_PAYMENT_INFO_PAGE;
	}

	@RequestMapping(value = "/remove-payment-method", method = RequestMethod.POST)
	@RequireHardLogIn
	public String removePaymentMethod(@RequestParam(value = "paymentInfoId") final String paymentMethodId,
			final RedirectAttributes redirectAttributes) throws CMSItemNotFoundException  // NOSONAR
	{
		userFacade.unlinkCCPaymentInfo(paymentMethodId);  // NOSONAR
		GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.CONF_MESSAGES_HOLDER,
				"text.account.profile.paymentCart.removed");
		return REDIRECT_TO_PAYMENT_INFO_PAGE;
	}

	@RequestMapping(value = "/consents", method = RequestMethod.GET)
	@RequireHardLogIn
	public String consentManagement(final Model model) throws CMSItemNotFoundException
	{
		model.addAttribute("consentTemplateDataList", getConsentFacade().getConsentTemplatesWithConsents());
		final ContentPageModel consentManagementPage = getContentPageForLabelOrId(CONSENT_MANAGEMENT_CMS_PAGE);
		storeCmsPageInModel(model, consentManagementPage);
		setUpMetaDataForContentPage(model, consentManagementPage);
		model.addAttribute(BREADCRUMBS_ATTR, accountBreadcrumbBuilder.getBreadcrumbs(TEXT_ACCOUNT_CONSENT_MANAGEMENT));
		model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
		return getViewForPage(model);
	}

	@RequestMapping(value = "/consents/give/{consentTemplateId}/{version}", method = RequestMethod.POST)
	@RequireHardLogIn
	public String giveConsent(@PathVariable final String consentTemplateId, @PathVariable final Integer version,
			final RedirectAttributes redirectModel)
	{
		try
		{
			getConsentFacade().giveConsent(consentTemplateId, version);
			GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.CONF_MESSAGES_HOLDER, TEXT_ACCOUNT_CONSENT_GIVEN);
		}
		catch (final ModelNotFoundException | AmbiguousIdentifierException e)
		{
			LOG.warn(String.format("ConsentTemplate with code [%s] and version [%s] was not found", consentTemplateId, version), e);
			GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.ERROR_MESSAGES_HOLDER,
					TEXT_ACCOUNT_CONSENT_TEMPLATE_NOT_FOUND, null);
		}
		catch (final CommerceConsentGivenException e)
		{
			LOG.warn(String.format("ConsentTemplate with code [%s] and version [%s] already has a given consent", consentTemplateId,
					version), e);
			GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.ERROR_MESSAGES_HOLDER, TEXT_ACCOUNT_CONSENT_ALREADY_GIVEN,
					null);
		}
		customerConsentDataStrategy.populateCustomerConsentDataInSession();
		return REDIRECT_TO_CONSENT_MANAGEMENT;
	}

	@RequestMapping(value = "/consents/withdraw/{consentCode}", method = RequestMethod.POST)
	@RequireHardLogIn
	public String withdrawConsent(@PathVariable final String consentCode, final RedirectAttributes redirectModel)
			throws CMSItemNotFoundException  // NOSONAR
	{
		try
		{
			getConsentFacade().withdrawConsent(consentCode);
			GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.CONF_MESSAGES_HOLDER, TEXT_ACCOUNT_CONSENT_WITHDRAWN);
		}
		catch (final ModelNotFoundException e)
		{
			LOG.warn(String.format("Consent with code [%s] was not found", consentCode), e);
			GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.ERROR_MESSAGES_HOLDER, TEXT_ACCOUNT_CONSENT_NOT_FOUND,
					null);
		}
		catch (final CommerceConsentWithdrawnException e)
		{
			LOG.error(String.format("Consent with code [%s] is already withdrawn", consentCode), e);
			GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.ERROR_MESSAGES_HOLDER,
					TEXT_ACCOUNT_CONSENT_ALREADY_WITHDRAWN, null);
		}
		customerConsentDataStrategy.populateCustomerConsentDataInSession();
		return REDIRECT_TO_CONSENT_MANAGEMENT;
	}

	@RequestMapping(value = "/close-account", method = RequestMethod.GET)
	@RequireHardLogIn
	public String showCloseAccountPage(final Model model) throws CMSItemNotFoundException
	{
		final ContentPageModel closeAccountPage = getContentPageForLabelOrId(CLOSE_ACCOUNT_CMS_PAGE);
		storeCmsPageInModel(model, closeAccountPage);
		setUpMetaDataForContentPage(model, closeAccountPage);
		model.addAttribute(BREADCRUMBS_ATTR, accountBreadcrumbBuilder.getBreadcrumbs(TEXT_ACCOUNT_CLOSE));
		model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
		return getViewForPage(model);
	}

	@RequestMapping(value = "/close-account", method = RequestMethod.POST)
	@ResponseStatus(value = HttpStatus.OK)
	@RequireHardLogIn
	public void closeAccount(final HttpServletRequest request) throws CMSItemNotFoundException, ServletException  // NOSONAR
	{
		customerFacade.closeAccount();
		request.logout();
	}

	@GetMapping(value = "/bookmarks")
	@RequireHardLogIn
	public String bookmarksPage(@RequestParam(value = "page", defaultValue = "0") final int page,
			@RequestParam(value = "show", defaultValue = "Page") final ShowMode showMode,
			@RequestParam(value = "sort", required = false) final String sortCode, final Model model)
			throws CMSItemNotFoundException {
		model.addAttribute(BlCoreConstants.BL_PAGE_TYPE,
				BlControllerConstants.BOOKMARKS_PAGE_IDENTIFIER);
		final PageableData pageableData = createPageableData(page, 5, sortCode, showMode);
		final SearchPageData<Wishlist2EntryData> searchPageData = wishlistFacade
				.getWishlistEntries(pageableData);
		removeDiscontinuedEntries(searchPageData);
		populateModel(model, searchPageData, showMode);
		final ContentPageModel bookmarksPage = getContentPageForLabelOrId(BOOKMARKS_CMS_PAGE);
		storeCmsPageInModel(model, bookmarksPage);
		setUpMetaDataForContentPage(model, bookmarksPage);
		model.addAttribute("searchPageData", searchPageData);
		model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS,
				ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
		return getViewForPage(model);

	}

	/**
	 *  This is to remove the discontinued products from the pageable data of WishlistEntries
	 * @param searchPageData
	 */
	private void removeDiscontinuedEntries(SearchPageData<Wishlist2EntryData> searchPageData) {
		final RentalDateDto rentalDateDto = blDatePickerService.getRentalDatesFromSession();
		List<Wishlist2EntryData> wishlistEntries = searchPageData.getResults();

		if (Objects.nonNull(rentalDateDto) || CollectionUtils.isNotEmpty(wishlistEntries)) {
			for (Wishlist2EntryData entry : wishlistEntries) {
				if (BooleanUtils.isTrue(entry.getProduct().getIsDiscontinued())) {
					wishlistFacade.removeWishlist(entry.getProduct().getCode());
				}
			}
		}
	}

	@GetMapping(value = "/verificationImages")
	@RequireHardLogIn
	public String getVarificationImagesDetails(final Model model) throws CMSItemNotFoundException{
		final ContentPageModel varificationImagesPage = getContentPageForLabelOrId(VERIFICATION_IMAGES_CMS_PAGE);
		storeCmsPageInModel(model, varificationImagesPage);
		setUpMetaDataForContentPage(model, varificationImagesPage);
		model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
		return getViewForPage(model);
	}

	@GetMapping(value = "/creditCarts")
	@RequireHardLogIn
	public String getCreditCartsDetails(final Model model) throws CMSItemNotFoundException{
		final ContentPageModel savedCartPage = getContentPageForLabelOrId(CREDIT_CARTS_CMS_PAGE);
		storeCmsPageInModel(model, savedCartPage);
		setUpMetaDataForContentPage(model, savedCartPage);
		model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
		return getViewForPage(model);
	}

	/**
	 * This method used for renting the order again from order details page
	 */
	@RequestMapping(value = "/rentAgain/" +  ORDER_CODE_PATH_VARIABLE_PATTERN)
	@RequireHardLogIn
	public String rentAgain(@PathVariable(value = "orderCode", required = false) final String orderCode, final Model pModel ,
			final HttpServletRequest request) throws CommerceCartModificationException {

		 boolean isAddProductToCartAllowed;

		  if(StringUtils.isNotEmpty(orderCode)) {
			isAddProductToCartAllowed = blOrderFacade.addToCartAllOrderEnrties(orderCode, pModel);
			if(BooleanUtils.isTrue(isAddProductToCartAllowed)) {
				return BlControllerConstants.REDIRECT_CART_URL;
			}
			else {
				return REDIRECT_PREFIX + BlControllerConstants.MY_ACCOUNT_ORDER + orderCode;
			}
		}
		 return REDIRECT_PREFIX + BlControllerConstants.MY_ACCOUNT_ORDER + orderCode;
	}

	/**
	 * This method created for extend only rental orders from MyAccount Page
	 */
	@GetMapping(value = "/extendRent/" +  ORDER_CODE_PATH_VARIABLE_PATTERN)
	@RequireHardLogIn
	public String extendRent(@PathVariable(value = "orderCode" ,required = false) final String orderCode, final Model model , final HttpServletRequest request)
			throws CMSItemNotFoundException {

		// To remove seesion if current order and session order when mismatch
		if(null != BlExtendOrderUtils.getCurrentExtendOrderToSession() &&
				! StringUtils.containsIgnoreCase(orderCode , BlExtendOrderUtils.getCurrentExtendOrderToSession().getCode())){
			BlExtendOrderUtils.removeCurrentExtendOrderToSession();
		}

		final OrderData orderDetails = blOrderFacade.getOrderDetailsForCode(orderCode);
		orderDetails.setIsExtendOrderPage(true);
		model.addAttribute(BlControllerConstants.ORDER_DATA, orderDetails);
		model.addAttribute(BlControllerConstants.VOUCHER_FORM, new VoucherForm());
		setupAdditionalFields(model); //Add braintree detils
		final ContentPageModel extendOrderDetailPage = getContentPageForLabelOrId(EXTEND_RENTAL_ORDER_DETAILS);
		storeCmsPageInModel(model, extendOrderDetailPage);
		model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
		setUpMetaDataForContentPage(model, extendOrderDetailPage);
		return getViewForPage(model);
	}


	/**
	 * This method created for seleting the new return for extend order
	 */
	@GetMapping(value = "/extendDate")
	public String setExtendRenatalEndDate(@RequestParam(value = "extendEndDate", defaultValue = "") final String selectedEndDate,
			@RequestParam(value = "orderEndDate", defaultValue = "") final String orderEndDate,
			@RequestParam(value = "orderCode", defaultValue = "") final String orderCode ,final HttpServletRequest request,
			final HttpServletResponse response, final Model model, final RedirectAttributes redirectModel) throws CommerceCartModificationException {

		final OrderData orderData = blOrderFacade.setRentalExtendOrderDetails(orderCode , orderEndDate, selectedEndDate);

		model.addAttribute(BlControllerConstants.ORDER_DATA , orderData);

		if (!model.containsAttribute(BlControllerConstants.VOUCHER_FORM))
		{
			model.addAttribute(BlControllerConstants.VOUCHER_FORM, new VoucherForm());
		}
		return Account.AccountOrderExtendSummaryPage;
	}


	@PostMapping(value = "/voucher/apply")
	public String applyVoucherAction(@Valid final VoucherForm form, final BindingResult bindingResult,
			final HttpServletRequest request, final RedirectAttributes redirectAttributes , final Model model)
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
							getMessageSource().getMessage("text.voucher.apply.bruteforce.error", null, getI18nService().getCurrentLocale()));
				}
				else
				{
					defaultBlCouponFacade.applyVoucherForExtendOrder(form.getVoucherCode());
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

		return "";
	}


	/**
	 * To set the payment details for extend rental page
	 */
	private void setupAdditionalFields(final Model model)
	{
		String clientToken = StringUtils.EMPTY;

		try
		{
			clientToken = brainTreeCheckoutFacade.generateClientToken();
		}
		catch (final AdapterException exception)
		{
			BlLogger.logMessage(LOG , Level.ERROR , "[ExtendOrderController] Error during token generation!" , exception);
		}

		model.addAttribute(BlControllerConstants.CLIENT_TOKEN, clientToken);
	}

	/**
	 * To extend the order once payment is captured
	 */

	@PostMapping(value = "/extendOrder/"+  ORDER_CODE_PATH_VARIABLE_PATTERN)
	public String placeExtendOrder(@PathVariable(value = "orderCode" ,required = false) final String orderCode ,
			final HttpServletRequest request, final HttpServletResponse response, final Model model)
			throws CMSItemNotFoundException {

		String paymentInfoId = request.getParameter(BlControllerConstants.PAYMENT_ID);
		String paymentMethodNonce = request.getParameter(BlControllerConstants.PAYMENT_NONCE);

		boolean isSuccess = false;
		if(StringUtils.isNotBlank(orderCode) && StringUtils.isNotBlank(paymentInfoId) &&
				StringUtils.isNotBlank(paymentMethodNonce)) {

			final OrderModel orderModel = blOrderFacade.getExtendedOrderModelFromCode(orderCode);

			if(null != orderModel) {
				// Needs to uncomment below code once Payment related PR merged
				/*final BrainTreePaymentInfoModel paymentInfo = brainTreeCheckoutFacade
						.getBrainTreePaymentInfoForCode(
								(CustomerModel) orderModel.getUser(), paymentInfoId, paymentMethodNonce);
				if(null != paymentInfo) {

					isSuccess = brainTreeTransactionService
							.createAuthorizationTransactionOfOrder(orderModel,
									BigDecimal.valueOf(orderModel.getTotalPrice()), true, paymentInfo);
				}*/
			}

			if(isSuccess) {
				blOrderFacade.updateOrderExtendDetails(orderModel); //to update extend order details to DB
				final OrderData extendOrderData = blOrderFacade.getExtendedOrderDetailsFromOrderCode(orderCode);
				model.addAttribute(BlControllerConstants.EXTEND_ORDER_DATA, extendOrderData);
				final ContentPageModel extendOrderConfirmation = getContentPageForLabelOrId(EXTEND_RENTAL_ORDER_CONFIRMATION);
				storeCmsPageInModel(model, extendOrderConfirmation);
				model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
				setUpMetaDataForContentPage(model, extendOrderConfirmation);
				return getViewForPage(model);
			}

		}
			return REDIRECT_PREFIX + BlControllerConstants.MY_ACCOUNT_EXTEND_RENTAL + orderCode;
	}

	}
