package com.braintree.controllers.pages;

import static com.braintree.controllers.BraintreeaddonControllerConstants.CLIENT_TOKEN;
import static de.hybris.platform.util.localization.Localization.getLocalizedString;

import com.bl.facades.customer.BlCustomerFacade;
import com.bl.facades.giftcard.BlGiftCardFacade;
import com.bl.facades.order.BlOrderFacade;
import com.bl.storefront.controllers.pages.BlControllerConstants;
import com.bl.storefront.forms.GiftCardForm;
import com.braintree.controllers.BraintreeaddonControllerConstants;
import com.braintree.exceptions.ResourceErrorMessage;
import com.braintree.facade.BrainTreeUserFacade;
import com.braintree.facade.impl.BrainTreeCheckoutFacade;
import com.braintree.hybris.data.BrainTreeSubscriptionInfoData;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.payment.validators.PaymentMethodValidator;
import com.braintree.transaction.service.BrainTreeTransactionService;
import com.fasterxml.jackson.databind.ObjectMapper;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.RequireHardLogIn;
import de.hybris.platform.acceleratorstorefrontcommons.breadcrumb.Breadcrumb;
import de.hybris.platform.acceleratorstorefrontcommons.breadcrumb.ResourceBreadcrumbBuilder;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.ThirdPartyConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.pages.AbstractPageController;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.util.GlobalMessages;
import de.hybris.platform.acceleratorstorefrontcommons.forms.AddressForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.SopPaymentDetailsForm;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.cms2.model.pages.ContentPageModel;
import de.hybris.platform.commercefacades.order.OrderFacade;
import de.hybris.platform.commercefacades.order.data.CCPaymentInfoData;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.commercefacades.user.data.CountryData;
import de.hybris.platform.commercefacades.user.data.RegionData;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.payment.AdapterException;
import de.hybris.platform.servicelayer.session.SessionService;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.List;
import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Logger;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.PriceDataFactory;
import java.math.RoundingMode;
import java.util.Locale;
import java.util.Objects;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.GiftCardModel;
import com.bl.facades.giftcard.data.BLGiftCardData;
import java.util.ArrayList;
import org.apache.commons.collections.CollectionUtils;
import com.bl.logging.BlLogger;
import org.apache.log4j.Level;
import java.util.Date;
import com.bl.core.model.GiftCardMovementModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.core.enums.OrderStatus;



@Controller
@RequestMapping("/my-account")
public class BrainTreeAccountPageController extends AbstractPageController
{
	private static final String ORDER_CODE = "orderCode";

	private static final String ORDER_DATA = "orderData";

	private static final Logger LOG = Logger.getLogger(BrainTreeAccountPageController.class);
	
	private static final String MY_ACCOUNT_MODIFY_PAYMENT = "/my-account/modifyPayment/";
	private static final String PAY_BILL = "/payBill";
	private static final String MY_ACCOUNT = "/my-account/";
	private static final String MY_ACCOUNT_PAYMENT_DETAILS = "/my-account/payment-details";
	private static final Logger LOGGER = Logger.getLogger(BrainTreeAccountPageController.class);
	private static final String REDIRECT_TO_PAYMENT_INFO_PAGE = REDIRECT_PREFIX + MY_ACCOUNT_PAYMENT_DETAILS;
	private static final String REDIRECT_TO_ADD_PAYMENT_INFO_PAGE = REDIRECT_PREFIX + "/my-account/add-payment-method";
	private static final String REDIRECT_TO_EDIT_PAYMENT_INFO_PAGE = REDIRECT_PREFIX + "/my-account/edit-payment-method";
	private static final String REDIRECT_TO_ORDER_DETAILS_PAGE = REDIRECT_PREFIX + "/my-account/order/";

	// CMS Page
	private static final String ADD_EDIT_PAYMENT_METHOD_CMS_PAGE = "add-edit-payment-method";
	private static final String EDIT_PAYMENT_METHOD_CMS_PAGE = "edit-payment-method";
	
	private static final String PAY_BILL_CMS_PAGE = "pay-bill";
	private static final int DECIMAL_PRECISION = 2;
	private static final String MODIFY_PAYMENT_CMS_PAGE = "modify-payment";
	
	private static final String DEPOSIT_PAYMENT_CMS_PAGE = "deposit-payment";
	private static final String MY_ACCOUNT_DEPOSIT_PAYMENT = "/my-account/depositPayment/";
	
	@Resource(name = "userFacade")
	protected BrainTreeUserFacade userFacade;

	@Resource(name = "brainTreeCheckoutFacade")
	private BrainTreeCheckoutFacade brainTreeCheckoutFacade;

	@Resource(name = "accountBreadcrumbBuilder")
	private ResourceBreadcrumbBuilder accountBreadcrumbBuilder;

	@Resource(name = "paymentMethodValidator")
	private PaymentMethodValidator paymentMethodValidator;

	@Resource(name = "customerFacade")
	private BlCustomerFacade blCustomerFacade;
	
	@Resource()
	private BrainTreeTransactionService brainTreeTransactionService;
	
	@Resource(name = "orderFacade")
	private OrderFacade orderFacade;
	
	@Resource(name = "blOrderFacade")
	private BlOrderFacade blOrderFacade;

	@Resource(name = "priceDataFactory")
	private PriceDataFactory priceDataFactory;
	
	@Resource(name = "sessionService")
	private SessionService sessionService;
	
	@Resource(name = "blGiftCardFacade")
	private BlGiftCardFacade blGiftCardFacade;

	@Resource(name = "modelService")
	private ModelService modelService;
	
	@RequestMapping(value = "/remove-payment-method-bt", method = RequestMethod.POST)
	@RequireHardLogIn
	public String removePaymentMethod(@RequestParam(value = "paymentInfoIdRemove") final String paymentInfoId,
                                      @RequestParam(value = "paymentMethodTokenRomove") final String paymentMethodNonce, final RedirectAttributes redirectAttributes)
			throws CMSItemNotFoundException
	{
		userFacade.unlinkCCPaymentInfo(paymentInfoId);
		userFacade.removeBTCCPaymentInfo(paymentMethodNonce);
		GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.CONF_MESSAGES_HOLDER,
				getLocalizedString("text.account.profile.paymentCart.remove"));

		return REDIRECT_TO_PAYMENT_INFO_PAGE;
	}


	@RequestMapping(value = "/edit-payment-method", method = RequestMethod.GET)
	@RequireHardLogIn
	public String editPaymentMethod(final Model model, @RequestParam(value = "paymentInfoId") final String paymentMethodId,
                                    @RequestParam(value = "cardholder", required = false) final String cardholder,
                                    @RequestParam(value = "errorMessage", required = false) final String errorMessage,
                                    @RequestParam(value = "expirationDate", required = false) final String expirationDate) throws CMSItemNotFoundException
	{
		final List<Breadcrumb> breadcrumbs = accountBreadcrumbBuilder.getBreadcrumbs(null);
		breadcrumbs.add(new Breadcrumb(MY_ACCOUNT_PAYMENT_DETAILS,
				getMessageSource().getMessage("text.account.paymentDetails", null, getI18nService().getCurrentLocale()), null));
		breadcrumbs.add(new Breadcrumb("#",getLocalizedString("text.account.paymentMethod.editPaymentMethod"), null));

		storeCmsPageInModel(model, getContentPageForLabelOrId(EDIT_PAYMENT_METHOD_CMS_PAGE));
		setUpMetaDataForContentPage(model, getContentPageForLabelOrId(EDIT_PAYMENT_METHOD_CMS_PAGE));
		List<AddressData> addressBook = userFacade.getAddressBook();
		CCPaymentInfoData ccPaymentInfo = userFacade.getCCPaymentInfoForCode(paymentMethodId);

		if (errorMessage != null && !model.containsAttribute("accErrorMsgs"))
		{
			GlobalMessages.addErrorMessage(model, errorMessage);
		}

		model.addAttribute("errorMessage", cardholder);
		model.addAttribute("cardholder", cardholder);
		model.addAttribute("expirationDate", expirationDate);
		model.addAttribute("ccPaymentInfo", ccPaymentInfo);
		model.addAttribute("deliveryAddresses", addressBook);
		model.addAttribute("breadcrumbs", breadcrumbs);
		model.addAttribute("selectedPaymentMethodId", paymentMethodId);
		model.addAttribute("metaRobots", "noindex,nofollow");
		return getViewForPage(model);
	}

	@RequestMapping(value = "/edit-payment-method", method = RequestMethod.POST)
	@RequireHardLogIn
	public String editPaymentMethod(final Model model, @RequestParam(value = "paymentInfoId") final String paymentMethodId,
                                    @RequestParam(value = "billingAddressId") final String billingAddressId,
                                    @RequestParam(value = "expirationDate") final String expirationDate, @RequestParam(value = "cvv") final String cvv,
                                    @RequestParam(value = "default_Card") final String defaultCard,
									final RedirectAttributes redirectAttributes) throws CMSItemNotFoundException
	{
		try
		{
			String updatedexpirationDate = expirationDate.replace(",", "");
			ResourceErrorMessage validationMessage = paymentMethodValidator.validate(updatedexpirationDate, cvv);
			if (validationMessage != null && StringUtils.isNotBlank(validationMessage.getMessageKey()))
			{
				String localizedErrorMessage = getLocalizedString(validationMessage.getMessageKey());
				LOGGER.error("Failed to edit payment method. Error occurred while contacting external payment services.");
				GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER, localizedErrorMessage);
				return redirectToEditPage(paymentMethodId, updatedexpirationDate, redirectAttributes, localizedErrorMessage);
			}
			else
			{
				CCPaymentInfoData ccPaymentInfo = userFacade.getCCPaymentInfoForCode(paymentMethodId);

				List<AddressData> addressBook = userFacade.getAddressBook();
				AddressData addressData = getAddressForPaymentInfo(ccPaymentInfo, billingAddressId, addressBook);

				userFacade.editPaymentMethod(ccPaymentInfo, updatedexpirationDate, cvv, addressData, defaultCard);
			}
		}
		catch (AdapterException e)
		{
			String localizedErrorMessage = getLocalizedString("text.account.profile.paymentCart.addPaymentMethod.general")
					+ e.getMessage();
			LOGGER.error("Failed to edit payment method. Error occurred while contacting external payment services.", e);
			GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER, localizedErrorMessage);

			return redirectToEditPage(paymentMethodId, expirationDate, redirectAttributes, localizedErrorMessage);
		}
		GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.CONF_MESSAGES_HOLDER,
				getLocalizedString("text.account.profile.paymentCart.editPaymentMethod.success"));
		return REDIRECT_TO_PAYMENT_INFO_PAGE;
	}

	private String redirectToEditPage(final String paymentMethodId,  final String expirationDate,
                                      RedirectAttributes redirectAttributes, final String localizedErrorMessage)
	{
        redirectAttributes.addAttribute("paymentInfoId", paymentMethodId);

//        redirectAttributes.addAttribute("cardholder", cardholder); // NOSONAR
        redirectAttributes.addAttribute("expirationDate", expirationDate);
        redirectAttributes.addAttribute("errorMessage", localizedErrorMessage);
		return REDIRECT_TO_EDIT_PAYMENT_INFO_PAGE;
	}

	@RequestMapping(value = "/receive-address", method = RequestMethod.GET)
	@RequireHardLogIn
	@ResponseBody
	public String receiveAddress(@RequestParam(value = "selectedAddressCode", required = false) String selectedAddressCode)
			throws CMSItemNotFoundException, IOException
	{
		if (StringUtils.isNotBlank(selectedAddressCode))
		{
			final ObjectMapper mapper = new ObjectMapper();
			List<AddressData> addressBook = userFacade.getAddressBook();
			AddressData addressForPaymentInfo = getAddressForPaymentInfo(selectedAddressCode, addressBook);
			return mapper.writeValueAsString(addressForPaymentInfo);
		}
		return StringUtils.EMPTY;
	}

	@RequestMapping(value = "/add-payment-method", method = RequestMethod.GET)
	@RequireHardLogIn
	public String addPaymentMethod(@RequestParam(value = "orderId", required = false) final String orderCode,
			final Model model, final String selectedAddressCode) throws CMSItemNotFoundException {

		final List<Breadcrumb> breadcrumbs = accountBreadcrumbBuilder.getBreadcrumbs(null);
		breadcrumbs.add(new Breadcrumb(MY_ACCOUNT_PAYMENT_DETAILS,
				getMessageSource().getMessage("text.account.paymentDetails", null, getI18nService().getCurrentLocale()), null));
		breadcrumbs.add(new Breadcrumb("#",
				getMessageSource().getMessage("text.account.profile.paymentMethod.add", null, getI18nService().getCurrentLocale()), null));

		storeCmsPageInModel(model, getContentPageForLabelOrId(ADD_EDIT_PAYMENT_METHOD_CMS_PAGE));
		setUpMetaDataForContentPage(model, getContentPageForLabelOrId(ADD_EDIT_PAYMENT_METHOD_CMS_PAGE));
		setupAdditionalFields(model);
		final List<AddressData> addressBook = userFacade.getAddressBook();
		model.addAttribute("deliveryAddresses", addressBook);

	//	model.addAttribute("selectedAddressCode", selectedAddressCode); // NOSONAR
		model.addAttribute("billingAddresses", blCustomerFacade.getAllVisibleBillingAddressesOnUser());
		model.addAttribute("defaultBillingAddress", blCustomerFacade.getDefaultBillingAddress());
		model.addAttribute("addressForm", new AddressForm());
		model.addAttribute("breadcrumbs", breadcrumbs);
		model.addAttribute("metaRobots", "noindex,nofollow");
		model.addAttribute(ORDER_CODE, orderCode);
		return getViewForPage(model);
	}

	private void setupAdditionalFields(final Model model)
	{
		String clientToken = StringUtils.EMPTY;

		try
		{
			clientToken = brainTreeCheckoutFacade.generateClientToken();
		}
		catch (final AdapterException exception)
		{
			LOGGER.error("[Brain Tree Controller] Error during token generation!");
		}

		model.addAttribute(CLIENT_TOKEN, clientToken);
	}

	@RequestMapping(value = "/add-payment-method", method = RequestMethod.POST)
	@RequireHardLogIn

	public String addPaymentMethod(@RequestParam(value = "bt_payment_method_nonce") final String nonce,   // NOSONAR
			@RequestParam(value = "payment_type") final String paymentProvider,
			@RequestParam(value = "paypal_email") final String payPalEmail,
			@RequestParam(value = "card_type") final String cardType,
			@RequestParam(value = "card_details") final String cardDetails,
			@RequestParam(value = "device_data") final String deviceData,
			@RequestParam(value = "liability_shifted") final String liabilityShifted,
			@RequestParam(value = "save_billing_address") final String saveBillingAddress,
		    @RequestParam(value = "company_name") final String companyName,
			@RequestParam(value = "selected_Billing_Address_Id") final String selectedAddressCode,
			@RequestParam(value = "cardholder", required = false) final String cardholder,
			@RequestParam(value = "default_Card") final String defaultCard,@RequestParam(value = ORDER_CODE, required = false) final String orderCode,
			final RedirectAttributes redirectAttributes, @Valid final SopPaymentDetailsForm sopPaymentDetailsForm) throws CMSItemNotFoundException
	{
		if (StringUtils.isEmpty(nonce))
		{
			LOGGER.error("Failed to create payment method. Error occurred while contacting external payment services.");
			GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER,
					getLocalizedString("text.account.profile.paymentCart.addPaymentMethod.noNonce"));
			return REDIRECT_TO_ADD_PAYMENT_INFO_PAGE;
		}

		final String storeInVault = getSiteConfigService().getProperty("braintree.store.in.vault");
		if (storeInVault != null && !Boolean.parseBoolean(storeInVault))
		{
			LOGGER.error(
					"Failed to create payment method. Store in vault is forbidden. For adding new payment methods enable store in vault.");
			GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER,
					getLocalizedString("text.account.profile.paymentCart.addPaymentMethod.forbidden"));
			return REDIRECT_TO_PAYMENT_INFO_PAGE;
		}
		
		if (StringUtils.isBlank(selectedAddressCode))
	    {
	      try
	      {
	        final AddressData newAddress = interpretResponseAddressData(StringUtils.EMPTY, sopPaymentDetailsForm, companyName);
	        newAddress.setVisibleInAddressBook(StringUtils.isNotBlank(saveBillingAddress) && Boolean.TRUE.toString().equals(saveBillingAddress));
	        getUserFacade().addAddress(newAddress);
	      }
	      catch (final Exception exception)
	      {
	    	  LOGGER.error("Error occurred while adding new billing address", exception);
	      }
	    }
		
		final AddressData addressData = interpretResponseAddressData(selectedAddressCode, sopPaymentDetailsForm, companyName);
	
		if (addressData == null)
		{
			LOGGER.error("Failed to create payment method. Error occurred while address selection");
			GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER,
					getLocalizedString("text.account.profile.paymentCart.addPaymentMethod.noAddress"));
			return REDIRECT_TO_ADD_PAYMENT_INFO_PAGE;
		}

		final BrainTreeSubscriptionInfoData subscriptionInfo = buildSubscriptionInfo(nonce, paymentProvider, cardDetails, cardType,
				payPalEmail, deviceData, liabilityShifted, addressData, cardholder, defaultCard);

		try
		{
			final BrainTreePaymentInfoModel paymentMethod = userFacade.addPaymentMethod(subscriptionInfo);
			if (paymentMethod == null)
			{
				LOGGER.error("Failed to create payment method. Error occurred while payment method creation");
				GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER,
						getLocalizedString("text.account.profile.paymentCart.addPaymentMethod.error"));
				return REDIRECT_TO_ADD_PAYMENT_INFO_PAGE;
			}
		}
		catch (final AdapterException e)
		{
			LOGGER.error("Failed to create payment method. Error occurred while payment method creation: " + e.getMessage());
			GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER,
					getLocalizedString("text.account.profile.paymentCart.addPaymentMethod.general.error"));
			return REDIRECT_TO_PAYMENT_INFO_PAGE;
		}

		GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.CONF_MESSAGES_HOLDER,
				getLocalizedString("text.account.profile.paymentCart.addPaymentMethod.success"));

		if(StringUtils.isNotBlank(orderCode)) {
			String originalOrderCode = orderCode.replace(BlControllerConstants.RATIO + getRedirectionUrl(orderCode), BlControllerConstants.EMPTY);
			if(getRedirectionUrl(orderCode).equalsIgnoreCase(BlControllerConstants.EXTEND)) {
				return REDIRECT_PREFIX + BlControllerConstants.MY_ACCOUNT_EXTEND_RENTAL + originalOrderCode;
			}
			else if(getRedirectionUrl(orderCode).equalsIgnoreCase(BlControllerConstants.MODIFYPAYMENT)){
				return REDIRECT_PREFIX + MY_ACCOUNT_MODIFY_PAYMENT + originalOrderCode;
			}
			else {
				return REDIRECT_PREFIX + MY_ACCOUNT + originalOrderCode + PAY_BILL;
			}
		}
		return REDIRECT_TO_PAYMENT_INFO_PAGE;
	}

	/**
	 * This method created for the PayBill page. 
	 */
	@GetMapping(value = "/{orderCode}/payBill")
	@RequireHardLogIn
	public String getPayBillDetailsForOrder(@PathVariable(value = ORDER_CODE ,required = false) final String orderCode, final Model model) throws CMSItemNotFoundException{
		final ContentPageModel payBillPage = getContentPageForLabelOrId(PAY_BILL_CMS_PAGE);
		storeCmsPageInModel(model, payBillPage);
		setUpMetaDataForContentPage(model, payBillPage);
		final OrderData orderDetails = blOrderFacade.getOrderDetailsForCode(orderCode);
		blOrderFacade.setPayBillAttributes(orderDetails);
		model.addAttribute(ORDER_DATA, orderDetails);
		model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
		setupAdditionalFields(model);
		return getViewForPage(model);
	}
	
	/**
	 * This method is created for the Modify Payment page. 
	 */
	@GetMapping(value = "/modifyPayment/{orderCode}")
	@RequireHardLogIn
	public String getModifyPaymentForOrder(@PathVariable(value = ORDER_CODE ,required = false) final String orderCode, final Model model) throws CMSItemNotFoundException{
		final ContentPageModel modifyPaymentPage = getContentPageForLabelOrId(MODIFY_PAYMENT_CMS_PAGE);
		storeCmsPageInModel(model, modifyPaymentPage);
		setUpMetaDataForContentPage(model, modifyPaymentPage);
		final OrderData orderDetails = blOrderFacade.getOrderDetailsForCode(orderCode);
		model.addAttribute(ORDER_DATA, orderDetails);
		model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
		if (sessionService.getAttribute(BlCoreConstants.COUPON_APPLIED_MSG) != null)
		{
			model.addAttribute(BlCoreConstants.COUPON_APPLIED_MSG, sessionService.getAttribute(BlCoreConstants.COUPON_APPLIED_MSG));
			sessionService.removeAttribute(BlCoreConstants.COUPON_APPLIED_MSG);
		}
		setupAdditionalFields(model);
		return getViewForPage(model);
	}
	
	/**
	 * This method created for the PayBill confirmation page. 
	 */
	@PostMapping(value = "/payBillSuccess")
	@RequireHardLogIn
	public String getPayBillDetailsForOrder(final Model model, final HttpServletRequest request,
			final HttpServletResponse response) throws CMSItemNotFoundException{
		String orderCode = request.getParameter(ORDER_CODE);
		String paymentInfoId = request.getParameter("paymentId");
		String paymentMethodNonce = request.getParameter("paymentNonce");
		String billPayTotal = request.getParameter("payBillTotal");
		String poNumber = request.getParameter("extendPoNumber");
		String poNotes = request.getParameter("extendPoNotes");

		boolean isSuccess = false;
		double payBillAmount = Double.parseDouble(billPayTotal);
		AbstractOrderModel order = null;
		if ((StringUtils.isNotBlank(orderCode) && StringUtils.isNotBlank(paymentInfoId) &&
				StringUtils.isNotBlank(paymentMethodNonce)) || StringUtils.isNotBlank(poNumber) ) {
			order = brainTreeCheckoutFacade.getOrderByCode(orderCode);

				isSuccess = payBillSuccess(model, paymentInfoId, paymentMethodNonce, payBillAmount, poNumber,
						poNotes, order);

		}
		

		if (isSuccess) {
		  blOrderFacade.setResolvedStatusOnRepairLog(orderCode);
			final OrderData orderDetails = orderFacade.getOrderDetailsForCode(orderCode);
			order = brainTreeCheckoutFacade.getOrderByCode(orderCode);
		    PriceData payBillTotal  = convertDoubleToPriceData(payBillAmount, order);
			orderDetails.setOrderTotalWithTaxForPayBill(payBillTotal);
			model.addAttribute(ORDER_DATA, orderDetails);
			brainTreeCheckoutFacade.setPayBillFlagTrue(order);
			final ContentPageModel payBillSuccessPage = getContentPageForLabelOrId(
					BraintreeaddonControllerConstants.PAY_BILL_SUCCESS_CMS_PAGE);
			storeCmsPageInModel(model, payBillSuccessPage);
			setUpMetaDataForContentPage(model, payBillSuccessPage);
			model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS,
					ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
			return getViewForPage(model);
		} else {
			return REDIRECT_PREFIX + MY_ACCOUNT + orderCode + PAY_BILL;
		}
	}

	@PostMapping(value = "/modify-payment-success")
	@RequireHardLogIn
	public String doSelectPaymentMethod(final Model model, final HttpServletRequest request,
										final HttpServletResponse response, final RedirectAttributes redirectAttributes)throws CMSItemNotFoundException
	{
		String paymentInfoId = request.getParameter("paymentId");
		String paymentMethodNonce = request.getParameter("paymentNonce");
		String orderCode = request.getParameter(ORDER_CODE);
		String modifiedOrderTotal = request.getParameter("modifyOrderTotal");
		double modifyOrderTotal = Double.parseDouble(modifiedOrderTotal);
		AbstractOrderModel order = null;
		boolean isSuccess = false;
			
			 order = brainTreeCheckoutFacade.getOrderByCode(orderCode);
			if (StringUtils.isNotBlank(paymentInfoId))
			{
				if (StringUtils.isNotBlank(paymentMethodNonce))
				{
					brainTreeCheckoutFacade.setPaymentDetailsForModifyPayment(paymentInfoId, paymentMethodNonce, order);
				}
				if(null != order) {
					final BrainTreePaymentInfoModel paymentInfo = brainTreeCheckoutFacade
							.getBrainTreePaymentInfoForCode(
									(CustomerModel) order.getUser(), paymentInfoId, paymentMethodNonce);
					if (null != paymentInfo) {

						isSuccess = brainTreeTransactionService
								.createAuthorizationTransactionOfOrder(order,
										BigDecimal.valueOf(modifyOrderTotal).setScale(DECIMAL_PRECISION, RoundingMode.HALF_EVEN), true, paymentInfo);
					}
				}
				
			}
			
			
		if (isSuccess) {
			order.setStatus(OrderStatus.SHIPPED);
			order.setIsCaptured(Boolean.TRUE);
			modelService.save(order);
			GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.CONF_MESSAGES_HOLDER,
					getLocalizedString("text.account.modify.payment.success"));
			return REDIRECT_TO_ORDER_DETAILS_PAGE + orderCode;
		} else {
			GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.CONF_MESSAGES_HOLDER,
					getLocalizedString("text.account.modify.payment.error"));
			return REDIRECT_PREFIX + MY_ACCOUNT_MODIFY_PAYMENT + orderCode;
		}
		
	}
    /**
     * This method will return true after Authorization and capture done successfully
     *
     * @param paymentInfoId
     * @param paymentMethodNonce
     * @param billPayTotal
     * @param poNumber
     * @param poNotes
     * @return boolean
     *
     */
	private boolean payBillSuccess(final Model model, String paymentInfoId, final String paymentMethodNonce,
			final double billPayTotal, final String poNumber, String poNotes, final AbstractOrderModel order) {
		boolean isSuccess = false;
		if (null != order && StringUtils.isNotBlank(poNumber)) {
			isSuccess = blOrderFacade.savePoPaymentForPayBillOrder(poNumber, poNotes, order.getCode());
			if (BooleanUtils.isTrue(isSuccess)) {
				model.addAttribute(BlControllerConstants.PAYMENT_METHOD, BlControllerConstants.PO);
			}
		} else if(null != order) {
			final BrainTreePaymentInfoModel paymentInfo = brainTreeCheckoutFacade
					.getBrainTreePaymentInfoForCode(
							(CustomerModel) order.getUser(), paymentInfoId, paymentMethodNonce);
			if (null != paymentInfo) {

				isSuccess = brainTreeTransactionService
						.createAuthorizationTransactionOfOrder(order,
								BigDecimal.valueOf(billPayTotal).setScale(DECIMAL_PRECISION, RoundingMode.HALF_EVEN), true, paymentInfo);
			}
			if (BooleanUtils.isTrue(isSuccess)) {
				model.addAttribute(BlControllerConstants.PAYMENT_METHOD, BlControllerConstants.CREDIT_CARD);
			}
		}
		return isSuccess;
	}

	 
	private BrainTreeSubscriptionInfoData buildSubscriptionInfo(final String nonce, final String paymentProvider,                         // NOSONAR
                                                                final String cardDetails, final String cardType, final String email, final String deviceData,
                                                                final String liabilityShifted, final AddressData addressData, final String cardholder, final String defaultCard  )
	{
		final BrainTreeSubscriptionInfoData subscriptionInfo = new BrainTreeSubscriptionInfoData();
		subscriptionInfo.setPaymentProvider(paymentProvider);
		subscriptionInfo.setCardNumber(cardDetails);
		subscriptionInfo.setDeviceData(deviceData);
		subscriptionInfo.setCardType(cardType);
		subscriptionInfo.setNonce(nonce);
		subscriptionInfo.setEmail(email);
		subscriptionInfo.setAddressData(addressData);
		subscriptionInfo.setSavePaymentInfo(Boolean.TRUE);
		subscriptionInfo.setShouldBeSaved(Boolean.TRUE);
		subscriptionInfo.setCardholder(cardholder);
		//Added condition for default Card
		if(StringUtils.isNotBlank(defaultCard) && Boolean.TRUE.toString().equals(defaultCard))
		{
			subscriptionInfo.setIsDefault(Boolean.TRUE);
		}
		
		if (StringUtils.isNotBlank(liabilityShifted))
		{
			subscriptionInfo.setLiabilityShifted(Boolean.valueOf(liabilityShifted));
		}

		return subscriptionInfo;
	}

	private AddressData getAddressForPaymentInfo(String selectedAddressCode, List<AddressData> addressBook)
	{
		if (StringUtils.isNotBlank(selectedAddressCode))
		{
			for (AddressData addressData : addressBook)
			{
				if (selectedAddressCode.equals(addressData.getId()))
				{
					return addressData;
				}
			}
		}
		return null;
	}

	private AddressData getAddressForPaymentInfo(CCPaymentInfoData ccPaymentInfo, String selectedAddressCode,
                                                 List<AddressData> addressBook)
	{
		AddressData addressForPaymentInfo = getAddressForPaymentInfo(selectedAddressCode, addressBook);
		if (addressForPaymentInfo == null)
		{
			return ccPaymentInfo.getBillingAddress();
		}
		return addressForPaymentInfo;
	}
	
	private AddressData interpretResponseAddressData(final String selectedAddressId, final SopPaymentDetailsForm sopPaymentDetailsForm, 
		      final String companyName)
		  {
		    if (StringUtils.isNotBlank(selectedAddressId))
		    {
		      return blCustomerFacade.getAddressForCode(selectedAddressId);
		    }
		    final AddressData address = new AddressData();
		    final CountryData country = new CountryData();
		    country.setIsocode("US");
		    address.setCountry(country);
		    final RegionData region = new RegionData();
		    region.setIsocode(sopPaymentDetailsForm.getBillTo_state());
		    address.setRegion(region);
		    address.setTitleCode(sopPaymentDetailsForm.getBillTo_titleCode());
		    address.setFirstName(sopPaymentDetailsForm.getBillTo_firstName());
		    address.setLastName(sopPaymentDetailsForm.getBillTo_lastName());
		    address.setCompanyName(companyName);
		    address.setTown(sopPaymentDetailsForm.getBillTo_city());
		    address.setLine1(sopPaymentDetailsForm.getBillTo_street1());
		    address.setLine2(sopPaymentDetailsForm.getBillTo_street2());
		    address.setPostalCode(sopPaymentDetailsForm.getBillTo_postalCode());
		    address.setEmail(sopPaymentDetailsForm.getBillTo_email());
		    address.setPhone(sopPaymentDetailsForm.getBillTo_phoneNumber());
		    address.setBillingAddress(Boolean.TRUE);
		    address.setShippingAddress(Boolean.FALSE);
		    address.setPickStoreAddress(Boolean.FALSE);
		    address.setUpsStoreAddress(Boolean.FALSE);
		    return address;
		  }
	
	//Added method to set default credit card from frontend
	@RequestMapping(value = "/set-default-cc-payment-details", method = RequestMethod.POST)
	@RequireHardLogIn
	public String setDefaultPaymentDetails(@RequestParam(value = "paymentInfoId") final String paymentInfoId) {
		CCPaymentInfoData paymentInfoData = null;

		if (StringUtils.isNotBlank(paymentInfoId)) {
			paymentInfoData = userFacade.getCCPaymentInfoForCode(paymentInfoId);
		}
		userFacade.setDefaultPaymentInfo(paymentInfoData);
		return REDIRECT_TO_PAYMENT_INFO_PAGE;
	}
	
	/**
	 * This method is used to apply gift card.
	 *
	 * @param code  giftCard code
	 * @param request 
	 * @param model
	 * @return String. for the order
	 * @throws CMSItemNotFoundException
	 * @throws CalculationException
	 */
	@PostMapping(value = "/applyGiftCard")
	@ResponseBody
	public String apply(final String orderCode, final String code, final HttpServletRequest request, final Model model) {

		final AbstractOrderModel orderModel  = brainTreeCheckoutFacade.getOrderByCode(orderCode);
		final Locale locale = getI18nService().getCurrentLocale();
		if(StringUtils.isEmpty(code)){
			sessionService.setAttribute(BlCoreConstants.COUPON_APPLIED_MSG,
					getMessageSource().getMessage("text.gift.code.blank", null, locale));
			return BlControllerConstants.ERROR;
		}

		if(orderModel != null) {
			final GiftCardModel giftCardModel = blGiftCardFacade.getGiftCard(code);
			if (checkGcEndDate(locale, giftCardModel)) {
				return BlControllerConstants.ERROR;
			}
			
			final OrderData orderDetails = blOrderFacade.getOrderDetailsForCode(orderCode);
			final List<BLGiftCardData> blGiftCardDataList = orderDetails
					.getGiftCardData();
			final List<String> giftCardDataList = new ArrayList<>();
			if (CollectionUtils.isNotEmpty(blGiftCardDataList)) {
				for (BLGiftCardData giftCardData : blGiftCardDataList) {
					giftCardDataList.add(giftCardData.getCode());
				}
			}
			try {
				return handleGiftCardStatus(code, locale, giftCardDataList, orderModel, giftCardModel);
			} catch (final Exception exception) {
				BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
						"Error occurred while applying gift card code: {} on cart: {} for the customer: {}",
						code,
						orderModel.getCode(), orderModel.getUser().getUid(), exception);
			}
		}
		return BlControllerConstants.ERROR;
	}
	
	/**
	 * It removes applied gift card from Modified order .
	 * @param giftCardForm 
	 * @param request
	 * @param model
	 * @return String giftcard code and orderCode
	 */
	@PostMapping(value = "/removeGiftCard")
	@ResponseBody
	public String removeGiftCardForModifyOrder(final String orderCode, final String gcCode, final GiftCardForm giftCardForm, final HttpServletRequest request, final Model model) {
		final AbstractOrderModel orderModel  = brainTreeCheckoutFacade.getOrderByCode(orderCode);
		try {
			blGiftCardFacade.removeGiftCardforModifyOrder(gcCode, orderModel);
			return BlControllerConstants.TRUE_STRING;
		} catch (final Exception exception) {
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
					"Error while removing applied gift card code: {} from cart: {} for the customer: {}",
					gcCode, orderModel.getCode(), orderModel.getUser().getUid(), exception);
			return BlControllerConstants.FALSE_STRING;
		}
	}
	
	/**
	 * Check end date for gift card
	 * @param locale
	 * @param giftCardModel for current GiftCard
	 * @return boolean true/false
	 */
	private boolean checkGcEndDate(Locale locale, GiftCardModel giftCardModel) {
		if (giftCardModel != null) {
			int endDate = giftCardModel.getEndDate().compareTo(new Date());
			if(endDate < 0)
			{
				sessionService.setAttribute(BlCoreConstants.COUPON_APPLIED_MSG,
						getMessageSource().getMessage("text.gift.apply.applied.expire", null, locale));
				return true;
			}
		}
		return false;
	}

	/**
	 * It sets message in the session attribute based on the gift card apply operation.
	 *
	 * @param code GiftCard code
	 * @param locale
	 * @param giftCardData list of Gift Card
	 * @param orderModel current order
	 * @param giftCardModel current gift card
	 * @return String. for GiftCard code
	 */
	private String handleGiftCardStatus(final String code, final Locale locale,
			final List<String> giftCardData, final AbstractOrderModel orderModel,
			final GiftCardModel giftCardModel) {
		if (CollectionUtils.isNotEmpty(giftCardData) && giftCardData.contains(code)
				&& isOrderFullyPaid(orderModel)) {
			//if cart already have applied GC  and order total is 0.00
			sessionService.setAttribute(BlCoreConstants.COUPON_APPLIED_MSG,
					getMessageSource().getMessage("text.gift.apply.applied.again", null, locale));
			return BlControllerConstants.ERROR;
		} else if (CollectionUtils.isNotEmpty(giftCardData) && giftCardData.contains(code)) {
			//if cart already have applied GC and GC have insufficient balance
			sessionService.setAttribute(BlCoreConstants.COUPON_APPLIED_MSG,
					getMessageSource().getMessage("text.gift.cart.insufficient.balance", new Object[]
							{code}, locale));
			return BlControllerConstants.ERROR;
		}
		if (blGiftCardFacade.applyGiftCardForModifyOrder(code, orderModel)) {
			final List<GiftCardMovementModel> giftCardMovementModelList = giftCardModel.getMovements();
			final BigDecimal gcRedeemedAmount = BigDecimal.valueOf(giftCardMovementModelList.get(giftCardMovementModelList.size()-1).getAmount()).setScale(2, RoundingMode.HALF_DOWN);
			sessionService.setAttribute(BlCoreConstants.COUPON_APPLIED_MSG,
					getMessageSource().getMessage("text.gift.apply.success", new Object[]
							{gcRedeemedAmount.abs().doubleValue()}, locale));
			return BlControllerConstants.SUCCESS;
		} else {
			if (giftCardModel == null) {
				//if applied GC is not present in the system.
				sessionService.setAttribute(BlCoreConstants.COUPON_APPLIED_MSG,
						getMessageSource().getMessage("text.gift.apply.applied.fail", null, locale));
				return BlControllerConstants.ERROR;
			} else if (isOrderFullyPaid(orderModel)) {
				//if order total is 0.00
				sessionService.setAttribute(BlCoreConstants.COUPON_APPLIED_MSG,
						getMessageSource().getMessage("text.gift.apply.applied.again", null, locale));
				return BlControllerConstants.ERROR;
			}
		}
		return BlControllerConstants.ERROR;
	}

	/**
	 * It checks that cart total price shouldn't be zero or less than zero.
	 *
	 * @param orderModel current order
	 * @return boolean value
	 */
	private boolean isOrderFullyPaid(final AbstractOrderModel orderModel) {
		return orderModel.getTotalPrice() <= 0;
	}

	/**
	 * This method create to get the URL based on order code
	 * @param orderCode orderCode
	 * @return string
	 */
	private String getRedirectionUrl(final String orderCode) {
		return orderCode.contains(BlControllerConstants.EXTEND) ? BlControllerConstants.EXTEND : BlControllerConstants.MODIFYPAYMENT;
	}

	/**
	 * This method converts double to price data
	 */
	private PriceData convertDoubleToPriceData(final Double price , final AbstractOrderModel orderModel) {
		return priceDataFactory.create(PriceDataType.BUY ,BigDecimal.valueOf(price),orderModel.getCurrency());
	}
	
  /**
   * This method is created for the Deposit Payment page.
   */
  @GetMapping(value = "/{orderCode}/depositPayment")
  @RequireHardLogIn
  public String getDepositPaymentForOrder(@PathVariable(value = ORDER_CODE, required = false) final String orderCode, final Model model)
      throws CMSItemNotFoundException
  {
    final ContentPageModel depositPaymentPage = getContentPageForLabelOrId(DEPOSIT_PAYMENT_CMS_PAGE);
    storeCmsPageInModel(model, depositPaymentPage);
    setUpMetaDataForContentPage(model, depositPaymentPage);
    final OrderData orderDetails = blOrderFacade.getOrderDetailsForCode(orderCode);
    model.addAttribute(ORDER_DATA, orderDetails);
    model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
    setupAdditionalFields(model);
    return getViewForPage(model);
  }
  
  @PostMapping(value = "/deposit-payment-success")
  @RequireHardLogIn
  public String getDepositPaymentForOrder(final Model model, final HttpServletRequest request, final HttpServletResponse response,
      final RedirectAttributes redirectAttributes) throws CMSItemNotFoundException
  {
    final String paymentInfoId = request.getParameter("paymentId");
    final String paymentMethodNonce = request.getParameter("paymentNonce");
    final String orderCode = request.getParameter(ORDER_CODE);
    final String depositTotal = request.getParameter("depositOrderTotal");
    System.out.println(paymentInfoId + " ==== " + paymentMethodNonce + " ==== " + orderCode + " ==== " + depositTotal);
    try
    {      
      if (isParametersEligible(paymentInfoId, paymentMethodNonce, orderCode, depositTotal))
      {
        boolean isSuccess = false;
        final double depositOrderTotal = Double.parseDouble(depositTotal);
        final AbstractOrderModel order = brainTreeCheckoutFacade.getOrderByCode(orderCode);
        if (Objects.nonNull(order))
        {
          final BrainTreePaymentInfoModel paymentInfo =
              brainTreeCheckoutFacade.getBrainTreePaymentInfoForCodeToDeposit((CustomerModel) order.getUser(), paymentInfoId, paymentMethodNonce, depositOrderTotal);
          if (Objects.nonNull(paymentInfo))
          {
            isSuccess = brainTreeTransactionService.createAuthorizationTransactionOfOrder(order,
                BigDecimal.valueOf(depositOrderTotal).setScale(DECIMAL_PRECISION, RoundingMode.HALF_EVEN), true, paymentInfo);
          }
        }
        if (isSuccess)
        {
          final OrderData orderDetails = orderFacade.getOrderDetailsForCode(orderCode);
          final PriceData billPayTotal  = convertDoubleToPriceData(depositOrderTotal, order);
          model.addAttribute("orderData", orderDetails);
          model.addAttribute("depositAmount", billPayTotal);
          model.addAttribute("paymentType", "Credit Card");
          final ContentPageModel payBillSuccessPage = getContentPageForLabelOrId(BraintreeaddonControllerConstants.DEPOSIT_SUCCESS_CMS_PAGE);
          storeCmsPageInModel(model, payBillSuccessPage);
          setUpMetaDataForContentPage(model, payBillSuccessPage);
          model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
          return getViewForPage(model);
        }
      }
    }
    catch (final Exception exception)
    {
      BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception,
          "Error occurred while making deposit for order : {} with ammount : {} with PaymentID - {}", orderCode, depositTotal, paymentInfoId);
    }
    return REDIRECT_PREFIX + MY_ACCOUNT_DEPOSIT_PAYMENT + orderCode;
  }
  
  


  /**
   * Checks if is parameters eligible.
   *
   * @param paymentInfoId the payment info id
   * @param paymentMethodNonce the payment method nonce
   * @param orderCode the order code
   * @param depositTotal the deposit total
   * @return true, if is parameters eligible
   */
  private boolean isParametersEligible(String paymentInfoId, String paymentMethodNonce, String orderCode, String depositTotal)
  {
    return StringUtils.isNotBlank(depositTotal) && StringUtils.isNotBlank(paymentInfoId) && StringUtils.isNotBlank(paymentMethodNonce)
        && StringUtils.isNotBlank(orderCode);
  }
}
