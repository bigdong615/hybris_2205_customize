/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.storefront.controllers.pages;

import com.bl.core.constants.BlCoreConstants;
import com.bl.logging.BlLogger;
import com.bl.storefront.controllers.ControllerConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.ThirdPartyConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.util.GlobalMessages;
import de.hybris.platform.acceleratorstorefrontcommons.forms.GuestForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.LoginForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.RegisterForm;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.cms2.model.pages.AbstractPageModel;
import de.hybris.platform.cms2.model.pages.ContentPageModel;
import de.hybris.platform.servicelayer.exceptions.UnknownIdentifierException;
import de.hybris.platform.servicelayer.user.UserService;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.security.web.savedrequest.HttpSessionRequestCache;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.validation.Validator;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;


/**
 * Login Controller. Handles login and register for the account flow.
 */
@Controller
@RequestMapping(value = "/login")
public class LoginPageController extends AbstractBlLoginPageController
{
	private static final Logger LOG = Logger.getLogger(LoginPageController.class);
	
	private HttpSessionRequestCache httpSessionRequestCache;

	@Resource(name = "blRegisterFormValidator")
	private Validator blRegisterFormValidator;
	
	@Resource(name = "userService")
	private UserService userService;

	@Override
	protected String getView()
	{
		return ControllerConstants.Views.Pages.Account.AccountLoginPage;
	}

	@Override
	protected String getSuccessRedirect(final HttpServletRequest request, final HttpServletResponse response)
	{
		if (null != httpSessionRequestCache.getRequest(request, response))
		{
			return httpSessionRequestCache.getRequest(request, response).getRedirectUrl();
		}
		return request.getHeader(BlControllerConstants.REFERER);
	}

	@Override
	protected AbstractPageModel getCmsPage() throws CMSItemNotFoundException
	{
		return getContentPageForLabelOrId(BlControllerConstants.LOG_IN);
	}


	@Resource(name = "httpSessionRequestCache")
	public void setHttpSessionRequestCache(final HttpSessionRequestCache accHttpSessionRequestCache)
	{
		this.httpSessionRequestCache = accHttpSessionRequestCache;
	}

	@GetMapping
	@ResponseBody
	public String doLogin(@RequestHeader(value = "referer", required = false) final String referer,
			@RequestParam(value = "error", defaultValue = "false") final boolean loginError, final Model model,
			final HttpServletRequest request, final HttpServletResponse response, final HttpSession session)
			throws CMSItemNotFoundException
	{
		String userIp = getUserIp(request);
		if (!loginError)
		{
			storeReferer(referer, request, response);
			LOG.info("Customer with email id :- "+(String) session.getAttribute(SPRING_SECURITY_LAST_USERNAME)+ " Logged in successfully with IP address :- "+ userIp);
		}
		else {
			LOG.info("Customer with email id :- "+(String) session.getAttribute(SPRING_SECURITY_LAST_USERNAME)+ " Login was unsuccessfull with IP address :- "+ userIp);
		}
		final LoginForm loginForm = new LoginForm();
		model.addAttribute(loginForm);
		model.addAttribute(new RegisterForm());
		model.addAttribute(new GuestForm());

		final String username = (String) session.getAttribute(SPRING_SECURITY_LAST_USERNAME);
		final String userId = username;
		if (username != null)
		{
			session.removeAttribute(SPRING_SECURITY_LAST_USERNAME);
		}

		loginForm.setJ_username(username);
		if (loginError)
		{
			model.addAttribute(BlControllerConstants.LOG_IN_ERROR, loginError);
			try
			{
				if (BooleanUtils.isTrue(userService.getUserForUID(userId).isLoginDisabled()))
				{
					GlobalMessages.addErrorMessage(model, BlControllerConstants.ACCOUNT_DEACTIVATED);
					BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlControllerConstants.ACCOUNT_DEACTIVATED_MSG, userId);
					return BlControllerConstants.ACCOUNT_DEACTIVATED.trim();
				}
				else
				{
					GlobalMessages.addErrorMessage(model, BlControllerConstants.LOGIN_EMAIL_OR_PASSWORD_INCORRECT);
					BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlControllerConstants.LOGIN_EMAIL_OR_PASSWORD_INCORRECT_MSG, userId);
				}
				return BlControllerConstants.LOGIN_EMAIL_OR_PASSWORD_INCORRECT;
			}
			catch (final UnknownIdentifierException ex)
			{
				GlobalMessages.addErrorMessage(model, BlControllerConstants.LOGIN_EMAIL_OR_PASSWORD_INCORRECT);
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlControllerConstants.LOGIN_EMAIL_OR_PASSWORD_INCORRECT_MSG, userId);
				return BlControllerConstants.LOGIN_EMAIL_OR_PASSWORD_INCORRECT;
			}
		}
		return ControllerConstants.Views.Fragments.Login.LoginPopup;
	}
	
	private String getUserIp(final HttpServletRequest request)
	{
		final String trueClient = "True-Client-IP";
		final String cfConnectingIP = "CF-Connecting-IP";
		final String forwardFor = "X-Forwarded-For";
		String shopperIP = request.getHeader(trueClient);
		String headerUsed = trueClient;
		if (shopperIP == null)
		{
			headerUsed = cfConnectingIP;
			shopperIP = request.getHeader(cfConnectingIP);
		}
		if (shopperIP == null)
		{
			headerUsed = forwardFor;
			final String xfowardedHeader = request.getHeader(forwardFor);
			// to be extra sure I don't want this to throw an exception for any reason, so I'll add a try catch
			try
			{
				if (StringUtils.isNotBlank(xfowardedHeader))
				{
					final String[] headerParts = xfowardedHeader.split(",");
					if (headerParts.length > 0)
					{
						shopperIP = request.getHeader(headerParts[0]);
					}
				}
			}
			catch (final Exception ex)
			{
				LOG.info("Could not assign shopper IP from X-Forward-For header ", ex);
			}
		}
		if (shopperIP == null)
		{
			headerUsed = "Remote Address";
			shopperIP = request.getRemoteAddr();
		}
		if (shopperIP == null)
		{
			shopperIP = "";
		}
		return shopperIP;
	}

	protected void storeReferer(final String referer, final HttpServletRequest request, final HttpServletResponse response)
	{
		if (StringUtils.isNotBlank(referer) && !StringUtils.endsWith(referer, BlControllerConstants.LOG_IN_URL)
				&& StringUtils.contains(referer, request.getServerName()))
		{
			httpSessionRequestCache.saveRequest(request, response);
		}
	}

	@PostMapping(value = "/register")
	@ResponseBody
	public String doRegister(@RequestHeader(value = "referer", required = false) final String referer, final RegisterForm form,
			final BindingResult bindingResult, final Model model, final HttpServletRequest request,
			final HttpServletResponse response, final RedirectAttributes redirectModel) throws CMSItemNotFoundException
	{
		blRegisterFormValidator.validate(form, bindingResult);
		return processRegisterUserRequest(referer, form, bindingResult, model, request, response, redirectModel);
	}

	@GetMapping(value = "/register/termsandconditions")
	public String getTermsAndConditions(final Model model) throws CMSItemNotFoundException
	{
		final ContentPageModel pageForRequest = getCmsPageService().getPageForLabel("/termsAndConditions"); // NOSONAR
		storeCmsPageInModel(model, pageForRequest);
		setUpMetaDataForContentPage(model, pageForRequest);
		return ControllerConstants.Views.Fragments.Checkout.TermsAndConditionsPopup;
	}


	/**
	 * This method is responsible for render login popup.
	 */
	@GetMapping(value = "/loginpopup")
	public String loginPopup(@RequestHeader(value = "referer", required = false) final String referer,
			@RequestParam(value = "error", defaultValue = "false") final boolean loginError,
			final Model model,
			final HttpServletRequest request, final HttpServletResponse response,
			final HttpSession session)
			throws CMSItemNotFoundException {
		final LoginForm loginForm = new LoginForm();
		model.addAttribute(loginForm);
		final String username = (String) session.getAttribute(SPRING_SECURITY_LAST_USERNAME);
		if (username != null) {
			session.removeAttribute(SPRING_SECURITY_LAST_USERNAME);
		}
		loginForm.setJ_username(username);
		addModelAttributes(loginError, referer, request, response, model);
		return ControllerConstants.Views.Fragments.Login.LoginPopup;
	}

	/**
	 * This method is responsible for render registration popup.
	 */
	@GetMapping(value = "/register")
	public String doRegistrationRequest(
			@RequestHeader(value = "referer", required = false) final String referer,
			@RequestParam(value = "error", defaultValue = "false") final boolean loginError,
			final Model model,
			final HttpServletRequest request, final HttpServletResponse response,
			final HttpSession session)
			throws CMSItemNotFoundException {
		model.addAttribute(new RegisterForm());
		final String username = (String) session.getAttribute(SPRING_SECURITY_LAST_USERNAME);
		if (username != null) {
			session.removeAttribute(SPRING_SECURITY_LAST_USERNAME);
		}
		addModelAttributes(loginError, referer, request, response, model);
		return ControllerConstants.Views.Fragments.Login.CreateAccountPopup;
	}

	/**
	 * This method is responsible for showing error message.
	 */
	private void addModelAttributes(final boolean loginError, final String referer,
			final HttpServletRequest request, final HttpServletResponse response, final Model model) {
		if (loginError) {
			model.addAttribute(BlControllerConstants.LOG_IN_ERROR, loginError);
			GlobalMessages.addErrorMessage(model, BlControllerConstants.LOGIN_EMAIL_OR_PASSWORD_INCORRECT);
		} else {
			storeReferer(referer, request, response);
		}
		model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS,
				ThirdPartyConstants.SeoRobots.INDEX_NOFOLLOW);
	}

	/**
	 * This method used to check is previous session present or not.
	 */
	@GetMapping(value = "/isSessionPresent")
	@ResponseBody
	public String checkSessionPresent(final Model model,
			final HttpServletRequest request, final HttpServletResponse response){
		final HttpSession session = request.getSession(false);
		 return  (session != null && !session.isNew()) ? BlControllerConstants.SUCCESS :BlCoreConstants.FAILURE_STRING;
	}

}
