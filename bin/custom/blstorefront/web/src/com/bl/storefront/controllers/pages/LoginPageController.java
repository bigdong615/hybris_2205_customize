/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.storefront.controllers.pages;

import com.bl.storefront.controllers.ControllerConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.ThirdPartyConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.util.GlobalMessages;
import de.hybris.platform.acceleratorstorefrontcommons.forms.LoginForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.RegisterForm;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.cms2.model.pages.AbstractPageModel;
import de.hybris.platform.cms2.model.pages.ContentPageModel;
import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import org.apache.commons.lang.StringUtils;
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
import org.springframework.web.servlet.mvc.support.RedirectAttributes;


/**
 * Login Controller. Handles login and register for the account flow.
 */
@Controller
@RequestMapping(value = "/login")
public class LoginPageController extends AbstractBlLoginPageController
{
	private static final Logger LOGGER = Logger.getLogger(LoginPageController.class);
	private HttpSessionRequestCache httpSessionRequestCache;

	@Resource(name = "blRegisterFormValidator")
	private Validator blRegisterFormValidator;

	@Override
	protected String getView()
	{
		return ControllerConstants.Views.Pages.Account.AccountLoginPage;
	}

	@Override
	protected String getSuccessRedirect(final HttpServletRequest request, final HttpServletResponse response)
	{
		if (httpSessionRequestCache.getRequest(request, response) != null)
		{
			return httpSessionRequestCache.getRequest(request, response).getRedirectUrl();
		}
		String redirectUrl=request.getHeader("Referer");
		// TODO :Only redirect to referer once start working on html integration : BL-30 // NOSONAR
		return redirectUrl.contains("/login")? "/":redirectUrl;
	}

	@Override
	protected AbstractPageModel getCmsPage() throws CMSItemNotFoundException
	{
		return getContentPageForLabelOrId("login");
	}


	@Resource(name = "httpSessionRequestCache")
	public void setHttpSessionRequestCache(final HttpSessionRequestCache accHttpSessionRequestCache)
	{
		this.httpSessionRequestCache = accHttpSessionRequestCache;
	}

	@GetMapping
	public String doLogin(@RequestHeader(value = "referer", required = false) final String referer,
			@RequestParam(value = "error", defaultValue = "false") final boolean loginError, final Model model,
			final HttpServletRequest request, final HttpServletResponse response, final HttpSession session)
			throws CMSItemNotFoundException
	{
		if (!loginError)
		{
			storeReferer(referer, request, response);
		}
		return getDefaultLoginPage(loginError, session, model);
	}

	protected void storeReferer(final String referer, final HttpServletRequest request, final HttpServletResponse response)
	{
		if (StringUtils.isNotBlank(referer) && !StringUtils.endsWith(referer, "/login")
				&& StringUtils.contains(referer, request.getServerName()))
		{
			httpSessionRequestCache.saveRequest(request, response);
		}
	}

	@PostMapping(value = "/register")
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
			model.addAttribute("loginError", Boolean.valueOf(loginError));
			GlobalMessages.addErrorMessage(model, "login.error.account.not.found.title");
		} else {
			storeReferer(referer, request, response);
		}
		model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS,
				ThirdPartyConstants.SeoRobots.INDEX_NOFOLLOW);
	}

}
