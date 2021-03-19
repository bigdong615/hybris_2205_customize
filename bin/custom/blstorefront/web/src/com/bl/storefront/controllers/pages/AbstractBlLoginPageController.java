package com.bl.storefront.controllers.pages;

import com.bl.storefront.form.BlRegisterForm;
import de.hybris.platform.acceleratorstorefrontcommons.breadcrumb.Breadcrumb;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.ThirdPartyConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.pages.AbstractLoginPageController;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.util.GlobalMessages;
import de.hybris.platform.acceleratorstorefrontcommons.forms.GuestForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.LoginForm;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.cms2.model.pages.ContentPageModel;
import org.springframework.ui.Model;

import javax.servlet.http.HttpSession;
import java.util.Collections;

/**
 *
 *This is created to provide bl register form for register.
 * @author vijay vishwakarma
 *
 */

public abstract class AbstractBlLoginPageController extends AbstractLoginPageController {

    /**
     * TODO : This code will be remove when ui for sign up popup is available.
    * This method is used for rendering login page ,sign and guest form.
     */
    @Override
    protected String getDefaultLoginPage(final boolean loginError, final HttpSession session, final Model model)
            throws CMSItemNotFoundException
    {
        final LoginForm loginForm = new LoginForm();
        model.addAttribute(loginForm);
        model.addAttribute(new BlRegisterForm()); // changes in registration form
        model.addAttribute(new GuestForm());

        final String username = (String) session.getAttribute(SPRING_SECURITY_LAST_USERNAME);
        if (username != null)
        {
            session.removeAttribute(SPRING_SECURITY_LAST_USERNAME);
        }

        loginForm.setJ_username(username);
        storeCmsPageInModel(model, getCmsPage());
        setUpMetaDataForContentPage(model, (ContentPageModel) getCmsPage());
        model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.INDEX_NOFOLLOW);

        addRegistrationConsentDataToModel(model);

        final Breadcrumb loginBreadcrumbEntry = new Breadcrumb("#",
                getMessageSource().getMessage("header.link.login", null, "header.link.login", getI18nService().getCurrentLocale()),
                null);
        model.addAttribute("breadcrumbs", Collections.singletonList(loginBreadcrumbEntry));

        if (loginError)
        {
            model.addAttribute("loginError", Boolean.valueOf(loginError));
            GlobalMessages.addErrorMessage(model, "login.error.account.not.found.title");
        }

        return getView();
    }
}
