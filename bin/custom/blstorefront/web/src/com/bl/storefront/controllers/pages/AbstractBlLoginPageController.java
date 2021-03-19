package com.bl.storefront.controllers.pages;

import static de.hybris.platform.commercefacades.constants.CommerceFacadesConstants.CONSENT_GIVEN;

import com.bl.logging.BlLogger;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.pages.AbstractLoginPageController;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.util.GlobalMessages;
import de.hybris.platform.acceleratorstorefrontcommons.forms.LoginForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.RegisterForm;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.commercefacades.user.data.RegisterData;
import de.hybris.platform.commerceservices.customer.DuplicateUidException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.ui.Model;

import org.springframework.validation.BindingResult;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

/**
 *
 *This is created to provide bl register form for register.
 * @author vijay vishwakarma
 *
 */

public abstract class AbstractBlLoginPageController extends AbstractLoginPageController {

    private static final Logger LOGGER = Logger.getLogger(AbstractBlLoginPageController.class);

    /*
     * Changing the register form to bl specific register form
     */
    @Override
    protected String processRegisterUserRequest(final String referer, final RegisterForm form, final BindingResult bindingResult,
        final Model model, final HttpServletRequest request, final HttpServletResponse response,
        final RedirectAttributes redirectModel) throws CMSItemNotFoundException // NOSONAR
    {
        if (bindingResult.hasErrors())
        {
            model.addAttribute(form);
            model.addAttribute(new LoginForm());
            GlobalMessages.addErrorMessage(model, BlControllerConstants.FORM_GLOBAL_ERROR);
            return handleRegistrationError(model);
        }
        final RegisterData data = new RegisterData();
        data.setLogin(form.getEmail());
        data.setPassword(form.getPwd());
        try
        {
            getCustomerFacade().register(data);
            getAutoLoginStrategy().login(form.getEmail().toLowerCase(), form.getPwd(), request, response); // NOSONAR
            GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.CONF_MESSAGES_HOLDER,
                BlControllerConstants.REGISTRATION_CONFIRMATION_MESSAGE);

        }
        catch (final DuplicateUidException duplicateUidException)
        {
            BlLogger.logMessage(LOGGER, Level.ERROR, "registration failed due to unique uid");
            BlLogger.logFormatMessageInfo(LOGGER,Level.ERROR,"Registration failed due to duplicated uid : {}",form.getEmail());
            model.addAttribute(form);
            model.addAttribute(new LoginForm());
            bindingResult.rejectValue("email", BlControllerConstants.DUBLICATE_UID_ERROR);
            GlobalMessages.addErrorMessage(model, BlControllerConstants.FORM_GLOBAL_ERROR);
            return handleRegistrationError(model);
        }

        return REDIRECT_PREFIX + getSuccessRedirect(request, response);
    }
}
