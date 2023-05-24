package com.bl.storefront.controllers.pages;


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
import org.springframework.validation.ObjectError;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import com.bl.logging.BlLogger;

/**
 *
 *This is created to override processRegisterUserRequest method to perform bl specific registration.
 * @author vijay vishwakarma
 *
 */

public abstract class AbstractBlLoginPageController extends AbstractLoginPageController {

    private static final Logger LOGGER = Logger.getLogger(AbstractBlLoginPageController.class);

    /*
     * This method override to remove guest form, breadcrumb and consent form related unwanted code.
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
            // This code added temporary to show the error message. Once we have the user story needs to change the code accordingly
            final StringBuilder stringBuilder = new StringBuilder(BlControllerConstants.ERROR_MESSAGE);
            for(final ObjectError objectError : bindingResult.getAllErrors()) {
              stringBuilder.append(objectError.getCode()).append(BlControllerConstants.RATIO);
            }
            return stringBuilder.toString();
        }
        final RegisterData data = new RegisterData();
        data.setLogin(form.getEmail());
        data.setPassword(form.getPwd());
		  data.setFirstName(form.getFirstName());
		  data.setLastName(form.getLastName());
        try
        {
            getCustomerFacade().register(data);
            getAutoLoginStrategy().login(form.getEmail().toLowerCase(), form.getPwd(), request, response); // NOSONAR
            GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.CONF_MESSAGES_HOLDER,
                BlControllerConstants.REGISTRATION_CONFIRMATION_MESSAGE);

        }
        catch (final DuplicateUidException duplicateUidException)
        {
            BlLogger.logFormatMessageInfo(LOGGER,Level.ERROR,"Registration failed due to duplicated uid : {}",form.getEmail());
            model.addAttribute(form);
            model.addAttribute(new LoginForm());
            bindingResult.rejectValue(BlControllerConstants.EMAIL, BlControllerConstants.DUBLICATE_UID_ERROR);
            GlobalMessages.addErrorMessage(model, BlControllerConstants.FORM_GLOBAL_ERROR);
            return  BlControllerConstants.DUBLICATE_UID_ERROR;
        }

        return REDIRECT_PREFIX + getSuccessRedirect(request, response);
    }
}
