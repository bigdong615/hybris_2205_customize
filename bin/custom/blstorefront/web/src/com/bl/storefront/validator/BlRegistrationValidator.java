package com.bl.storefront.validator;

import de.hybris.platform.acceleratorstorefrontcommons.constants.WebConstants;
import de.hybris.platform.acceleratorstorefrontcommons.forms.RegisterForm;
import de.hybris.platform.servicelayer.config.ConfigurationService;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.annotation.Resource;

import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.bl.storefront.controllers.pages.BlControllerConstants;

/**
 *{javadoc}
 *
 * @author vijay vishwakarma
 * This validater use to validate bl register form.
 */

@Component("blRegisterFormValidator")
public class BlRegistrationValidator implements Validator{

    @Resource(name = "configurationService")
    private ConfigurationService configurationService;

    @Override
    public boolean supports(final Class<?> aClass) {
        return RegisterForm.class.equals(aClass);
    }

    @Override
    public void validate(final Object object, final Errors errors) {
        final RegisterForm registerForm = (RegisterForm)object;
        final String email = registerForm.getEmail();
        final String password = registerForm.getPwd();
        final String checkPassword = registerForm.getCheckPwd();
		  final String firstName = registerForm.getFirstName();
		  final String lastName = registerForm.getLastName();

        validateEmail(errors, email);
		  validateFirstName(errors, firstName);
		  validateLastName(errors, lastName);
		  validateEmail(errors, email);
        validatePassword(errors, password);
        comparePasswords(errors, password, checkPassword);
    }

	 /**
	  * @param errors
	  * @param firstName
	  */
	 private void validateFirstName(final Errors errors, final String firstName)
	 {
		 if (firstName.trim().isEmpty())
		 {
			 errors.rejectValue(BlControllerConstants.FIRSTNAME, BlControllerConstants.REGISTER_FIRSTNAME_INVALID);
		 }
	 }

	 private void validateLastName(final Errors errors, final String lastName)
	 {
		 if (lastName.trim().isEmpty())
		 {
			 errors.rejectValue(BlControllerConstants.LASTNAME, BlControllerConstants.REGISTER_LASTNAME_INVALID);
		 }
	 }

	 protected void comparePasswords(final Errors errors, final String password, final String checkPassword)
    {
        if (StringUtils.isNotEmpty(password) && StringUtils.isNotEmpty(checkPassword) && !StringUtils.equals(password, checkPassword))
        {
            errors.rejectValue(BlControllerConstants.CONFIRM_PASSWORD_STRING, BlControllerConstants.VALIDATE_CHECKPASSWORD_EQUALS);
        }
        else
        {
            if (StringUtils.isEmpty(checkPassword))
            {
                errors.rejectValue(BlControllerConstants.CONFIRM_PASSWORD_STRING, BlControllerConstants.REGISTER_CHECKPASSWORD_INVALID);
            }
        }
    }

    protected void validatePassword(final Errors errors, final String password)
    {
        if (BlControllerConstants.PASSWORD_MIN_LENGTH > StringUtils.length(password))
        {
            errors.rejectValue(BlControllerConstants.PASSWORD_STRING, BlControllerConstants.REGISTER_PASSWORD_INVALID);
        }
    }

    protected void validateEmail(final Errors errors, final String email)
    {
        if (BlControllerConstants.EMAIL_MAX_LENGTH < StringUtils.length(email) || !validateEmailAddress(email))
        {
            errors.rejectValue(BlControllerConstants.EMAIL, BlControllerConstants.REGISTER_EMAIL_INVALID);
        }
    }

    public boolean validateEmailAddress(final String email)
    {
        final Matcher matcher = Pattern.compile(configurationService.getConfiguration().getString(WebConstants.EMAIL_REGEX))
                .matcher(email);
        return matcher.matches();
    }


}
