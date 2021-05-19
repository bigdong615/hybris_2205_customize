package com.bl.storefront.validator;

import com.bl.storefront.controllers.pages.BlControllerConstants;

import de.hybris.platform.acceleratorstorefrontcommons.constants.WebConstants;
import de.hybris.platform.acceleratorstorefrontcommons.forms.RegisterForm;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import javax.annotation.Resource;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
    public boolean supports(Class<?> aClass) {
        return RegisterForm.class.equals(aClass);
    }

    @Override
    public void validate(final Object object, final Errors errors) {
        RegisterForm registerForm = (RegisterForm)object;
        final String email = registerForm.getEmail();
        final String pwd = registerForm.getPwd();
        final String checkPwd = registerForm.getCheckPwd();

        validateEmail(errors, email);
        validatePassword(errors, pwd);
        comparePasswords(errors, pwd, checkPwd);
    }

    protected void comparePasswords(final Errors errors, final String pwd, final String checkPwd)
    {
        if (StringUtils.isNotEmpty(pwd) && StringUtils.isNotEmpty(checkPwd) && !StringUtils.equals(pwd, checkPwd))
        {
            errors.rejectValue(BlControllerConstants.CONFIRM_PASSWORD_STRING, BlControllerConstants.VALIDATE_CHECKPWD_EQUALS);
        }
        else
        {
            if (StringUtils.isEmpty(checkPwd))
            {
                errors.rejectValue(BlControllerConstants.CONFIRM_PASSWORD_STRING, BlControllerConstants.REGISTER_CHECKPWD_INVALID);
            }
        }
    }

    protected void validatePassword(final Errors errors, final String pwd)
    {
        if (StringUtils.isEmpty(pwd) || StringUtils.length(pwd) < BlControllerConstants.PASSWORD_MIN_LENGTH || StringUtils.length(pwd) >BlControllerConstants.PASSWORD_MAX_LENGTH)
        {
            errors.rejectValue(BlControllerConstants.PASSWORD_STRING, BlControllerConstants.REGISTER_PWD_INVALID);
        }
    }

    protected void validateEmail(final Errors errors, final String email)
    {
        if (StringUtils.isEmpty(email))
        {
            errors.rejectValue(BlControllerConstants.EMAIL, BlControllerConstants.REGISTER_EMAIL_EMPTY);
        }
        else if (StringUtils.length(email) > BlControllerConstants.EMAIL_MAX_LENGTH || !validateEmailAddress(email))
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
