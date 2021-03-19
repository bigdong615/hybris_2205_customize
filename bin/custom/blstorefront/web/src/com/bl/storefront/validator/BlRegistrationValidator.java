package com.bl.storefront.validator;

import com.bl.storefront.form.BlRegisterForm;
import de.hybris.platform.acceleratorstorefrontcommons.constants.WebConstants;
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

@Component("registerFormValidator")
public class BlRegistrationValidator implements Validator{

    @Resource(name = "configurationService")
    private ConfigurationService configurationService;

    @Override
    public boolean supports(Class<?> aClass) {
        return BlRegisterForm.class.equals(aClass);
    }

    @Override
    public void validate(final Object object, final Errors errors) {
        BlRegisterForm registerForm = (BlRegisterForm)object;
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
            errors.rejectValue("checkPwd", "validation.checkPwd.equals");
        }
        else
        {
            if (StringUtils.isEmpty(checkPwd))
            {
                errors.rejectValue("checkPwd", "register.checkPwd.invalid");
            }
        }
    }

    protected void validatePassword(final Errors errors, final String pwd)
    {
        if (StringUtils.isEmpty(pwd))
        {
            errors.rejectValue("pwd", "register.pwd.invalid");
        }
        else if (StringUtils.length(pwd) < 6 || StringUtils.length(pwd) > 255) // NOSONAR
        {
            errors.rejectValue("pwd", "register.pwd.invalid");
        }
    }

    protected void validateEmail(final Errors errors, final String email)
    {
        if (StringUtils.isEmpty(email))
        {
            errors.rejectValue("email", "register.email.invalid");
        }
        else if (StringUtils.length(email) > 255 || !validateEmailAddress(email)) // NOSONAR
        {
            errors.rejectValue("email", "register.email.invalid");
        }
    }

    public boolean validateEmailAddress(final String email)
    {
        final Matcher matcher = Pattern.compile(configurationService.getConfiguration().getString(WebConstants.EMAIL_REGEX))
                .matcher(email);
        return matcher.matches();
    }


}
