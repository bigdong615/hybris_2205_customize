/**
 *
 */
package com.bl.storefront.validator;

import de.hybris.platform.acceleratorstorefrontcommons.forms.UpdatePasswordForm;

import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;


/** 
 * @author Aditi
 * Created validator for BL-2094 : Password Policy - InfoSec updates 
 * To Update validation on reset password.
 */

@Component("blPasswordValidator")
public class BlPasswordValidator implements Validator
{
	private static final String UPDATE_PWD_INVALID = "updatePwd.pwd.invalid";

	@Override
	public boolean supports(final Class<?> aClass)
	{
		return UpdatePasswordForm.class.equals(aClass);
	}

	/**
	 * This method is used to validate password during change password
	 */
	@Override
	public void validate(final Object object, final Errors errors)
	{
		final UpdatePasswordForm passwordForm = (UpdatePasswordForm) object;
		final String currPasswd = passwordForm.getCurrentPassword();
		final String newPasswd = passwordForm.getNewPassword();
		final String checkPasswd = passwordForm.getCheckNewPassword();

		if (StringUtils.isEmpty(currPasswd))
		{
			errors.rejectValue("currentPassword", "profile.currentPassword.invalid");
		}

		if (StringUtils.isEmpty(newPasswd))
		{
			errors.rejectValue("newPassword", UPDATE_PWD_INVALID);
		}
		else if (StringUtils.length(newPasswd) < 8)
		{
			errors.rejectValue("newPassword", UPDATE_PWD_INVALID);
		}

		if (StringUtils.isEmpty(checkPasswd))
		{
			errors.rejectValue("checkNewPassword", UPDATE_PWD_INVALID);
		}
		else if (StringUtils.length(checkPasswd) < 8)
		{
			errors.rejectValue("checkNewPassword", UPDATE_PWD_INVALID);
		}
	}

}

