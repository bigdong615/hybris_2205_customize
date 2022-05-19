/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.storefront.validator;

import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.bl.storefront.controllers.pages.BlControllerConstants;
import com.bl.storefront.forms.BlUpdatePwdForm;


/**
 * @author Aditi Created validator for BL-2094 : Password Policy - InfoSec updates To Update validation on reset
 *         password.
 */

@Component("blUpdatePasswordFormValidator")
public class BlUpdatePasswordFormValidator implements Validator
{
	@Override
	public boolean supports(final Class<?> aClass)
	{
		return BlUpdatePwdForm.class.equals(aClass);
	}

	/**
	 * This method is used to validate password during reset password
	 */
	@Override
	public void validate(final Object object, final Errors errors)
	{
		final BlUpdatePwdForm updatePasswordForm = (BlUpdatePwdForm) object;
		final String newPassword = updatePasswordForm.getPwd();
		final String checkPassword = updatePasswordForm.getCheckPwd();

		if (StringUtils.isNotEmpty(newPassword) && StringUtils.isNotEmpty(checkPassword)
				&& !StringUtils.equals(newPassword, checkPassword))
		{
			errors.rejectValue(BlControllerConstants.CONFIRM_PASSWORD_STRING, BlControllerConstants.VALIDATE_CHECKPASSWORD_EQUALS);
		}
		else if (StringUtils.isEmpty(checkPassword))
		{
			errors.rejectValue(BlControllerConstants.CONFIRM_PASSWORD_STRING, BlControllerConstants.UPDATE_CHECKPASSWORD_INVALID);
		}
	}
}
