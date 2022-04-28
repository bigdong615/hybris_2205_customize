/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.storefront.validator;

import de.hybris.platform.acceleratorstorefrontcommons.forms.UpdatePasswordForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.UpdatePwdForm;
import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.bl.storefront.forms.BlUpdatePwdForm;


@Component("blUpdatePasswordFormValidator")
public class BlUpdatePasswordFormValidator implements Validator
{
	@Override
	public boolean supports(Class<?> aClass)
	{
		return BlUpdatePwdForm.class.equals(aClass);
	}

	@Override
	public void validate(Object object, Errors errors)
	{
		final BlUpdatePwdForm updatePasswordForm = (BlUpdatePwdForm) object;
        final String newPassword = updatePasswordForm.getPwd();
		final String checkPassword = updatePasswordForm.getCheckPwd();

		if (StringUtils.isNotEmpty(newPassword) && StringUtils.isNotEmpty(checkPassword)
				&& !StringUtils.equals(newPassword,checkPassword))
		{
			errors.rejectValue("checkPwd", "validation.checkPwd.equals");
		}
		else if(StringUtils.isEmpty(checkPassword))
		{
			errors.rejectValue("checkPwd", "updatePwd.checkPwd.invalid");
		}
	}
}
