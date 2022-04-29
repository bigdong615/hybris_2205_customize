/**
 *
 */
package com.bl.storefront.validator;

import de.hybris.platform.acceleratorstorefrontcommons.forms.UpdatePasswordForm;

import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import com.bl.storefront.controllers.pages.BlControllerConstants;


/**
 * @author Aditi Created validator for BL-2094 : Password Policy - InfoSec updates To Update validation on reset
 *         password.
 */

@Component("blPasswordValidator")
public class BlPasswordValidator implements Validator
{

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

		validatePassword(errors, currPasswd, newPasswd, checkPasswd);
		comparePassword(errors, newPasswd, checkPasswd);
	}

	/**
	 * This method is used to compare password
	 * @param errors
	 * @param newPasswd
	 * @param checkPasswd
	 */
	private void comparePassword(final Errors errors, final String newPasswd, final String checkPasswd)
	{
		if (StringUtils.isNotEmpty(newPasswd) && StringUtils.isNotEmpty(checkPasswd) && !StringUtils.equals(newPasswd, checkPasswd))
		{
			errors.rejectValue(BlControllerConstants.CHECK_NEW_PASSWORD, BlControllerConstants.VALIDATE_CHECKPASSWORD_EQUALS);
		}
		else
      {
          if (StringUtils.isEmpty(checkPasswd))
          {
              errors.rejectValue(BlControllerConstants.CHECK_NEW_PASSWORD, BlControllerConstants.REGISTER_CHECKPASSWORD_INVALID);
          }
      }
	}

	/**
	 * This method is used to validate password
	 * @param errors
	 * @param currPasswd
	 * @param newPasswd
	 * @param checkPasswd
	 */
	private void validatePassword(final Errors errors, final String currPasswd, final String newPasswd, final String checkPasswd)
	{
		if (StringUtils.isEmpty(currPasswd))
		{
			errors.rejectValue(BlControllerConstants.CURRENT_PASSWORD, "profile.currentPassword.invalid");
		}

		if (StringUtils.isEmpty(newPasswd) || (StringUtils.isNotEmpty(newPasswd) && StringUtils.length(newPasswd) < 8))
		{
			errors.rejectValue(BlControllerConstants.NEW_PASSWORD, BlControllerConstants.REGISTER_PASSWORD_INVALID);
		}
	}

}

