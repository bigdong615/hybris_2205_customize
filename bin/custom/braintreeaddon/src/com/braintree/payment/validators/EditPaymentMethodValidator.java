package com.braintree.payment.validators;


import com.braintree.exceptions.ResourceErrorMessage;
import org.apache.commons.lang.StringUtils;

import java.util.Calendar;


public class EditPaymentMethodValidator implements PaymentMethodValidator
{

	public static final int BRAINTREE_CARDHOLDER_MAX_LENGTHS = 175;
	public static final int YEAR_LENGTH = 4;
	public static final int CVV_LENGTH_ALL = 3;
	public static final int CVV_LENGTH_AMERICAN_EXPRESS = 4;

	@Override
	public ResourceErrorMessage validate(final String expirationDate, final String cvv)
	{
		/*if (StringUtils.isNotBlank(cardholder) && cardholder.length() > BRAINTREE_CARDHOLDER_MAX_LENGTHS)
		{
			return new ResourceErrorMessage("text.account.profile.paymentCart.editPaymentMethod.invalid.cardholderTooLong");
		}*/

		if (StringUtils.isNotBlank(cvv))
		{
			if (cvv.length() < CVV_LENGTH_ALL || cvv.length() > CVV_LENGTH_AMERICAN_EXPRESS)
			{
				return new ResourceErrorMessage("text.account.profile.paymentCart.editPaymentMethod.invalid.cvv");
			}
		}
		return validateExpirationDate(expirationDate);
	}

	private ResourceErrorMessage validateExpirationDate(String expirationDate)
	{
		if (StringUtils.isNotBlank(expirationDate))
		{
			String[] split = expirationDate.split("/");
			if (split.length == 2)
			{
				String startMonth = split[0];
				String startYear = split[1];
				return checkExpirationDate(startMonth, startYear);
			}
			else
			{
				return new ResourceErrorMessage("text.account.profile.paymentCart.editPaymentMethod.invalid.expirationDate");
			}
		}
		else
		{
			return new ResourceErrorMessage("text.account.profile.paymentCart.editPaymentMethod.required.expirationDate");
		}
	}

	private ResourceErrorMessage checkExpirationDate(String startMonth, String startYear)
	{
		try
		{
			final Calendar entered = Calendar.getInstance();
			entered.set(Calendar.DAY_OF_MONTH, 0);
			entered.set(Calendar.MONTH, Integer.parseInt(startMonth) - 1);
			entered.set(Calendar.YEAR, Integer.parseInt(startYear));

			final Calendar current = Calendar.getInstance();
			if (startYear.length() == YEAR_LENGTH)
			{
				if (current.after(entered))
				{
					return new ResourceErrorMessage("text.account.profile.paymentCart.editPaymentMethod.error.expirationDate");
				}
			}
			else
			{
				return new ResourceErrorMessage("text.account.profile.paymentCart.editPaymentMethod.invalid.expirationDate");
			}
		}
		catch (NumberFormatException e)
		{
			return new ResourceErrorMessage("text.account.profile.paymentCart.editPaymentMethod.invalid.expirationDate");
		}
		return new ResourceErrorMessage(StringUtils.EMPTY);
	}
}
