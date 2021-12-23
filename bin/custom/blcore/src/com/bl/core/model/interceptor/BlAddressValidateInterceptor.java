package com.bl.core.model.interceptor;

import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.ValidateInterceptor;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.logging.BlLogger;


/**
 * @author Ravikumar
 *
 *         Validator Interceptor for AddressModel to verify the values set on attribute before saving the model
 *
 */
public class BlAddressValidateInterceptor implements ValidateInterceptor<AddressModel>
{

	private static final Logger LOG = Logger.getLogger(BlAddressValidateInterceptor.class);

	@Override
	public void onValidate(final AddressModel addressModel, final InterceptorContext interceptorContext)
			throws InterceptorException
	{
		validateNameLength(addressModel);
	}

	/**
	 * Validate name length on address.
	 *
	 *
	 * @param addressModel
	 *           the address model
	 * @throws InterceptorException
	 *            the interceptor exception
	 */
	private void validateNameLength(final AddressModel addressModel) throws InterceptorException
	{
		final String firstName = ObjectUtils.defaultIfNull(addressModel.getFirstname(), StringUtils.EMPTY);
		final String lastName = ObjectUtils.defaultIfNull(addressModel.getLastname(), StringUtils.EMPTY);
		final int nameLength = firstName.length() + lastName.length();
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Length of first name and last name is : {}", nameLength);
		/*if (nameLength > BlCoreConstants.NAME_LENGTH_SIZE)
		{
			throw new InterceptorException("Combined length of First Name and Last Name should not be more than 48 characters");
		}*/
	}

}
