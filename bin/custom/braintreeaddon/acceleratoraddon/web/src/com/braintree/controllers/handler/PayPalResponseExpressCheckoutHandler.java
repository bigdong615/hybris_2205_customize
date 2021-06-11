/**
 *
 */
package com.braintree.controllers.handler;

import com.braintree.hybris.data.PayPalAddressData;
import com.braintree.hybris.data.PayPalDetails;
import com.braintree.hybris.data.PayPalExpressResponse;
import com.braintree.paypal.converters.impl.PayPalAddressDataConverter;
import com.fasterxml.jackson.databind.ObjectMapper;
import de.hybris.platform.commercefacades.user.data.AddressData;
import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;

import static com.braintree.constants.BraintreeaddonWebConstants.PAY_PAL_RESPONSE;
import static com.fasterxml.jackson.databind.DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES;
import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;
import static org.apache.commons.lang.StringUtils.isNotEmpty;


@Component
public class PayPalResponseExpressCheckoutHandler
{

	@Resource(name = "payPalAddressDataConverter")
	private PayPalAddressDataConverter payPalAddressDataConverter;

	public PayPalExpressResponse handlePayPalResponse(final HttpServletRequest request) throws IllegalArgumentException
	{
		validateParameterNotNull(request, "request cannot be null");

		final String[] jsonResponseArray = request.getParameterMap().get(PAY_PAL_RESPONSE);

		final String responseJson = jsonResponseArray[0];
		PayPalExpressResponse response = null;

		if (isNotEmpty(responseJson))
		{
			final ObjectMapper mapper = new ObjectMapper().disable(FAIL_ON_UNKNOWN_PROPERTIES);

			try
			{
				response = mapper.readValue(responseJson, PayPalExpressResponse.class);

			}
			catch (final Exception e)
			{
				throw new IllegalArgumentException(
						"Pay Pal express checkout response is invalid!. Please try again later or contact with payment provider support."+"\n"+ "Response: " + responseJson);
			}
		}
		else
		{
			throw new IllegalArgumentException(
					"Pay Pal express checkout response is empty! Please try again later or contact with payment provider support.");
		}
		return response;
	}

	public AddressData getPayPalAddress(PayPalDetails details, final PayPalAddressData address)
	{
		final AddressData addressData = payPalAddressDataConverter.convert(address);
		if (StringUtils.isBlank(address.getRecipientName()))
		{
			addressData.setFirstName(details.getFirstName());
			addressData.setLastName(details.getLastName());
		}
		return addressData;
	}

}
