/**
 *
 */
package com.braintree.security;

import com.braintree.constants.BraintreeaddonConstants;
import org.apache.commons.codec.binary.Hex;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Required;
import org.springframework.util.Assert;
import org.springframework.web.util.CookieGenerator;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;


public class PayPalGUIDCookieStrategy
{
	private static final Logger LOG = Logger.getLogger(PayPalGUIDCookieStrategy.class);

	private final SecureRandom random;
	private final MessageDigest sha;

	private CookieGenerator cookieGenerator;

	public PayPalGUIDCookieStrategy() throws NoSuchAlgorithmException
	{
		random = SecureRandom.getInstance("SHA1PRNG");
		sha = MessageDigest.getInstance("SHA-1");
		Assert.notNull(random);
		Assert.notNull(sha);
	}

	public void setCookie(final HttpServletRequest request, final HttpServletResponse response)
	{
		final String guid = createGUID();

		getCookieGenerator().addCookie(response, guid);
		request.getSession().setAttribute(BraintreeaddonConstants.SECURE_GUID_SESSION_KEY, guid);

		if (LOG.isInfoEnabled())
		{
			LOG.info("Setting guid cookie and session attribute: " + guid);
		}
	}

	protected String createGUID()
	{
		final String randomNum = String.valueOf(getRandom().nextInt());
		final byte[] result = getSha().digest(randomNum.getBytes());
		return String.valueOf(Hex.encodeHex(result));
	}

	protected CookieGenerator getCookieGenerator()
	{
		return cookieGenerator;
	}

	@Required
	public void setCookieGenerator(final CookieGenerator cookieGenerator)
	{
		this.cookieGenerator = cookieGenerator;
	}


	protected SecureRandom getRandom()
	{
		return random;
	}

	protected MessageDigest getSha()
	{
		return sha;
	}
}
