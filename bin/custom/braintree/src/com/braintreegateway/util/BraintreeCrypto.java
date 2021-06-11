package com.braintreegateway.util;

import java.security.Key;
import java.util.Base64;

import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;

import org.apache.log4j.Logger;


public class BraintreeCrypto
{

	private static final Logger LOG = Logger.getLogger(BraintreeCrypto.class);
	private static final String ALGORITHM_AES = "AES";
	public static final String DEFAULT_VALUE = "Hybris_BT";

	public static String encrypt(final String key, final String data)
	{
		Key generatedKey = new SecretKeySpec(key.getBytes(), ALGORITHM_AES);

		try
		{
			Cipher cipher = Cipher.getInstance(ALGORITHM_AES);
			cipher.init(Cipher.ENCRYPT_MODE, generatedKey);
			byte[] encryptedValue = cipher.doFinal(data.getBytes());
			return Base64.getEncoder().encodeToString(encryptedValue);
		}
		catch (final Exception e)
		{
			LOG.error("Encryption error, message: " + e.getMessage(), e);
			return DEFAULT_VALUE;
		}
	}

	public static String decrypt(final String key, final String encryptedData)
	{
		Key generatedKey = null;
		byte[] decValue = {};
		try
		{
			generatedKey = new SecretKeySpec(key.getBytes(), ALGORITHM_AES);
			Cipher c = Cipher.getInstance(ALGORITHM_AES);
			c.init(Cipher.DECRYPT_MODE, generatedKey);
			byte[] decodedValue = Base64.getDecoder().decode(encryptedData);
			decValue = c.doFinal(decodedValue);
		}
		catch (final Exception e)
		{
			LOG.error("Decryption error, message: " + e.getMessage(), e);
			return DEFAULT_VALUE;
		}
		return new String(decValue);
	}
}
