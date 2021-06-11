package com.braintree.configuration.service;

import org.apache.commons.lang.StringUtils;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import static com.braintree.constants.BrainTreeSupportedLocationConstants.CN_COUNTRY;
import static com.braintree.constants.BrainTreeSupportedLocationConstants.DE_COUNTRY;
import static com.braintree.constants.BrainTreeSupportedLocationConstants.DE_LANGUAGE;
import static com.braintree.constants.BrainTreeSupportedLocationConstants.EN_LANGUAGE;
import static com.braintree.constants.BrainTreeSupportedLocationConstants.JA_LANGUAGE;
import static com.braintree.constants.BrainTreeSupportedLocationConstants.JP_COUNTRY;
import static com.braintree.constants.BrainTreeSupportedLocationConstants.US_COUNTRY;
import static com.braintree.constants.BrainTreeSupportedLocationConstants.ZH_LANGUAGE;


public class BrainTreeSupportedLocaleConfig
{

	private static final String UNDERSCORE = "_";
	private static Map<String, Locale> brainTreeSupportedLocaleMap;

	static
	{
		brainTreeSupportedLocaleMap = new HashMap<String, Locale>();
		brainTreeSupportedLocaleMap.put(EN_LANGUAGE, new Locale(EN_LANGUAGE, US_COUNTRY));
		brainTreeSupportedLocaleMap.put(JA_LANGUAGE, new Locale(JA_LANGUAGE, JP_COUNTRY));
		brainTreeSupportedLocaleMap.put(DE_LANGUAGE, new Locale(DE_LANGUAGE, DE_COUNTRY));
		brainTreeSupportedLocaleMap.put(ZH_LANGUAGE, new Locale(ZH_LANGUAGE, CN_COUNTRY));
	}

	public static Locale getSupportedLocaleByLanguage(final String language)
	{
		return brainTreeSupportedLocaleMap.get(language);
	}

	public static boolean supportLocale(final String locale)
	{
		final String languageKey = StringUtils.substringBefore(locale, UNDERSCORE);
		final String countryValue = StringUtils.substringAfter(locale, UNDERSCORE);
		return brainTreeSupportedLocaleMap.get(languageKey).toString().equals(languageKey + UNDERSCORE + countryValue);
	}

	public static Locale getDefaultLocale(final String currentLocale)
	{
		return brainTreeSupportedLocaleMap.get(currentLocale);
	}
}
