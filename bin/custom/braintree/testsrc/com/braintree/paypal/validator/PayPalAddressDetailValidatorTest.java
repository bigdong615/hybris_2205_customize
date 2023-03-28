/**
 *
 */
package com.braintree.paypal.validator;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.model.c2l.CountryModel;
import de.hybris.platform.core.model.c2l.RegionModel;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;


@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class PayPalAddressDetailValidatorTest
{
	private static final String EMPTY_CODE = "";
	private static final String COUNTRY_CODE = "COUNTRY_CODE";
	private static final String WRONG_COUNTRY_CODE = "WRONG_COUNTRY_CODE";
	private static final String REGION_CODE = "COUNTRY_CODE";
	private static final String WRONG_REGION_CODE = "WRONG_COUNTRY_CODE";


	@Mock
	private CommonI18NService commonI18NService;

	@InjectMocks
	private final PayPalAddressDetailValidator payPalAddressDetailValidator = new PayPalAddressDetailValidator();

	@Before
	public void init()
	{
		//MockitoAnnotations.initMocks(this);
	}

	@Test
	public void shouldValidatePayPalCountryCode()
	{
		// given
		final CountryModel countryModel = mock(CountryModel.class);
		final List<CountryModel> countries = Arrays.asList(countryModel);
		final List<CountryModel> unmodifiableCountries = Collections.unmodifiableList(countries);
		when(commonI18NService.getAllCountries()).thenReturn(unmodifiableCountries);
		when(countryModel.getIsocode()).thenReturn(COUNTRY_CODE);

		// when
		final boolean isValidPayPalCountryCode = payPalAddressDetailValidator.validatePayPalCountryCode(COUNTRY_CODE);

		// then
		assertTrue(isValidPayPalCountryCode);
	}

	@Test
	public void shouldNotValidatePayPalCountryCodeIfNull()
	{
		// when
		final boolean isValidPayPalCountryCode = payPalAddressDetailValidator.validatePayPalCountryCode(EMPTY_CODE);

		// then
		assertFalse(isValidPayPalCountryCode);
	}

	@Test
	public void shouldNotValidatePayPalCountryCodeIfEmptyList()
	{
		// given
		final List<CountryModel> countries = new ArrayList<>();
		when(commonI18NService.getAllCountries()).thenReturn(countries);

		// when
		final boolean isValidPayPalCountryCode = payPalAddressDetailValidator.validatePayPalCountryCode(COUNTRY_CODE);

		// then
		assertFalse(isValidPayPalCountryCode);
	}

	@Test
	public void shouldNotValidatePayPalCountryCodeIfIsoCodeNotFind()
	{
		// given
		final CountryModel countryModel = mock(CountryModel.class);
		final List<CountryModel> countries = Arrays.asList(countryModel);
		final List<CountryModel> unmodifiableCountries = Collections.unmodifiableList(countries);
		when(commonI18NService.getAllCountries()).thenReturn(unmodifiableCountries);
		when(countryModel.getIsocode()).thenReturn(WRONG_COUNTRY_CODE);

		// when
		final boolean isValidPayPalCountryCode = payPalAddressDetailValidator.validatePayPalCountryCode(COUNTRY_CODE);

		// then
		assertFalse(isValidPayPalCountryCode);
	}

	@Test
	public void shouldValidatePayPalRegionCode()
	{
		// given
		final RegionModel regionModel = mock(RegionModel.class);
		final List<RegionModel> countries = Arrays.asList(regionModel);
		final List<RegionModel> unmodifiableCountries = Collections.unmodifiableList(countries);
		when(commonI18NService.getAllRegions()).thenReturn(unmodifiableCountries);
		when(regionModel.getIsocode()).thenReturn(COUNTRY_CODE + "-" + REGION_CODE);

		// when
		final boolean isValidPayPalRegionCode = payPalAddressDetailValidator.validatePayPalRegionCode(COUNTRY_CODE, REGION_CODE);

		// then
		assertTrue(isValidPayPalRegionCode);
	}

	@Test
	public void shouldNotValidatePayPalRegionCodeIfNull()
	{
		// when
		final boolean isValidPayPalRegionCode = payPalAddressDetailValidator.validatePayPalRegionCode(COUNTRY_CODE, EMPTY_CODE);

		// then
		assertFalse(isValidPayPalRegionCode);
	}

	@Test
	public void shouldNotValidatePayPalRegionCodeIfEmptyList()
	{
		// given
		final List<RegionModel> countries = new ArrayList<>();
		when(commonI18NService.getAllRegions()).thenReturn(countries);

		// when
		final boolean isValidPayPalRegionCode = payPalAddressDetailValidator.validatePayPalRegionCode(COUNTRY_CODE, REGION_CODE);

		// then
		assertFalse(isValidPayPalRegionCode);
	}

	@Test
	public void shouldNotValidatePayPalRegionCodeIfIsoCodeNotFind()
	{
		// given
		final RegionModel regionModel = mock(RegionModel.class);
		final List<RegionModel> countries = Arrays.asList(regionModel);
		final List<RegionModel> unmodifiableCountries = Collections.unmodifiableList(countries);
		when(commonI18NService.getAllRegions()).thenReturn(unmodifiableCountries);
		when(regionModel.getIsocode()).thenReturn(WRONG_REGION_CODE);

		// when
		final boolean isValidPayPalRegionCode = payPalAddressDetailValidator.validatePayPalRegionCode(COUNTRY_CODE, REGION_CODE);

		// then
		assertFalse(isValidPayPalRegionCode);
	}

}
