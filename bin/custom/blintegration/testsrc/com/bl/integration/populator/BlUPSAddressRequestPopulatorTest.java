package com.bl.integration.populator;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.when;

import com.bl.integration.populators.BlUPSAddressRequestPopulator;
import com.bl.integration.shipping.request.avsrequest.AddressKeyFormatType;
import com.bl.integration.shipping.request.avsrequest.AddressValidationRequest;
import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.commercefacades.user.data.CountryData;
import de.hybris.platform.commercefacades.user.data.RegionData;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.BDDMockito;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

@UnitTest
public class BlUPSAddressRequestPopulatorTest {

  @InjectMocks
  private final BlUPSAddressRequestPopulator populator = Mockito.spy(BlUPSAddressRequestPopulator.class);

  private static String FNAME = "testname";
  private static String LNAME = "vishwararma";
  private static String COUNTRY_ISO = "US";
  private static String REGION_ISO_SHORT = "CA";
  private static String ADDRESS_LINE1 = "47 , 10 flor";
  private static String ADDRESS_LINE2 = "bl bilding";
  private static String TOWN = "california";
  private static String POSTAL_CODE = "95054";
  @Mock
  private AddressData addressData;
  private AddressValidationRequest avrequest;
  private CountryData countryData;
  private RegionData regionData;

  @Before
  public void prepare() {
    MockitoAnnotations.initMocks(this);
    avrequest = new AddressValidationRequest();
    countryData = new CountryData();
    countryData.setIsocode(COUNTRY_ISO);
    regionData = new RegionData();
    regionData.setIsocodeShort(REGION_ISO_SHORT);
  }

  @Test
  public void shouldPopulateAddressRequest(){
    when(addressData.getFirstName()).thenReturn(FNAME);
    when(addressData.getLastName()).thenReturn(LNAME);
    when(addressData.getCountry()).thenReturn(countryData);
    when(addressData.getRegion()).thenReturn(regionData);
    when(addressData.getLine1()).thenReturn(ADDRESS_LINE1);
    when(addressData.getLine2()).thenReturn(ADDRESS_LINE2);
    when(addressData.getTown()).thenReturn(TOWN);
    when(addressData.getPostalCode()).thenReturn(POSTAL_CODE);
    populator.populateAddressRequest(addressData,avrequest);
    assertNotNull(avrequest.getRequest());
    AddressKeyFormatType addressKeyFormatType=avrequest.getAddressKeyFormat().get(0);
    assertNotNull(addressKeyFormatType);
    assertEquals(addressKeyFormatType.getConsigneeName(),FNAME+" "+LNAME);
    assertEquals(addressKeyFormatType.getAddressLine().get(0),ADDRESS_LINE1);
    assertEquals(addressKeyFormatType.getAddressLine().get(1),ADDRESS_LINE2);
    assertEquals(addressKeyFormatType.getPoliticalDivision2(),TOWN);
    assertEquals(addressKeyFormatType.getPoliticalDivision1(),REGION_ISO_SHORT);
    assertEquals(addressKeyFormatType.getPostcodePrimaryLow(), POSTAL_CODE);
    assertEquals(addressKeyFormatType.getCountryCode(),COUNTRY_ISO);
  }

}
