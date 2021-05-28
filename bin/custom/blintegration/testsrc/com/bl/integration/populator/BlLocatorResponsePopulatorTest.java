package com.bl.integration.populator;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.when;

import com.bl.facades.locator.data.UpsLocatorResposeData;
import com.bl.integration.populators.BlLocatorResponsePopulator;
import com.bl.integration.response.jaxb.AddressKeyFormatType;
import com.bl.integration.response.jaxb.DropLocationType;
import com.bl.integration.response.jaxb.Error;
import com.bl.integration.response.jaxb.LocatorResponse;
import com.bl.integration.response.jaxb.Response;
import com.bl.integration.response.jaxb.SearchResultsType;
import de.hybris.bootstrap.annotations.UnitTest;
import java.util.ArrayList;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

@UnitTest
public class BlLocatorResponsePopulatorTest {

  @InjectMocks
  private final  BlLocatorResponsePopulator populator= Mockito.spy(BlLocatorResponsePopulator.class);

  private static String LOCATION_ID = "142108";
  private static String RESPONSE_CODE = "200";
  private static String RESPONSE_DESCRIPTION = "Success";
  private static String CONSIGNEE_NAME = "THE UPS STORE";
  private static String ADDRESS_LINE = "2700 MISSION COLLEGE BLVD";
  private static String POLITICAL_DEVISION2 = "SANTA CLARA";
  private static String POLITICAL_DEVISION1 = "CA";
  private static String POSTCODE_PRIMARY_LOW = "95054";
  private static String POSTCODE_EXTENDED_LOW = "1218";
  private static String COUNTRY_CODE = "US";
  private static String ERROR_CODE = "350104";
  private static String ERROR_DESCRIPTION = "The country code is missing or invalid.";

  @Mock
  private LocatorResponse locatorResponse;
  @Mock
  private Response response;
  @Mock
  private SearchResultsType searchResult;
  @Mock
  private DropLocationType dropLocation;
  @Mock
  private AddressKeyFormatType addressKeyFormat;
  @Mock
  private Error error;
  private List<Object> locationList;
  private UpsLocatorResposeData upsLocatorResposeData;
  private List<Error> errorList;

  @Before
  public void prepare() {
    MockitoAnnotations.initMocks(this);
    upsLocatorResposeData = new UpsLocatorResposeData();
    locationList = new ArrayList<>();
    errorList = new ArrayList<>();
  }

  @Test
  public void ShouldPopulateDropDownLocation() {
    locationList.add(dropLocation);
    mockingData();
    populator.populateDropDownLocation(upsLocatorResposeData, locatorResponse);
    assertEquals(upsLocatorResposeData.getStatusCode(), RESPONSE_CODE);
    assertEquals(upsLocatorResposeData.getStatusMessage(), RESPONSE_DESCRIPTION);
    verifyResult();
  }

  @Test
  public void ShouldHandleError(){
    errorList.add(error);
    when(error.getErrorCode()).thenReturn(ERROR_CODE);
    when(error.getErrorDescription()).thenReturn(ERROR_DESCRIPTION);
    when(response.getError()).thenReturn(errorList);
    when(response.getResponseStatusDescription()).thenReturn(RESPONSE_DESCRIPTION);
    when(locatorResponse.getResponse()).thenReturn(response);
    populator.populateDropDownLocation(upsLocatorResposeData,locatorResponse);
    assertNotNull(upsLocatorResposeData.getResult());
    assertEquals(upsLocatorResposeData.getStatusCode(),ERROR_CODE);
    assertEquals(upsLocatorResposeData.getStatusMessage(),RESPONSE_DESCRIPTION);
    assertEquals(upsLocatorResposeData.getErrorDescription(),ERROR_DESCRIPTION);
  }

  private void mockingData(){
    when(locatorResponse.getResponse()).thenReturn(response);
    when(locatorResponse.getSearchResults()).thenReturn(searchResult);
    when(searchResult.getDisclaimerAndDropLocation()).thenReturn(locationList);
    when(dropLocation.getLocationID()).thenReturn(LOCATION_ID);
    when(response.getResponseStatusCode()).thenReturn(RESPONSE_CODE);
    when(response.getResponseStatusDescription()).thenReturn(RESPONSE_DESCRIPTION);
    when(dropLocation.getAddressKeyFormat()).thenReturn(addressKeyFormat);
    when(addressKeyFormat.getConsigneeName()).thenReturn(CONSIGNEE_NAME);
    when(addressKeyFormat.getAddressLine()).thenReturn(ADDRESS_LINE);
    when(addressKeyFormat.getCountryCode()).thenReturn(COUNTRY_CODE);
    when(addressKeyFormat.getPoliticalDivision1()).thenReturn(POLITICAL_DEVISION1);
    when(addressKeyFormat.getPoliticalDivision2()).thenReturn(POLITICAL_DEVISION2);
    when(addressKeyFormat.getPostcodePrimaryLow()).thenReturn(POSTCODE_PRIMARY_LOW);
    when(addressKeyFormat.getPostcodeExtendedLow()).thenReturn(POSTCODE_EXTENDED_LOW);
  }

  private void verifyResult(){
    upsLocatorResposeData.getResult().forEach(upsStoreData -> {
      assertEquals(upsStoreData.getLocationId(), LOCATION_ID);
      assertEquals(upsStoreData.getConsigneeName(), CONSIGNEE_NAME);
      assertEquals(upsStoreData.getAddressLine(), ADDRESS_LINE);
      assertEquals(upsStoreData.getCountryCode(), COUNTRY_CODE);
      assertEquals(upsStoreData.getPoliticalDivision1(), POLITICAL_DEVISION1);
      assertEquals(upsStoreData.getPoliticalDivision2(), POLITICAL_DEVISION2);
      assertEquals(upsStoreData.getPostcodePrimaryLow(), POSTCODE_PRIMARY_LOW);
      assertEquals(upsStoreData.getPostcodeExtendedLow(), POSTCODE_EXTENDED_LOW);
    });
  }
}
