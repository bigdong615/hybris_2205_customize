package com.bl.integration.populator;


import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commercefacades.user.data.CountryData;
import de.hybris.platform.commercefacades.user.data.RegionData;
import de.hybris.platform.core.model.c2l.CountryModel;
import de.hybris.platform.core.model.c2l.RegionModel;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;

import java.util.ArrayList;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;

import com.bl.core.enums.AddressTypeEnum;
import com.bl.facades.ups.address.data.AVSResposeData;
import com.bl.integration.populators.BlUPSAddressResponsePopulator;
import com.bl.integration.response.jaxb.Error;
import com.bl.integration.shipping.response.avsresponse.AddressClassificationType;
import com.bl.integration.shipping.response.avsresponse.AddressKeyFormatType;
import com.bl.integration.shipping.response.avsresponse.AddressValidationResponse;
import com.bl.integration.shipping.response.avsresponse.Response;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class BlUPSAddressResponsePopulatorTest {

  @InjectMocks
  private final BlUPSAddressResponsePopulator populator = Mockito
      .spy(BlUPSAddressResponsePopulator.class);

  private static String STATUS_CODE = "200";
  private static String STATUS_MESSAGE = "200";
  private static String ADDRESS_TYPE_CODE = "2";
  private static String ADDRESS_LINE = "2700 MISSION COLLEGE BLVD";
  private static String POLITICIAL_DIVISION2 = "MENIFEE";
  private static String COUNTRY_CODE = "US";
  private static String COUNTRY_NAME = "United States";
  private static String REGION_CODE = "CA";
  private static String REGION_NAME = "California";
  private static String POSTCODE_PRIMARY_LOW = "95054";
  private static String POSTCODE_EXTENDED_LOW = "1218";
  private static String ERROR_CODE = "350104";
  private static String ERROR_DESCRIPTION = "The country code is missing or invalid.";

  @Mock
  private AddressValidationResponse xavRespon;
  @Mock
  private CommonI18NService commonI18NService;
  @Mock
  private Response response;
  @Mock
  private List<AddressKeyFormatType> addressKeyformatList;
  @Mock
  private CountryModel country;
  @Mock
  private RegionModel regionModel;
  @Mock
  private Error error;
  private AddressClassificationType addressClassificationType;
  private AVSResposeData avsResposeData;
  private AddressKeyFormatType addressKeyFormatType;
  private List<Error> errorList;

  @Before
  public void prepare() {
	  // MockitoAnnotations.initMocks(this);
    errorList = new ArrayList<>();
    when(xavRespon.getResponse()).thenReturn(response);
    avsResposeData = new AVSResposeData();
  }

  @Test
  public void shouldPopulateAddressKeyFormat() {
    addressClassificationType = new AddressClassificationType();
    addressClassificationType.setCode(ADDRESS_TYPE_CODE);
    addressKeyFormatType = new AddressKeyFormatType();
    populateAddressKeyFormatData(addressKeyFormatType);
    addressKeyformatList = new ArrayList<>();
    addressKeyformatList.add(addressKeyFormatType);
    mockingData();
    populator.populateAddressKeyFormatData(xavRespon, avsResposeData);
    avsResposeData.getResult();
    Assert.assertEquals(avsResposeData.getStatusCode(), STATUS_CODE);
    Assert.assertEquals(avsResposeData.getStatusMessage(), STATUS_MESSAGE);
    verifyAddressData();
  }

  @Test
  public void ShouldHandleError(){
    errorList.add(error);
    when(error.getErrorCode()).thenReturn(ERROR_CODE);
    when(error.getErrorDescription()).thenReturn(ERROR_DESCRIPTION);
    when(response.getError()).thenReturn(errorList);
    when(response.getResponseStatusDescription()).thenReturn(STATUS_MESSAGE);
    when(xavRespon.getResponse()).thenReturn(response);
    populator.populateAddressKeyFormatData(xavRespon, avsResposeData);
    assertNotNull(avsResposeData.getResult());
    assertEquals(avsResposeData.getStatusCode(),ERROR_CODE);
    assertEquals(avsResposeData.getStatusMessage(),STATUS_MESSAGE);
    assertEquals(avsResposeData.getErrorDescription(),ERROR_DESCRIPTION);
  }

  private void populateAddressKeyFormatData(final AddressKeyFormatType addressKeyFormatType){
    addressKeyFormatType.getAddressLine().add(ADDRESS_LINE);
    addressKeyFormatType.setCountryCode(COUNTRY_CODE);
    addressKeyFormatType.setPoliticalDivision1(REGION_CODE);
    addressKeyFormatType.setPoliticalDivision2(POLITICIAL_DIVISION2);
    addressKeyFormatType.setPostcodePrimaryLow(POSTCODE_PRIMARY_LOW);
    addressKeyFormatType.setPostcodeExtendedLow(POSTCODE_EXTENDED_LOW);
    addressKeyFormatType.setAddressClassification(addressClassificationType);
  }

  private void mockingData(){
    when(xavRespon.getAddressKeyFormat()).thenReturn(addressKeyformatList);
    when(xavRespon.getResponse().getResponseStatusCode()).thenReturn(STATUS_CODE);
    when(xavRespon.getResponse().getResponseStatusDescription()).thenReturn(STATUS_MESSAGE);
    when(country.getIsocode()).thenReturn(COUNTRY_CODE);
    when(country.getName()).thenReturn(COUNTRY_NAME);
    when(commonI18NService.getCountry(COUNTRY_CODE)).thenReturn(country);
    when(regionModel.getCountry()).thenReturn(country);
    when(regionModel.getName()).thenReturn(REGION_NAME);
    when(regionModel.getIsocodeShort()).thenReturn(REGION_CODE);
    when(regionModel.getIsocode()).thenReturn(COUNTRY_CODE + "-" + REGION_CODE);
    when(commonI18NService.getRegion(country, COUNTRY_CODE + "-" + REGION_CODE))
        .thenReturn(regionModel);
  }

  private void verifyAddressData(){
    avsResposeData.getResult().forEach(addressData -> {
      assertNotNull(addressData);
      assertEquals(addressData.getAddressType(), AddressTypeEnum.RESIDENTIAL.getCode());
      assertEquals(addressData.getTown(), POLITICIAL_DIVISION2);
      assertEquals(addressData.getLine1(), ADDRESS_LINE);
      assertEquals(addressData.getPostalCode(), POSTCODE_PRIMARY_LOW + "-" + POSTCODE_EXTENDED_LOW);
      final CountryData countryData = addressData.getCountry();
      assertEquals(countryData.getName(), COUNTRY_NAME);
      assertEquals(countryData.getIsocode(), COUNTRY_CODE);
      final RegionData regionData = addressData.getRegion();
      assertEquals(regionData.getName(), REGION_NAME);
      assertEquals(regionData.getIsocodeShort(), REGION_CODE);
      assertEquals(regionData.getCountryIso(), COUNTRY_CODE);
    });
  }
}
