package com.bl.integration.populator;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.BDDMockito;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;

import com.bl.facades.locator.data.UPSLocatorRequestData;
import com.bl.integration.populators.BlLocatorRequestPopulator;
import com.bl.integration.request.jaxb.LocatorRequest;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class BlLocatorRequestPopulatorTest {

  @InjectMocks
  private final BlLocatorRequestPopulator populator = Mockito.spy(new BlLocatorRequestPopulator());

  public static String ZIPCODE = "95054";
  public static String COUNTRY_CODE = "US";
  LocatorRequest locatorRequest;

  @Mock
  UPSLocatorRequestData locatorFormDTO;
  @Mock
  private CommonI18NService commonI18NService;

  @Before
  public void prepare() {
	  // MockitoAnnotations.initMocks(this);
    locatorRequest = new LocatorRequest();
    BDDMockito.given(locatorFormDTO.getZipcode()).willReturn(ZIPCODE);
    BDDMockito.given(locatorFormDTO.getCountryCode()).willReturn(COUNTRY_CODE);
  }

  @Test
  public void shouldPopulateLocatorRequest() {
    populator.populateLocatorRequest(locatorRequest, locatorFormDTO);
    Assert.assertEquals(
        locatorRequest.getOriginAddress().getAddressKeyFormat().getPostcodePrimaryLow(), ZIPCODE);
  }
}
