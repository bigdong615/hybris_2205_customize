package com.bl.integration.populators;

import com.bl.facades.locator.data.UpsLocatorResposeData;
import com.bl.facades.locator.data.UpsStoreData;
import com.bl.integration.response.jaxb.AddressKeyFormatType;
import com.bl.integration.response.jaxb.DropLocationType;
import com.bl.integration.response.jaxb.Error;
import com.bl.integration.response.jaxb.LocatorResponse;
import com.bl.integration.response.jaxb.SearchResultsType;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;

/**
 *This class was created to populate UPS Locator response.
 * @author vijay vishwakarma
 */
public class BlLocatorResponsePopulator {


  public void populateDropDownLocation(UpsLocatorResposeData upsLocatorResposeData,LocatorResponse locatorResponse) {
    List<UpsStoreData> locatorResponseDTOList = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(locatorResponse.getResponse().getError())) {
      populateErrorData(upsLocatorResposeData,locatorResponse.getResponse().getError().get(0),locatorResponse.getResponse().getResponseStatusDescription());
    } else {
      SearchResultsType searchResult = locatorResponse.getSearchResults();
      List<Object> locationList = searchResult.getDisclaimerAndDropLocation();
      locationList.forEach(location -> {
        if (location instanceof DropLocationType) {
          DropLocationType dropLocation = (DropLocationType) location;
          UpsStoreData locatorResponseData = new UpsStoreData();
          locatorResponseData.setLocationId(dropLocation.getLocationID());
          locatorResponseData.setLatestGroundDropOffTime(dropLocation.getLatestGroundDropOffTime());
          AddressKeyFormatType addressKeyFormat = dropLocation.getAddressKeyFormat();
          if (addressKeyFormat != null) {
            populateAddressData(locatorResponseData,addressKeyFormat);
          }
          locatorResponseDTOList.add(locatorResponseData);
        }
      });
      upsLocatorResposeData.setResult(locatorResponseDTOList);
      upsLocatorResposeData
          .setStatusCode(locatorResponse.getResponse().getResponseStatusCode());
      upsLocatorResposeData
          .setStatusMessage(locatorResponse.getResponse().getResponseStatusDescription());
    }
  }

/**
 * This method used for populating error data.
 */
  private void populateErrorData(UpsLocatorResposeData upsLocatorResposeData,Error error , String statusMessage){
    upsLocatorResposeData.setStatusCode(error.getErrorCode());
    upsLocatorResposeData.setStatusMessage(statusMessage);
    upsLocatorResposeData.setErrorDescription(error.getErrorDescription());
    upsLocatorResposeData.setResult(Collections.emptyList());
  }

  /**
   * This method used for populating address data.
   */
  private void populateAddressData( UpsStoreData locatorResponseData,AddressKeyFormatType addressKeyFormat){
    locatorResponseData.setConsigneeName(addressKeyFormat.getConsigneeName());
    locatorResponseData.setAddressLine(addressKeyFormat.getAddressLine());
    locatorResponseData.setCountryCode(addressKeyFormat.getCountryCode());
    locatorResponseData.setPoliticalDivision1(addressKeyFormat.getPoliticalDivision1());
    locatorResponseData.setPoliticalDivision2(addressKeyFormat.getPoliticalDivision2());
    locatorResponseData.setPostcodePrimaryLow(addressKeyFormat.getPostcodePrimaryLow());
    locatorResponseData.setPostcodeExtendedLow(addressKeyFormat.getPostcodeExtendedLow());
  }
}
