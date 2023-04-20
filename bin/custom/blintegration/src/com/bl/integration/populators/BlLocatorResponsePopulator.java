package com.bl.integration.populators;

import com.bl.facades.locator.data.DistanceData;
import com.bl.facades.locator.data.UpsLocatorResposeData;
import com.bl.facades.locator.data.UpsStoreData;
import com.bl.integration.response.jaxb.AddressKeyFormatType;
import com.bl.integration.response.jaxb.DistanceType;
import com.bl.integration.response.jaxb.DropLocationType;
import com.bl.integration.response.jaxb.Error;
import com.bl.integration.response.jaxb.LocatorResponse;
import com.bl.integration.response.jaxb.SearchResultsType;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Value;

/**
 *This class was created to populate UPS Locator response.
 * @author vijay vishwakarma
 */
public class BlLocatorResponsePopulator {

  @Value("${blintegration.locator.result.count}")
  private String maximumResult;

  public void populateDropDownLocation(UpsLocatorResposeData upsLocatorResposeData,LocatorResponse locatorResponse) {
    List<UpsStoreData> locatorResponseDTOList = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(locatorResponse.getResponse().getError())) {
      populateErrorData(upsLocatorResposeData,locatorResponse.getResponse().getError().get(0),locatorResponse.getResponse().getResponseStatusDescription());
    } else {
      SearchResultsType searchResult = locatorResponse.getSearchResults();
      List<Object> locationList = searchResult.getDisclaimerAndDropLocation();
      if (CollectionUtils.isNotEmpty(locationList)) {
        for (int count = 0; count < locationList.size(); count++) {
          if (count > (Integer.parseInt(maximumResult) - 1)) {
            break;
          }
          Object location = locationList.get(count);
          if (location instanceof DropLocationType) {
            DropLocationType dropLocation = (DropLocationType) location;
            UpsStoreData locatorResponseData = new UpsStoreData();
            locatorResponseData.setLocationId(dropLocation.getLocationID());

            final List<String> openingDaysGroundDropOffTime =new ArrayList<>();
            openingDaysGroundDropOffTime.add(dropLocation.getStandardHoursOfOperation());
            if (CollectionUtils.isNotEmpty(openingDaysGroundDropOffTime)) {
              locatorResponseData
                  .setLatestGroundDropOffTime(getOpeningDaysDetails(openingDaysGroundDropOffTime));
            }

            AddressKeyFormatType addressKeyFormat = dropLocation.getAddressKeyFormat();
            if (addressKeyFormat != null) {
              populateAddressData(locatorResponseData, addressKeyFormat);
            }
            DistanceType distanceType = dropLocation.getDistance();
            DistanceData distance = new DistanceData();
            if (distanceType != null) {
              populateDistanceData(distanceType, distance);
            }
            locatorResponseData.setDistance(distance);
            locatorResponseData.setContactNumber(
                CollectionUtils.isNotEmpty(dropLocation.getPhoneNumber()) ? dropLocation
                    .getPhoneNumber().get(0) : "");
            locatorResponseDTOList.add(locatorResponseData);
          }
        }
      }
      upsLocatorResposeData.setResult(locatorResponseDTOList);
      upsLocatorResposeData
          .setStatusCode(locatorResponse.getResponse().getResponseStatusCode());
      upsLocatorResposeData
          .setStatusMessage(locatorResponse.getResponse().getResponseStatusDescription());
    }
  }

  /**
   * Create opening day details list for UPS store
   * @param openingDaysGroundDropOffTime ups response
   * @return list of string for opening day schedules
   */
  private List<String> getOpeningDaysDetails(final List<String> openingDaysGroundDropOffTime) {
    final List<String> openingDaysDetails = new ArrayList<>();
    for(String day : openingDaysGroundDropOffTime.get(0).split(";")) {
      if(day.contains(",")){
        openingDaysDetails.add(day.split(",")[0] + ":" +day.split(":")[1] + ":" +day.split(":")[2]);
        openingDaysDetails.add(day.split(",")[2]);
      } else {
        openingDaysDetails.add(day);
      }
    }
    return openingDaysDetails;
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

  /**
   * This method used for populating distance data.
   */
  private void populateDistanceData(DistanceType distanceType, DistanceData distance) {
    distance.setUnitCode(distanceType.getUnitOfMeasurement().getCode());
    distance.setUnitDescription(distanceType.getUnitOfMeasurement().getDescription());
    distance.setValue(distanceType.getValue());
  }
}
