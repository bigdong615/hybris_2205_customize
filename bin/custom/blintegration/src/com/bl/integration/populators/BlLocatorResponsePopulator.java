package com.bl.integration.populators;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Value;

import com.bl.facades.locator.data.DistanceData;
import com.bl.facades.locator.data.UpsLocatorResposeData;
import com.bl.facades.locator.data.UpsStoreData;
import com.bl.integration.response.jaxb.AddressKeyFormatType;
import com.bl.integration.response.jaxb.DistanceType;
import com.bl.integration.response.jaxb.DropLocationType;
import com.bl.integration.response.jaxb.Error;
import com.bl.integration.response.jaxb.LocatorResponse;
import com.bl.integration.response.jaxb.SearchResultsType;

/**
 *This class was created to populate UPS Locator response.
 * @author vijay vishwakarma
 */
public class BlLocatorResponsePopulator {

  @Value("${blintegration.locator.result.count}")
  private String maximumResult;

  public void populateDropDownLocation(final UpsLocatorResposeData upsLocatorResposeData,final LocatorResponse locatorResponse) {
    final List<UpsStoreData> locatorResponseDTOList = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(locatorResponse.getResponse().getError())) {
      populateErrorData(upsLocatorResposeData,locatorResponse.getResponse().getError().get(0),locatorResponse.getResponse().getResponseStatusDescription());
    } else {
      final SearchResultsType searchResult = locatorResponse.getSearchResults();
      final List<Object> locationList = searchResult.getDisclaimerAndDropLocation();
      if (CollectionUtils.isNotEmpty(locationList)) {
        for (int count = 0; count < locationList.size(); count++) {
          if (count > (Integer.parseInt(maximumResult) - 1)) {
            break;
          }
          final Object location = locationList.get(count);
          if (location instanceof DropLocationType) {
            final DropLocationType dropLocation = (DropLocationType) location;
            final UpsStoreData locatorResponseData = new UpsStoreData();
            locatorResponseData.setLocationId(dropLocation.getLocationID());

            final List<String> openingDaysGroundDropOffTime =new ArrayList<>();
            openingDaysGroundDropOffTime.add(dropLocation.getStandardHoursOfOperation());
            if (CollectionUtils.isNotEmpty(openingDaysGroundDropOffTime)) {
              locatorResponseData
                  .setLatestGroundDropOffTime(getOpeningDaysDetails(openingDaysGroundDropOffTime));
            }

            final AddressKeyFormatType addressKeyFormat = dropLocation.getAddressKeyFormat();
            if (addressKeyFormat != null) {
              populateAddressData(locatorResponseData, addressKeyFormat);
            }
            final DistanceType distanceType = dropLocation.getDistance();
            final DistanceData distance = new DistanceData();
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
    for(final String day : openingDaysGroundDropOffTime.get(0).split(";")) {
        openingDaysDetails.add(day);
    }
    return openingDaysDetails;
  }

  /**
 * This method used for populating error data.
 */
  private void populateErrorData(final UpsLocatorResposeData upsLocatorResposeData,final Error error , final String statusMessage){
    upsLocatorResposeData.setStatusCode(error.getErrorCode());
    upsLocatorResposeData.setStatusMessage(statusMessage);
    upsLocatorResposeData.setErrorDescription(error.getErrorDescription());
    upsLocatorResposeData.setResult(Collections.emptyList());
  }

  /**
   * This method used for populating address data.
   */
  private void populateAddressData( final UpsStoreData locatorResponseData,final AddressKeyFormatType addressKeyFormat){
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
  private void populateDistanceData(final DistanceType distanceType, final DistanceData distance) {
    distance.setUnitCode(distanceType.getUnitOfMeasurement().getCode());
    distance.setUnitDescription(distanceType.getUnitOfMeasurement().getDescription());
    distance.setValue(distanceType.getValue());
  }
}
