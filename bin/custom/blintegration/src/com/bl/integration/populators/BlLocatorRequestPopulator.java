package com.bl.integration.populators;

import com.bl.facades.locator.data.UPSLocatorRequestData;
import com.bl.integration.request.jaxb.AccessPointSearchType;
import com.bl.integration.request.jaxb.AddressKeyFormatType;
import com.bl.integration.request.jaxb.CodeType;
import com.bl.integration.request.jaxb.IncludeCriteriaType;
import com.bl.integration.request.jaxb.LocationSearchCriteriaType;
import com.bl.integration.request.jaxb.LocatorRequest;
import com.bl.integration.request.jaxb.OptionCodeType;
import com.bl.integration.request.jaxb.OriginAddressType;
import com.bl.integration.request.jaxb.Request;
import com.bl.integration.request.jaxb.SearchFilterType;
import com.bl.integration.request.jaxb.SearchOptionType;
import com.bl.integration.request.jaxb.SortCriteriaType;
import com.bl.integration.request.jaxb.TransactionReference;
import com.bl.integration.request.jaxb.TranslateType;
import com.bl.integration.request.jaxb.UnitOfMeasurementType;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;

/**
 *This class was created to populate UPS locator request.
 * @author vijay vishwakarma
 */
public class BlLocatorRequestPopulator {

  @Value("${blintegration.locator.request.action}")
  private String requestAction;

  @Value("${blintegration.locator.request.option}")
  private String requestOption;

  @Value("${blintegration.locator.country.code}")
  private String countryCode;

  @Value("${blintegration.locator.country.language.code}")
  private String languageCode;

  @Value("${blintegration.locator.unitOfMeasurement}")
  private String measurementUnit;

  @Value("${blintegration.locator.timeout}")
  private String requestTimeout;

  @Value("${blintegration.locator.codeType}")
  private String codeTypeCode;

  @Value("${blintegration.locator.option.codeType}")
  private String optionCodeTypeCode;

  @Value("${blintegration.locator.access.point.status}")
  private String accessPointStatus;

  @Value("${blintegration.locator.maximum.size}")
  private String maximumSize;

  @Value("${blintegration.locator.sort.type}")
  private String sortType;


public void populateLocatorRequest(final LocatorRequest locatorRequest,final UPSLocatorRequestData locatorFormDTO){
  Request request = new Request();
  request.setRequestAction(requestAction);
  request.setRequestOption(requestOption);
  TransactionReference trRef = new TransactionReference();
  request.setTransactionReference(trRef);
  locatorRequest.setRequest(request);

  OriginAddressType origianAddType = new OriginAddressType();

  AddressKeyFormatType addressType = new AddressKeyFormatType();
  addressType.setPostcodePrimaryLow(locatorFormDTO.getZipcode());
  if(StringUtils.isNotEmpty(locatorFormDTO.getCountryCode())){
  addressType.setCountryCode(locatorFormDTO.getCountryCode());
  }
  else{
    addressType.setCountryCode(countryCode);
  }
  origianAddType.setAddressKeyFormat(addressType);
  locatorRequest.setOriginAddress(origianAddType);

  TranslateType translate = new TranslateType();
  translate.setLanguageCode(languageCode);
  locatorRequest.setTranslate(translate);

  UnitOfMeasurementType unit = new UnitOfMeasurementType();
  unit.setCode(measurementUnit);
  locatorRequest.setUnitOfMeasurement(unit);

  /************* setting LocationSearchCriteriaType start***********/
  LocationSearchCriteriaType locationSearchCriteriaType = new LocationSearchCriteriaType();

  CodeType codeType = new CodeType();
  codeType.setCode(codeTypeCode);
  OptionCodeType optionCodeType = new OptionCodeType();
  optionCodeType.setCode(optionCodeTypeCode);
  SearchOptionType searchOptionType = new SearchOptionType();
  searchOptionType.setOptionType(codeType);
  searchOptionType.getOptionCode().add(optionCodeType);

  IncludeCriteriaType criteriaType = new IncludeCriteriaType();
  SearchFilterType filterType = new SearchFilterType();
  filterType.setShippingAvailabilityIndicator(StringUtils.EMPTY);
  criteriaType.setSearchFilter(filterType);

  AccessPointSearchType accessPointSearchType = new AccessPointSearchType();
  accessPointSearchType.setAccessPointStatus(accessPointStatus);
  accessPointSearchType.setIncludeCriteria(criteriaType);

  locationSearchCriteriaType.getSearchOption().add(searchOptionType);
  locationSearchCriteriaType.setAccessPointSearch(accessPointSearchType);
  locationSearchCriteriaType.setMaximumListSize(maximumSize);

  locatorRequest.setLocationSearchCriteria(locationSearchCriteriaType);
  /*************setting LocationSearchCriteriaType start***********/

  /********setting SortCriteriaType **/
  SortCriteriaType sortCriteriaType = new SortCriteriaType();
  sortCriteriaType.setSortType(sortType);
  locatorRequest.setSortCriteria(sortCriteriaType);
}
}
