package com.bl.integration.populators;

import com.bl.facades.locator.data.UPSLocatorRequestData;
import com.bl.integration.request.jaxb.AddressKeyFormatType;
import com.bl.integration.request.jaxb.CodeType;
import com.bl.integration.request.jaxb.LocationSearchCriteriaType;
import com.bl.integration.request.jaxb.LocatorRequest;
import com.bl.integration.request.jaxb.OriginAddressType;
import com.bl.integration.request.jaxb.Request;
import com.bl.integration.request.jaxb.ServiceSearchType;
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

  @Value("${blintegration.locator.codetype}")
  private String code;


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

  LocationSearchCriteriaType location = new LocationSearchCriteriaType();
  ServiceSearchType search = new ServiceSearchType();
  search.setTime(requestTimeout);

  CodeType codeType = new CodeType();
  codeType.setCode(code);
  search.getServiceCode().add(codeType);

  location.setServiceSearch(search);
  locatorRequest.setLocationSearchCriteria(location);
}
}
