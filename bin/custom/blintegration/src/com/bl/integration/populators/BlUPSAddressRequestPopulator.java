package com.bl.integration.populators;

import com.bl.integration.shipping.request.avsrequest.AddressKeyFormatType;
import com.bl.integration.shipping.request.avsrequest.AddressValidationRequest;
import com.bl.integration.shipping.request.avsrequest.RequestType;
import de.hybris.platform.commercefacades.user.data.AddressData;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;

/**
 *This class was created to populate UPS address validator request.
 * @author vijay vishwakarma
 */
public class BlUPSAddressRequestPopulator {

  @Value("${blintegration.ups.address.validator.request.option}")
  private String requestOption;

  @Value("${blintegration.ups.address.validator.request.action}")
  private String requestAction;

  /**
   * Populating address validator request.
   */
  public void populateAddressRequest(AddressData addressData, AddressValidationRequest xavRequest) {
    RequestType request = populateRequestType();
    xavRequest.setRequest(request);
    AddressKeyFormatType addressKeyFormatType = populateAddressType(addressData);
    xavRequest.getAddressKeyFormat().add(addressKeyFormatType);
  }

  /**
   * Populating request type.
   */
  private RequestType populateRequestType() {
    RequestType request = new RequestType();
    request.setRequestOption(requestOption);
    request.setRequestAction(requestAction);
    return request;
  }

  /**
   * Populating address data.
   */
  private AddressKeyFormatType populateAddressType(AddressData addressData) {
    AddressKeyFormatType addressKeyFormatType = new AddressKeyFormatType();
    addressKeyFormatType.setConsigneeName(
        addressData.getFirstName() + (StringUtils.isNotEmpty(addressData.getLastName()) ? (" "
            + addressData.getLastName()) : ""));
    addressKeyFormatType.getAddressLine().add(addressData.getLine1());
    if (StringUtils.isNotEmpty(addressData.getLine2())) {
      addressKeyFormatType.getAddressLine().add(addressData.getLine2());
    }
    addressKeyFormatType.setPoliticalDivision2(addressData.getTown());
    addressKeyFormatType.setPoliticalDivision1(addressData.getRegion().getIsocodeShort());
    addressKeyFormatType.setPostcodePrimaryLow(addressData.getPostalCode());
    addressKeyFormatType.setCountryCode(addressData.getCountry().getIsocode());
    return addressKeyFormatType;
  }
}
