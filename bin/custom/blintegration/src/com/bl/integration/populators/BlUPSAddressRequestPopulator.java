package com.bl.integration.populators;

import com.bl.integration.shipping.request.avsrequest.AddressKeyFormatType;
import com.bl.integration.shipping.request.avsrequest.AddressValidationRequest;
import com.bl.integration.shipping.request.avsrequest.RequestType;
import de.hybris.platform.commercefacades.user.data.AddressData;

public class BlUPSAddressRequestPopulator {

  public void populateAddressRequest(AddressData addressData, AddressValidationRequest xavRequest){

    RequestType request =populateRequestType();
    xavRequest.setRequest(request);
    AddressKeyFormatType addressKeyFormatType=populateAddressType(addressData);
    xavRequest.getAddressKeyFormat().add(addressKeyFormatType);
  }

  private RequestType populateRequestType(){
    RequestType request =  new RequestType();
    request.setRequestOption("3");
    request.setRequestAction("XAV");
    return request;
  }
  private AddressKeyFormatType populateAddressType(AddressData addressData){
    AddressKeyFormatType addressKeyFormatType = new AddressKeyFormatType();
    addressKeyFormatType.setConsigneeName(addressData.getFirstName()+" "+addressData.getLastName());
    addressKeyFormatType.getAddressLine().add(addressData.getLine1());
    addressKeyFormatType.getAddressLine().add(addressData.getLine2());
    addressKeyFormatType.setPoliticalDivision2(addressData.getTown());
    addressKeyFormatType.setPoliticalDivision1(addressData.getRegion().getIsocodeShort());
    addressKeyFormatType.setPostcodePrimaryLow(addressData.getPostalCode());
    addressKeyFormatType.setCountryCode(addressData.getCountry().getIsocode());
    return addressKeyFormatType;
  }
}
