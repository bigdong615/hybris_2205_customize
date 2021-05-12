package com.bl.integration.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.AddressTypeEnum;
import com.bl.facades.ups.address.data.AVSResposeData;
import com.bl.integration.response.jaxb.Error;
import com.bl.integration.shipping.response.avsresponse.AddressKeyFormatType;
import com.bl.integration.shipping.response.avsresponse.AddressValidationResponse;
import de.hybris.platform.commercefacades.i18n.I18NFacade;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.commercefacades.user.data.CountryData;
import de.hybris.platform.commercefacades.user.data.RegionData;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;

public class BlUPSAddressResponsePopulator {

  @Resource(name = "i18NFacade")
  private I18NFacade i18NFacade;

  /**
   * Populating response data from UPS Address Validator.
   */
  public void populateAddressKeyFormatData(AddressValidationResponse xavRespon,
      AVSResposeData avsResposeData) {
    List<AddressKeyFormatType> addressKeyformatList = xavRespon.getAddressKeyFormat();
    if (CollectionUtils.isNotEmpty(xavRespon.getResponse().getError())) {
      populateErrorData(avsResposeData, xavRespon.getResponse().getError().get(0),
          xavRespon.getResponse().getResponseStatusDescription());
    } else {
      if (CollectionUtils.isNotEmpty(addressKeyformatList)) {
        List<AddressData> addressDataList = new ArrayList<>();
        addressKeyformatList.forEach(addressKeyFormat -> {
          AddressData addressData = populateAddressData(addressKeyFormat);
          addressDataList.add(addressData);
        });
        avsResposeData.setResult(addressDataList);
        avsResposeData.setStatusCode(xavRespon.getResponse().getResponseStatusCode());
        avsResposeData.setStatusMessage(xavRespon.getResponse().getResponseStatusDescription());
      }
    }
  }

  /**
   * Populating address data.
   */
  private AddressData populateAddressData(AddressKeyFormatType addressKeyFormat) {
    String code = addressKeyFormat.getAddressClassification().getCode();
    AddressData addressData = new AddressData();
    if (code.equals(BlCoreConstants.RESIDENTIAL_ADDRESS_TYPE_CODE)) {
      addressData.setAddressType(AddressTypeEnum.RESIDENTIAL.getCode());
    } else if (code.equals(BlCoreConstants.BUSINESS_ADDRESS_TYPE_CODE)) {
      addressData.setAddressType(AddressTypeEnum.BUSINESS.getCode());
    } else {
      addressData.setAddressType(BlCoreConstants.ADDRESS_TYPE_UNKNOWN);
    }
    addressData.setLine1(addressKeyFormat.getAddressLine().get(0));
    addressData.setTown(addressKeyFormat.getPoliticalDivision2());
    addressData.setPostalCode(
        addressKeyFormat.getPostcodePrimaryLow() + "-" + addressKeyFormat.getPostcodeExtendedLow());
    final CountryData countryData = i18NFacade
        .getCountryForIsocode(addressKeyFormat.getCountryCode());
    final RegionData regionData = i18NFacade.getRegion(addressKeyFormat.getCountryCode(),
        addressKeyFormat.getCountryCode() + "-" + addressKeyFormat.getPoliticalDivision1());
    addressData.setCountry(countryData);
    addressData.setRegion(regionData);
    return addressData;
  }

  /**
   * This method used for populating error data.
   */
  private void populateErrorData(AVSResposeData avsResposeData, Error error, String statusMessage) {
    avsResposeData.setStatusCode(error.getErrorCode());
    avsResposeData.setStatusMessage(statusMessage);
    avsResposeData.setErrorDescription(error.getErrorDescription());
    avsResposeData.setResult(Collections.emptyList());
  }

}
