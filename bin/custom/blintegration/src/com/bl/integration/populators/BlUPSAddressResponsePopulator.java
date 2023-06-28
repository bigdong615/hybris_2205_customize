package com.bl.integration.populators;

import com.bl.core.enums.AddressTypeEnum;
import com.bl.facades.ups.address.data.AVSResposeData;
import com.bl.integration.constants.BlintegrationConstants;
import com.bl.integration.response.jaxb.Error;
import com.bl.integration.shipping.response.avsresponse.AddressKeyFormatType;
import com.bl.integration.shipping.response.avsresponse.AddressValidationResponse;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.commercefacades.user.data.CountryData;
import de.hybris.platform.commercefacades.user.data.RegionData;
import de.hybris.platform.core.model.c2l.CountryModel;
import de.hybris.platform.core.model.c2l.RegionModel;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;

public class BlUPSAddressResponsePopulator {

  @Resource(name = "commonI18NService")
  private CommonI18NService commonI18NService;

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
      }
      avsResposeData.setStatusCode(xavRespon.getResponse().getResponseStatusCode());
      avsResposeData.setStatusMessage(xavRespon.getResponse().getResponseStatusDescription());
    }
  }

  /**
   * Populating address data.
   */
  private AddressData populateAddressData(AddressKeyFormatType addressKeyFormat) {
    String code = addressKeyFormat.getAddressClassification().getCode();
    AddressData addressData = new AddressData();
    if (code.equals(BlintegrationConstants.RESIDENTIAL_ADDRESS_TYPE_CODE)) {
      addressData.setAddressType(AddressTypeEnum.RESIDENTIAL.getCode());
    } else if (code.equals(BlintegrationConstants.BUSINESS_ADDRESS_TYPE_CODE)) {
      addressData.setAddressType(AddressTypeEnum.BUSINESS.getCode());
    } else {
      addressData.setAddressType(BlintegrationConstants.ADDRESS_TYPE_UNKNOWN);
    }
    addressData.setLine1(addressKeyFormat.getAddressLine().get(0));
    if(addressKeyFormat.getAddressLine().size()>1) {
   	 addressData.setLine2(addressKeyFormat.getAddressLine().get(1));
    }
    addressData.setTown(addressKeyFormat.getPoliticalDivision2());
    addressData.setPostalCode(
        addressKeyFormat.getPostcodePrimaryLow() + "-" + addressKeyFormat.getPostcodeExtendedLow());
    final CountryModel country = commonI18NService.getCountry(addressKeyFormat.getCountryCode());
    final RegionData regionData = getRegionData(country, addressKeyFormat.getPoliticalDivision1());
    CountryData countryData = populateCountryData(country);
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

  /**
   * This method used for populating country data.
   */
  private CountryData populateCountryData(final CountryModel countryModel) {
    CountryData countryData = new CountryData();
    countryData.setIsocode(countryModel.getIsocode());
    countryData.setName(countryModel.getName());
    return countryData;
  }

  /**
   * This method used for providing region data.
   */
  private RegionData getRegionData(final CountryModel countryModel, final String iso) {
    String regionIso = countryModel.getIsocode() + "-" + iso;
    RegionModel regionModel = commonI18NService.getRegion(countryModel, regionIso);
    RegionData regionData = new RegionData();
    populateRegionData(regionModel, regionData);
    return regionData;
  }

  /**
   * This method used for populating region data.
   */
  private void populateRegionData(final RegionModel source, final RegionData target) {
    target.setName(source.getName());
    target.setIsocode(source.getIsocode());
    target.setIsocodeShort(source.getIsocodeShort());
    target.setCountryIso(source.getCountry().getIsocode());
  }
}
