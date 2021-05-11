package com.bl.integration.populators;

import com.bl.core.enums.AddressTypeEnum;
import com.bl.facades.ups.address.data.AVSResposeData;
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
import com.bl.integration.response.jaxb.Error;

public class BlUPSAddressResponsePopulator {

  @Resource(name = "i18NFacade")
  private I18NFacade i18NFacade;

  public void populateAddressKeyFormateData(AddressValidationResponse xavRespon,AVSResposeData  avsResposeData){
    List<AddressKeyFormatType> addressKeyformatList  = xavRespon.getAddressKeyFormat();
    if (CollectionUtils.isNotEmpty(xavRespon.getResponse().getError())) {
      populateErrorData(avsResposeData,xavRespon.getResponse().getError().get(0),xavRespon.getResponse().getResponseStatusDescription());
    } else {
    if(CollectionUtils.isNotEmpty(addressKeyformatList)){
    List<AddressData> addressDataList = new ArrayList<>();
      addressKeyformatList.forEach( addressKeyFormat ->{
        String code = addressKeyFormat.getAddressClassification().getCode();
        AddressData addressData = new AddressData();
        if(code.equals("2")){
          addressData.setAddressType(AddressTypeEnum.RESIDENTIAL.getCode());
        }else if(code.equals("1")){
          addressData.setAddressType(AddressTypeEnum.BUSINESS.getCode());
        }else{
          addressData.setAddressType("UNKNOWN");
        }
        addressData.setLine1(addressKeyFormat.getAddressLine().get(0));
        addressData.setTown(addressKeyFormat.getPoliticalDivision2());
         // need to get resion model
        addressData.setPostalCode(addressKeyFormat.getPostcodePrimaryLow()+"-"+addressKeyFormat.getPostcodeExtendedLow());

        final CountryData countryData = i18NFacade.getCountryForIsocode(addressKeyFormat.getCountryCode());
        final RegionData regionData = i18NFacade.getRegion(addressKeyFormat.getCountryCode(), addressKeyFormat.getPoliticalDivision1());
        addressData.setCountry(countryData);
        addressData.setRegion(regionData);
        addressDataList.add(addressData);
      });
      avsResposeData.setResult(addressDataList);
      avsResposeData.setStatusCode(xavRespon.getResponse().getResponseStatusCode());
      avsResposeData.setStatusMessage(xavRespon.getResponse().getResponseStatusDescription());
    }
  }
  }

  /**
   * This method used for populating error data.
   */
  private void populateErrorData(AVSResposeData  avsResposeData,Error error , String statusMessage){
    avsResposeData.setStatusCode(error.getErrorCode());
    avsResposeData.setStatusMessage(statusMessage);
    avsResposeData.setErrorDescription(error.getErrorDescription());
    avsResposeData.setResult(Collections.emptyList());
  }

}
