package com.bl.integration.services.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.facades.ups.address.data.AVSResposeData;
import com.bl.integration.populators.BlUPSAddressRequestPopulator;
import com.bl.integration.populators.BlUPSAddressResponsePopulator;
import com.bl.integration.services.BlUPSAddressValidatorService;
import com.bl.integration.services.UPSIntegrationService;
import com.bl.integration.shipping.response.avsresponse.AddressKeyFormatType;
import com.bl.integration.shipping.request.avsrequest.ObjectFactory;
import com.bl.integration.shipping.request.avsrequest.AddressValidationRequest;
import com.bl.integration.shipping.response.avsresponse.AddressValidationResponse;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import de.hybris.platform.commercefacades.user.data.AddressData;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.StringWriter;
import java.util.List;
import javax.annotation.Resource;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import org.apache.log4j.Level;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;
import org.apache.commons.lang3.StringUtils;

public class DefaultBlUPSAddressValidatorService implements BlUPSAddressValidatorService {
  private static final Logger LOGGER = LogManager.getLogger(DefaultBlUPSAddressValidatorService.class);

  @Value("${blintegration.ups.address.validator.endpointurl}")
  private String upsEndPointUrl;
  @Resource(name = "upsIntegrationService")
  private UPSIntegrationService upsIntegrationService;
  @Resource(name = "apiRegistryRestTemplate")
  private RestTemplate apiRegistryRestTemplate;

  @Resource(name = "blUPSAddressRequestPopulator")
  private BlUPSAddressRequestPopulator blUPSAddressRequestPopulator;

  @Resource(name = "blUPSAddressResponsePopulator")
  private BlUPSAddressResponsePopulator blUPSAddressResponsePopulator;

  @Override
  public AVSResposeData getVerifiedAddress(AddressData addressData){
    AVSResposeData avsResposeData = new AVSResposeData();
    String results=null;
    try {
      String xmlRequest = createRequest(addressData);

      HttpEntity<?> entity = new HttpEntity<>(xmlRequest, new HttpHeaders());
      BlLogger.logMessage(LOGGER,Level.INFO,"UPS address validator URL :"+upsEndPointUrl+"\nUPS address Request"+xmlRequest );

      // Calling UPS API.
      ResponseEntity<String> response = apiRegistryRestTemplate.exchange(upsEndPointUrl, HttpMethod.POST, entity, String.class);
      if (response != null) { // NOSONAR
        results = response.getBody();
        BlLogger.logMessage(LOGGER, Level.INFO,
            "Response from UPS address validator\n" +
                "Response Status :" + response.getStatusCode() + "    Response code:" + response
                .getStatusCodeValue() + "  Response Data :" + results);
      }

      avsResposeData.setStatusCode(String.valueOf(response.getStatusCodeValue()));
      avsResposeData.setStatusMessage(response.getStatusCode().toString());
      populateResponse(avsResposeData,results);


  } catch (JAXBException e) {
    populateResponseExceptionData(avsResposeData,e);
    BlLogger.logFormatMessageInfo(LOGGER,Level.ERROR, LogErrorCodeEnum.UPS_LOCATOR_INTEGRATION_ERROR.getCode(),e,"Some error occure whiling Marshaling :{}",e.getMessage());
  } catch (
  RestClientException e) {
    populateResponseExceptionData(avsResposeData,e);
    BlLogger.logFormatMessageInfo(LOGGER,Level.ERROR, LogErrorCodeEnum.UPS_LOCATOR_INTEGRATION_ERROR.getCode(),e,"Some error occure whiling calling UPS API :{}",e.getMessage());
  } catch (Exception e) {
    populateResponseExceptionData(avsResposeData,e);
    BlLogger.logFormatMessageInfo(LOGGER,Level.ERROR, LogErrorCodeEnum.UPS_LOCATOR_INTEGRATION_ERROR.getCode(),e,"Some error occure from UPS service :{}",e.getMessage());
  }
return avsResposeData;
  }




  /**
   * This method used for creating xml request.
   */
  private String createRequest(AddressData addressData)
      throws JAXBException {
    String xmlRequest = null;
    try (StringWriter  strWriter = new StringWriter();)
    {
      upsIntegrationService.populateAccessRequest(strWriter);
      /*ObjectFactory requestObjectFactory = new ObjectFactory();
      AddressValidationRequest xavRequest = requestObjectFactory.createAddressValidationRequest();*/
      AddressValidationRequest xavRequest = new AddressValidationRequest();
      blUPSAddressRequestPopulator.populateAddressRequest(addressData,xavRequest);
      marshalAddressRequest(xavRequest,strWriter);
      strWriter.flush();
      xmlRequest = strWriter.getBuffer().toString();
    } catch (IOException e) {
      BlLogger.logFormatMessageInfo(LOGGER, Level.ERROR, LogErrorCodeEnum.UPS_LOCATOR_INTEGRATION_ERROR.getCode(),e,"Some error occure whiling create buffer :{}",e.getMessage());
    }
    return xmlRequest;
  }

  /**
   *  This method used for marshaling Locator request.
   */
  private void marshalAddressRequest(AddressValidationRequest validaterRequest,StringWriter stringWriter) throws JAXBException {
    JAXBContext avRequestJAXBC = JAXBContext.newInstance(AddressValidationRequest.class.getPackage().getName());
    Marshaller avRequestMarshaller = avRequestJAXBC.createMarshaller();
    avRequestMarshaller.marshal(validaterRequest  , stringWriter);
  }

  private void populateResponse(AVSResposeData  avsResposeData,String results ) throws JAXBException {

    if (StringUtils.isNotEmpty(results)) {
      AddressValidationResponse xavResponse =  unmarshalAVResponse(results);

      if (xavResponse != null) {
        blUPSAddressResponsePopulator.populateAddressKeyFormateData( xavResponse,avsResposeData);
      }
    }
  }

  private AddressValidationResponse unmarshalAVResponse(String results) throws JAXBException {
    JAXBContext xavResponseJAXBC = JAXBContext.newInstance(AddressValidationResponse.class.getPackage().getName());
    Unmarshaller xavResponseUnmarhsaller = xavResponseJAXBC.createUnmarshaller();
    ByteArrayInputStream input = new ByteArrayInputStream(results.getBytes());
    Object objResponse = xavResponseUnmarhsaller.unmarshal(input);
    return  (AddressValidationResponse)objResponse;
  }

  /**
   *  This method used for populating Exception.
   */
  private void populateResponseExceptionData(AVSResposeData  avsResposeData,Exception e){
    avsResposeData.setStatusCode(BlCoreConstants.INTERNAL_SERVER_ERROR_CODE);
    avsResposeData.setStatusMessage(BlCoreConstants.FAILURE_STRING);
    avsResposeData.setErrorDescription(e.getMessage());
  }
}
