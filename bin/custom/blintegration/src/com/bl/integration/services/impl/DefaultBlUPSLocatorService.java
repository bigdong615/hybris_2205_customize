package com.bl.integration.services.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.facades.locator.data.UPSLocatorRequestData;
import com.bl.facades.locator.data.UpsLocatorResposeData;
import com.bl.integration.populators.BlLocatorRequestPopulator;
import com.bl.integration.populators.BlLocatorResponsePopulator;
import com.bl.integration.request.jaxb.LocatorRequest;
import com.bl.integration.request.jaxb.ObjectFactory;
import com.bl.integration.response.jaxb.LocatorResponse;
import com.bl.integration.services.BlUPSLocatorService;
import com.bl.integration.services.UPSIntegrationService;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import de.hybris.platform.webservicescommons.jaxb.Jaxb2HttpMessageConverter;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.StringWriter;
import javax.annotation.Resource;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import org.apache.commons.lang3.StringUtils;
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

/**
 *This class was created to perform UPS Locator-related services.
 * @author vijay vishwakarma
 */
public class DefaultBlUPSLocatorService  implements BlUPSLocatorService {

  private static final Logger LOGGER = LogManager.getLogger(DefaultBlUPSLocatorService.class);

  @Resource(name = "blLocatorRequestPopulator")
  private BlLocatorRequestPopulator blLocatorRequestPopulator;

  @Resource(name = "blLocatorResponsePopulator")
  private BlLocatorResponsePopulator blLocatorResponsePopulator;

  @Resource(name = "apiRegistryRestTemplate")
  private RestTemplate apiRegistryRestTemplate;

  @Resource(name = "upsIntegrationService")
  private UPSIntegrationService upsIntegrationService;

  @Resource(name="xmlHttpMessageConverter")
  private Jaxb2HttpMessageConverter jaxb2HttpMessageConverter;

  @Value("${blintegration.locator.endpointurl}")
  private String locatorEndPointUrl;

  @Override
  public UpsLocatorResposeData provideUPSLocation(UPSLocatorRequestData upsLocatorRequestData) {

    UpsLocatorResposeData upsLocatorResposeData = new UpsLocatorResposeData();
    String results;
    try {
      // Create Xml Request
      String xmlRequest = createRequest(upsLocatorRequestData);

      // Create http Request for calling UPS API.
      HttpEntity<?> entity = new HttpEntity<>(xmlRequest, new HttpHeaders());
      BlLogger.logMessage(LOGGER,Level.DEBUG,"UPS Locator URL :"+locatorEndPointUrl+"\nUPS Locator Request"+xmlRequest );

      // Calling UPS API.
      ResponseEntity<String> response = apiRegistryRestTemplate.exchange(locatorEndPointUrl, HttpMethod.POST, entity, String.class);

      if (response != null) { // NOSONAR
        results = response.getBody();
        BlLogger.logMessage(LOGGER,Level.DEBUG,
            "Response from UPS Locator\n"+
            "Response Status :" + response.getStatusCode() + "    Response code:" + response
            .getStatusCodeValue() + "  Response Data :" + results);

        upsLocatorResposeData.setStatusCode(String.valueOf(response.getStatusCodeValue()));
        upsLocatorResposeData.setStatusMessage(response.getStatusCode().toString());
        populateResponse(upsLocatorResposeData, results);
      }
    } catch (JAXBException e) {
      populateResponseExceptionData(upsLocatorResposeData,e);
      BlLogger.logFormatMessageInfo(LOGGER,Level.ERROR, LogErrorCodeEnum.UPS_LOCATOR_INTEGRATION_ERROR.getCode(),e,"Some error occure whiling Marshaling :{}",e.getMessage());
    } catch (RestClientException e) {
      populateResponseExceptionData(upsLocatorResposeData,e);
      BlLogger.logFormatMessageInfo(LOGGER,Level.ERROR, LogErrorCodeEnum.UPS_LOCATOR_INTEGRATION_ERROR.getCode(),e,"Some error occure whiling calling UPS API :{}",e.getMessage());
    } catch (Exception e) {
      populateResponseExceptionData(upsLocatorResposeData,e);
      BlLogger.logFormatMessageInfo(LOGGER,Level.ERROR, LogErrorCodeEnum.UPS_LOCATOR_INTEGRATION_ERROR.getCode(),e,"Some error occure from UPS service :{}",e.getMessage());
    }
    return upsLocatorResposeData;
  }

  /**
   * This method used for creating xml request.
   */
  private String createRequest(UPSLocatorRequestData upsLocatorRequestData)
      throws JAXBException {
      String xmlRequest = null;
    try (StringWriter  strWriter = new StringWriter();)
    {
      upsIntegrationService.populateAccessRequest(strWriter);
      ObjectFactory requestObjectFactory = new ObjectFactory();
      LocatorRequest locatorRequest = requestObjectFactory.createLocatorRequest();
      blLocatorRequestPopulator.populateLocatorRequest(locatorRequest, upsLocatorRequestData);
      marshalLocatorRequest(locatorRequest,strWriter);
      strWriter.flush();
      xmlRequest = strWriter.getBuffer().toString();
    } catch (IOException e) {
      BlLogger.logFormatMessageInfo(LOGGER,Level.ERROR, LogErrorCodeEnum.UPS_LOCATOR_INTEGRATION_ERROR.getCode(),e,"Some error occure whiling create buffer :{}",e.getMessage());
    }
     return xmlRequest;
  }

 /**
  *  This method used for populating response.
  */
  private void populateResponse(UpsLocatorResposeData upsLocatorResposeData, String results)
      throws JAXBException {
    if (StringUtils.isNotEmpty(results)) {
      LocatorResponse locatorResponse =  unmarshalLocatorResponse(results);
      // Populate Response data.
      if (locatorResponse != null) {
        blLocatorResponsePopulator.populateDropDownLocation(upsLocatorResposeData, locatorResponse);
      }
    }
  }

 /**
  *  This method used for marshaling Locator request.
  */
  private void marshalLocatorRequest(LocatorRequest locatorRequest,StringWriter stringWriter) throws JAXBException {
    JAXBContext locatorRequestJAXBC = JAXBContext.newInstance(LocatorRequest.class.getPackage().getName());
    Marshaller locatorRequestMarshaller = locatorRequestJAXBC.createMarshaller();
    locatorRequestMarshaller.marshal(locatorRequest, stringWriter);
  }

  /**
   *  This method used for unmarshaling Locator response.
   */
  private LocatorResponse unmarshalLocatorResponse(String results) throws JAXBException {
    JAXBContext locatorResponseJAXBC = JAXBContext.newInstance(LocatorResponse.class.getPackage().getName());
    Unmarshaller locatorResponseUnmarshaller = locatorResponseJAXBC.createUnmarshaller();
    ByteArrayInputStream input = new ByteArrayInputStream(results.getBytes()); // NOSONAR
    return (LocatorResponse)locatorResponseUnmarshaller.unmarshal(input);
  }

  /**
   *  This method used for populating Exception.
   */
  private void populateResponseExceptionData(UpsLocatorResposeData upsLocatorResposeData,Exception e){
    upsLocatorResposeData.setStatusCode(BlCoreConstants.INTERNAL_SERVER_ERROR_CODE);
    upsLocatorResposeData.setStatusMessage(BlCoreConstants.FAILURE_STRING);
    upsLocatorResposeData.setErrorDescription(e.getMessage());
  }
}
