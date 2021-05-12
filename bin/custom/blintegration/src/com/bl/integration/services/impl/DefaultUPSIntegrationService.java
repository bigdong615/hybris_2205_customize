package com.bl.integration.services.impl;

import com.bl.integration.accessrequest.jaxb.AccessRequest;
import com.bl.integration.accessrequest.jaxb.ObjectFactory;
import com.bl.integration.services.UPSIntegrationService;
import java.io.StringWriter;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import org.springframework.beans.factory.annotation.Value;

/**
 *This class was created to provide UPS Locator related common service.
 * @author vijay vishwakarma
 */
public class DefaultUPSIntegrationService implements UPSIntegrationService {

  @Value("${blintegration.ups.license.number}")
  private String licenseNumber;

  @Value("${blintegration.ups.license.uid}")
  private String uid;

  @Value("${blintegration.ups.license.password}")
  private String password;

  @Override
  public void  populateAccessRequest(StringWriter strWriter) throws JAXBException {
    ObjectFactory accessRequestObjectFactory = new ObjectFactory();
    AccessRequest accessRequest = accessRequestObjectFactory.createAccessRequest();
    accessRequest.setAccessLicenseNumber(licenseNumber);
    accessRequest.setUserId(uid);
    accessRequest.setPassword(password);
    marshalAccessRequest(accessRequest,strWriter);
  }

  /**
   *This method is used for marshaling access request.
   */
  private void marshalAccessRequest(AccessRequest accessRequest,StringWriter stringWriter)
      throws JAXBException {
    JAXBContext accessRequestJAXBC = JAXBContext.newInstance(AccessRequest.class.getPackage().getName());
    Marshaller accessRequestMarshaller = accessRequestJAXBC.createMarshaller();
    accessRequestMarshaller.marshal(accessRequest,stringWriter);
  }
}
