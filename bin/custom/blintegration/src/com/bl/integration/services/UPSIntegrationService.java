package com.bl.integration.services;

import com.bl.integration.accessrequest.jaxb.AccessRequest;
import java.io.StringWriter;
import javax.xml.bind.JAXBException;

public interface UPSIntegrationService {
  public void  populateAccessRequest(StringWriter strWriter) throws JAXBException;
}
