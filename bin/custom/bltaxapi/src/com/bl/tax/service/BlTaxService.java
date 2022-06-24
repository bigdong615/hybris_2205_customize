package com.bl.tax.service;

import java.net.URISyntaxException;
import org.springframework.web.client.RestClientException;

/**
 * This interface created for Process the rquest
 * @author Manikandan
 */
public interface BlTaxService<REQUEST, RESPONSE> {

  /*
   * This method created for Process the request
   */
  public RESPONSE process(final REQUEST pRequest) throws RestClientException , URISyntaxException;

  public Double processShippingTax(final REQUEST pRequest, final Double amount) throws RestClientException , URISyntaxException;
}
