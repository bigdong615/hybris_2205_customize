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

  /**
   * Process shipping tax.
   *
   * @param pRequest the request
   * @param amount the amount
   * @return the double
   * @throws RestClientException the rest client exception
   * @throws URISyntaxException the URI syntax exception
   */
  public Double processShippingTax(final REQUEST pRequest, final Double amount) throws RestClientException , URISyntaxException;
}
