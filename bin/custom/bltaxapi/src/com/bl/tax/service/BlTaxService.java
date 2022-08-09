package com.bl.tax.service;

import java.net.URISyntaxException;
import org.springframework.web.client.RestClientException;

import com.bl.tax.ResponseData;

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
  
  /**
   * Commit order to avalara.
   *
   * @param pRequest the request
   * @return true, if successful
   * @throws RestClientException the rest client exception
   * @throws URISyntaxException the URI syntax exception
   */
  public ResponseData commitOrderToAvalara(final REQUEST pRequest) throws RestClientException , URISyntaxException;
}
