package com.bl.tax.service;

/**
 * This interface created for Process the rquest
 * @author Manikandan
 */
public interface BlTaxService<REQUEST, RESPONSE> {

  /*
   * This method created for Process the request
   */
  public RESPONSE process(final REQUEST pRequest) throws Exception;
}
