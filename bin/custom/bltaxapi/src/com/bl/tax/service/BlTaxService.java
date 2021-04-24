package com.bl.tax.service;

public interface BlTaxService<REQUEST, RESPONSE> {

  public RESPONSE process(final REQUEST pRequest) throws Exception;
}
