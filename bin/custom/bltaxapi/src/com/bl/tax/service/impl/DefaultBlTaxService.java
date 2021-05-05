package com.bl.tax.service.impl;

import com.bl.tax.TaxResponse;
import com.bl.tax.resttemplate.BlRestTemplate;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import org.apache.commons.codec.binary.Base64;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

public class DefaultBlTaxService<REQUEST, RESPONSE, SERVICEREQUEST, SERVICERESPONSE> {


  /** The FsxRestTemplate. */
  private BlRestTemplate blRestTemplate;



  /** The request populator. */
 private Populator<REQUEST, SERVICEREQUEST> requestPopulator;

  /** The response popualtor. */
  private Populator<SERVICERESPONSE, RESPONSE> responsePopulator;



  private Populator<TaxResponse, AbstractOrderModel> blAvalaraTaxPopulator;

  /** The service path url. */
  private String servicePathUrl;

  /** The user name. */
  private String userName;

  /** The password. */
  private String password;

  protected SERVICERESPONSE process(final HttpEntity<?> requestEntity, final Class responseClazz) throws Exception
  {
    final ResponseEntity<SERVICERESPONSE> responeEntity = getBlRestTemplate().executeRequest(getServicePathUrl(),
        requestEntity, responseClazz);
    return responeEntity.getBody();
  }

  protected HttpEntity<SERVICEREQUEST> createHttpEntity(final SERVICEREQUEST pRequest)
  {
    return new HttpEntity<SERVICEREQUEST>(pRequest, addHeadersToServiceRequest());
  }

  protected HttpHeaders addHeadersToServiceRequest()
  {
    final HttpHeaders headers = new HttpHeaders();
    headers.add(HttpHeaders.AUTHORIZATION, createAuthorization());
    headers.add(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE);
    return headers;
  }

  protected String createAuthorization()
  {
    final String plainCreds = userName.concat(":").concat(password);
    final byte[] plainCredsBytes = plainCreds.getBytes();
    final byte[] base64CredsBytes = Base64.encodeBase64(plainCredsBytes);
    return "Basic " + new String(base64CredsBytes);
  }



  public BlRestTemplate getBlRestTemplate() {
    return blRestTemplate;
  }

  public void setBlRestTemplate(BlRestTemplate blRestTemplate) {
    this.blRestTemplate = blRestTemplate;
  }

  public String getServicePathUrl() {
    return servicePathUrl;
  }

  public void setServicePathUrl(String servicePathUrl) {
    this.servicePathUrl = servicePathUrl;
  }

  public String getUserName() {
    return userName;
  }

  public void setUserName(String userName) {
    this.userName = userName;
  }

  public String getPassword() {
    return password;
  }

  public void setPassword(String password) {
    this.password = password;
  }

  public Populator<REQUEST, SERVICEREQUEST> getRequestPopulator() {
    return requestPopulator;
  }

  public void setRequestPopulator(
      Populator<REQUEST, SERVICEREQUEST> requestPopulator) {
    this.requestPopulator = requestPopulator;
  }

  public Populator<SERVICERESPONSE, RESPONSE> getResponsePopulator() {
    return responsePopulator;
  }

  public void setResponsePopulator(
      Populator<SERVICERESPONSE, RESPONSE> responsePopulator) {
    this.responsePopulator = responsePopulator;
  }

  public Populator<TaxResponse, AbstractOrderModel> getBlAvalaraTaxPopulator() {
    return blAvalaraTaxPopulator;
  }

  public void setBlAvalaraTaxPopulator(
      Populator<TaxResponse, AbstractOrderModel> blAvalaraTaxPopulator) {
    this.blAvalaraTaxPopulator = blAvalaraTaxPopulator;
  }

}
