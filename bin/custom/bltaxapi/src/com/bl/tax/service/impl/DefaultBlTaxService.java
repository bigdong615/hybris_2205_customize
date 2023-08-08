package com.bl.tax.service.impl;

import com.bl.tax.ResponseData;
import com.bl.tax.TaxResponse;
import com.bl.tax.billing.BillingPojo;
import com.bl.tax.resttemplate.BlRestTemplate;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import java.net.URISyntaxException;
import org.apache.commons.codec.binary.Base64;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestClientException;

/**
 * This class created for excute request
 * @author Manikandan
 */
public class DefaultBlTaxService<REQUEST, RESPONSE, SERVICEREQUEST, SERVICERESPONSE> {


  private BlRestTemplate blRestTemplate;
  private Populator<REQUEST, SERVICEREQUEST> requestPopulator;
  private Populator<SERVICERESPONSE, RESPONSE> responsePopulator;
  private Populator<BillingPojo, SERVICEREQUEST> billingRequestPopulator;
  private Populator<TaxResponse, AbstractOrderModel> blAvalaraTaxPopulator;
  private Populator<ResponseEntity<SERVICERESPONSE> ,ResponseData> blResponseDetailsAndLoggingPopulator;

  private Populator<SERVICERESPONSE, RESPONSE> blPayBillTaxResponsePopulator;

  private String servicePathUrl;
  private String userName;
  private String password;

  /**
   * this method created for request for avalara
   */

  protected ResponseData process(final HttpEntity<?> requestEntity, final Class responseClazz)
      throws RestClientException, URISyntaxException {
    final ResponseEntity<SERVICERESPONSE> responeEntity = getBlRestTemplate().executeRequest(getServicePathUrl(),
        requestEntity, responseClazz);
    ResponseData responseData = new ResponseData();
    getBlResponseDetailsAndLoggingPopulator().populate(responeEntity , responseData);
    return responseData;
  }

  /**
   * this method created for HttpEntity
   */
  protected HttpEntity<SERVICEREQUEST> createHttpEntity(final SERVICEREQUEST pRequest)
  {
    return new HttpEntity<SERVICEREQUEST>(pRequest, addHeadersToServiceRequest());  // NOSONAR
  }

  /**
   * this method created for authentication
   */
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


  public Populator<ResponseEntity<SERVICERESPONSE>, ResponseData> getBlResponseDetailsAndLoggingPopulator() {
    return blResponseDetailsAndLoggingPopulator;
  }

  public void setBlResponseDetailsAndLoggingPopulator(
      Populator<ResponseEntity<SERVICERESPONSE>, ResponseData> blResponseDetailsAndLoggingPopulator) {
    this.blResponseDetailsAndLoggingPopulator = blResponseDetailsAndLoggingPopulator;
  }

  public Populator<SERVICERESPONSE, RESPONSE> getBlPayBillTaxResponsePopulator() {
    return blPayBillTaxResponsePopulator;
  }

  public void setBlPayBillTaxResponsePopulator(
      Populator<SERVICERESPONSE, RESPONSE> blPayBillTaxResponsePopulator) {
    this.blPayBillTaxResponsePopulator = blPayBillTaxResponsePopulator;
  }

  public Populator<BillingPojo, SERVICEREQUEST> getBillingRequestPopulator() {
    return billingRequestPopulator;
  }

  public void setBillingRequestPopulator(Populator<BillingPojo, SERVICEREQUEST> billingRequestPopulator) {
    this.billingRequestPopulator = billingRequestPopulator;
  }

}
