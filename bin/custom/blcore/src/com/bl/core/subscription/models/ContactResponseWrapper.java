package com.bl.core.subscription.models;


/**
 * ContactResponseWrapper pojo class to hold all contact response details along with request object
 * and response object string.
 *
 * @author Sunil Sahu
 */
public class ContactResponseWrapper {

  private String operationStatus;
  private String contactKey;
  private String contactID;
  private String requestString;
  private String responseString;

  public String getOperationStatus() {
    return operationStatus;
  }

  public void setOperationStatus(final String operationStatus) {
    this.operationStatus = operationStatus;
  }

  public String getContactKey() {
    return contactKey;
  }

  public void setContactKey(final String contactKey) {
    this.contactKey = contactKey;
  }

  public String getContactID() {
    return contactID;
  }

  public void setContactID(final String contactID) {
    this.contactID = contactID;
  }

  public String getRequestString() {
    return requestString;
  }

  public void setRequestString(final String requestString) {
    this.requestString = requestString;
  }

  public String getResponseString() {
    return responseString;
  }

  public void setResponseString(final String responseString) {
    this.responseString = responseString;
  }

}
