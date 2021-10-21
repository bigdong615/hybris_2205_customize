package com.bl.esp.dto.orderconfirmation;


/**
 * OrderConfirmationResponseWrapper pojo class to hold all order response details along with request object
 * and response object string.
 *
 *
 */
public class ESPEventResponseWrapper {
  private String eventInstanceId;
  private String requestString;
  private String responseString;

  public String getEventInstanceId() {
    return eventInstanceId;
  }

  public void setEventInstanceId(String eventInstanceId) {
    this.eventInstanceId = eventInstanceId;
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
