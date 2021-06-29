package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class SupportContact{
    @JsonProperty("phoneNumber") 
    public String getPhoneNumber() { 
		 return this.phoneNumber; } 
    public void setPhoneNumber(String phoneNumber) { 
		 this.phoneNumber = phoneNumber; } 
    String phoneNumber;
    @JsonProperty("emailAddress") 
    public String getEmailAddress() { 
		 return this.emailAddress; } 
    public void setEmailAddress(String emailAddress) { 
		 this.emailAddress = emailAddress; } 
    String emailAddress;
    @JsonProperty("url") 
    public String getUrl() { 
		 return this.url; } 
    public void setUrl(String url) { 
		 this.url = url; } 
    String url;
}
