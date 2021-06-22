package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Contact{
    @JsonProperty("personName") 
    public String getPersonName() { 
		 return this.personName; } 
    public void setPersonName(String personName) { 
		 this.personName = personName; } 
    String personName;
    @JsonProperty("companyName") 
    public String getCompanyName() { 
		 return this.companyName; } 
    public void setCompanyName(String companyName) { 
		 this.companyName = companyName; } 
    String companyName;
    @JsonProperty("phoneNumber") 
    public String getPhoneNumber() { 
		 return this.phoneNumber; } 
    public void setPhoneNumber(String phoneNumber) { 
		 this.phoneNumber = phoneNumber; } 
    String phoneNumber;
    @JsonProperty("phoneExtension") 
    public String getPhoneExtension() { 
		 return this.phoneExtension; } 
    public void setPhoneExtension(String phoneExtension) { 
		 this.phoneExtension = phoneExtension; } 
    String phoneExtension;
    @JsonProperty("emailAddress") 
    public String getEmailAddress() { 
		 return this.emailAddress; } 
    public void setEmailAddress(String emailAddress) { 
		 this.emailAddress = emailAddress; } 
    String emailAddress;
}