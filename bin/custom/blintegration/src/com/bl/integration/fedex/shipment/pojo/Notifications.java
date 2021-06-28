package com.bl.integration.fedex.shipment.pojo;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Notifications{
    @JsonProperty("email") 
    public List<Email> getEmail() { 
		 return this.email; } 
    public void setEmail(List<Email> email) { 
		 this.email = email; } 
    List<Email> email;
    @JsonProperty("sms") 
    public List<Sm> getSms() { 
		 return this.sms; } 
    public void setSms(List<Sm> sms) { 
		 this.sms = sms; } 
    List<Sm> sms;
}