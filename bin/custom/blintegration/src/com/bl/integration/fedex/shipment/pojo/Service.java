package com.bl.integration.fedex.shipment.pojo;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Service{
    @JsonProperty("serviceType") 
    public String getServiceType() { 
		 return this.serviceType; } 
    public void setServiceType(String serviceType) { 
		 this.serviceType = serviceType; } 
    String serviceType;
    @JsonProperty("signatureService") 
    public String getSignatureService() { 
		 return this.signatureService; } 
    public void setSignatureService(String signatureService) { 
		 this.signatureService = signatureService; } 
    String signatureService;
    @JsonProperty("specialServices") 
    public List<Object> getSpecialServices() { 
		 return this.specialServices; } 
    public void setSpecialServices(List<Object> specialServices) { 
		 this.specialServices = specialServices; } 
    List<Object> specialServices;
    @JsonProperty("restrictions") 
    public Restrictions getRestrictions() { 
		 return this.restrictions; } 
    public void setRestrictions(Restrictions restrictions) { 
		 this.restrictions = restrictions; } 
    Restrictions restrictions;
}
