package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Content{
    @JsonProperty("application/json") 
    public ApplicationJson getApplicationJson() { 
		 return this.applicationJson; } 
    public void setApplicationJson(ApplicationJson applicationJson) { 
		 this.applicationJson = applicationJson; } 
    ApplicationJson applicationJson;
}