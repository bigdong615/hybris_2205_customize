package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Class500 {
	 @JsonProperty("description") 
	    public String getDescription() { 
			 return this.description; } 
	    public void setDescription(String description) { 
			 this.description = description; } 
	    String description;
}
