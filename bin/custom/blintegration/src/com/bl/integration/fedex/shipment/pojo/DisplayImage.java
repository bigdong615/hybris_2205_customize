package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class DisplayImage{
    @JsonProperty("large") 
    public String getLarge() { 
		 return this.large; } 
    public void setLarge(String large) { 
		 this.large = large; } 
    String large;
    @JsonProperty("medium") 
    public String getMedium() { 
		 return this.medium; } 
    public void setMedium(String medium) { 
		 this.medium = medium; } 
    String medium;
    @JsonProperty("small") 
    public String getSmall() { 
		 return this.small; } 
    public void setSmall(String small) { 
		 this.small = small; } 
    String small;
}