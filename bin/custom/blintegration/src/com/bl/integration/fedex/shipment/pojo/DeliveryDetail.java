package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class DeliveryDetail{
    @JsonProperty("location") 
    public Location getLocation() { 
		 return this.location; } 
    public void setLocation(Location location) { 
		 this.location = location; } 
    Location location;
    @JsonProperty("instructions") 
    public String getInstructions() { 
		 return this.instructions; } 
    public void setInstructions(String instructions) { 
		 this.instructions = instructions; } 
    String instructions;
}
