package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class PickupDetail{
    @JsonProperty("readyTime") 
    public long getReadyTime() { 
		 return this.readyTime; } 
    public void setReadyTime(long readyTime) { 
		 this.readyTime = readyTime; } 
    long readyTime;
    @JsonProperty("localTimeZone") 
    public String getLocalTimeZone() { 
		 return this.localTimeZone; } 
    public void setLocalTimeZone(String localTimeZone) { 
		 this.localTimeZone = localTimeZone; } 
    String localTimeZone;
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
