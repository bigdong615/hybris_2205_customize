package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Weight{
    @JsonProperty("units") 
    public String getUnits() { 
		 return this.units; } 
    public void setUnits(String units) { 
		 this.units = units; } 
    String units;
    @JsonProperty("value") 
    public int getValue() { 
		 return this.value; } 
    public void setValue(int value) { 
		 this.value = value; } 
    int value;
}