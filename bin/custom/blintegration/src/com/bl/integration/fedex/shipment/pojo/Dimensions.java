package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Dimensions{
    @JsonProperty("length") 
    public int getLength() { 
		 return this.length; } 
    public void setLength(int length) { 
		 this.length = length; } 
    int length;
    @JsonProperty("width") 
    public int getWidth() { 
		 return this.width; } 
    public void setWidth(int width) { 
		 this.width = width; } 
    int width;
    @JsonProperty("height") 
    public int getHeight() { 
		 return this.height; } 
    public void setHeight(int height) { 
		 this.height = height; } 
    int height;
    @JsonProperty("units") 
    public String getUnits() { 
		 return this.units; } 
    public void setUnits(String units) { 
		 this.units = units; } 
    String units;
}