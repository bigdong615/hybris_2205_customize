package com.bl.integration.fedex.shipment.pojo;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Package{
    @JsonProperty("items") 
    public List<Item> getItems() { 
		 return this.items; } 
    public void setItems(List<Item> items) { 
		 this.items = items; } 
    List<Item> items;
    @JsonProperty("weight") 
    public Weight getWeight() { 
		 return this.weight; } 
    public void setWeight(Weight weight) { 
		 this.weight = weight; } 
    Weight weight;
    @JsonProperty("dimensions") 
    public Dimensions getDimensions() { 
		 return this.dimensions; } 
    public void setDimensions(Dimensions dimensions) { 
		 this.dimensions = dimensions; } 
    Dimensions dimensions;
    @JsonProperty("trackingNumber") 
    public String getTrackingNumber() { 
		 return this.trackingNumber; } 
    public void setTrackingNumber(String trackingNumber) { 
		 this.trackingNumber = trackingNumber; } 
    String trackingNumber;
}
