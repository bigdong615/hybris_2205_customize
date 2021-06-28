package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Item{
    @JsonProperty("name") 
    public String getName() { 
		 return this.name; } 
    public void setName(String name) { 
		 this.name = name; } 
    String name;
    @JsonProperty("description") 
    public String getDescription() { 
		 return this.description; } 
    public void setDescription(String description) { 
		 this.description = description; } 
    String description;
    @JsonProperty("quantity") 
    public int getQuantity() { 
		 return this.quantity; } 
    public void setQuantity(int quantity) { 
		 this.quantity = quantity; } 
    int quantity;
    @JsonProperty("sku") 
    public String getSku() { 
		 return this.sku; } 
    public void setSku(String sku) { 
		 this.sku = sku; } 
    String sku;
    @JsonProperty("displayImage") 
    public DisplayImage getDisplayImage() { 
		 return this.displayImage; } 
    public void setDisplayImage(DisplayImage displayImage) { 
		 this.displayImage = displayImage; } 
    DisplayImage displayImage;
}