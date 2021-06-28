package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Recipient{
    @JsonProperty("displayImage") 
    public DisplayImage getDisplayImage() { 
		 return this.displayImage; } 
    public void setDisplayImage(DisplayImage displayImage) { 
		 this.displayImage = displayImage; } 
    DisplayImage displayImage;
    @JsonProperty("displayName") 
    public String getDisplayName() { 
		 return this.displayName; } 
    public void setDisplayName(String displayName) { 
		 this.displayName = displayName; } 
    String displayName;
    @JsonProperty("contact") 
    public Contact getContact() { 
		 return this.contact; } 
    public void setContact(Contact contact) { 
		 this.contact = contact; } 
    Contact contact;
}
