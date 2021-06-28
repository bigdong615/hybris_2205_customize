package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class RequestBody{
    @JsonProperty("description") 
    public String getDescription() { 
		 return this.description; } 
    public void setDescription(String description) { 
		 this.description = description; } 
    String description;
    @JsonProperty("content") 
    public Content getContent() { 
		 return this.content; } 
    public void setContent(Content content) { 
		 this.content = content; } 
    Content content;
}
