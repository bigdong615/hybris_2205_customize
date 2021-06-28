package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Server{
    @JsonProperty("url") 
    public String getUrl() { 
		 return this.url; } 
    public void setUrl(String url) { 
		 this.url = url; } 
    String url;
}