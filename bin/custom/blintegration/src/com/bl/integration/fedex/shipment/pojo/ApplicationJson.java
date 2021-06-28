package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class ApplicationJson{
    @JsonProperty("example") 
    public FedExShipmentRequest getExample() { 
		 return this.example; } 
    public void setExample(FedExShipmentRequest example) { 
		 this.example = example; } 
    FedExShipmentRequest example;
}
