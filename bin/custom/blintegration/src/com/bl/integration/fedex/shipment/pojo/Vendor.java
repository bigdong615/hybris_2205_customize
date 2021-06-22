package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Vendor{
    @JsonProperty("vcid") 
    public String getVcid() { 
		 return this.vcid; } 
    public void setVcid(String vcid) { 
		 this.vcid = vcid; } 
    String vcid;
}
