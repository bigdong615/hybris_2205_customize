package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class ExternalReferences{
    @JsonProperty("poNumber") 
    public String getPoNumber() { 
		 return this.poNumber; } 
    public void setPoNumber(String poNumber) { 
		 this.poNumber = poNumber; } 
    String poNumber;
    @JsonProperty("deptNumber") 
    public String getDeptNumber() { 
		 return this.deptNumber; } 
    public void setDeptNumber(String deptNumber) { 
		 this.deptNumber = deptNumber; } 
    String deptNumber;
    @JsonProperty("rmaNumber") 
    public String getRmaNumber() { 
		 return this.rmaNumber; } 
    public void setRmaNumber(String rmaNumber) { 
		 this.rmaNumber = rmaNumber; } 
    String rmaNumber;
    @JsonProperty("invoiceNumber") 
    public String getInvoiceNumber() { 
		 return this.invoiceNumber; } 
    public void setInvoiceNumber(String invoiceNumber) { 
		 this.invoiceNumber = invoiceNumber; } 
    String invoiceNumber;
    @JsonProperty("url") 
    public String getUrl() { 
		 return this.url; } 
    public void setUrl(String url) { 
		 this.url = url; } 
    String url;
    @JsonProperty("other") 
    public String getOther() { 
		 return this.other; } 
    public void setOther(String other) { 
		 this.other = other; } 
    String other;
}
