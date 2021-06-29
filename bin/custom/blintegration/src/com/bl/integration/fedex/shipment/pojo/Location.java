package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Location{
    @JsonProperty("address") 
    public Address getAddress() { 
		 return this.address; } 
    public void setAddress(Address address) { 
		 this.address = address; } 
    Address address;
    @JsonProperty("residential") 
    public boolean getResidential() { 
		 return this.residential; } 
    public void setResidential(boolean residential) { 
		 this.residential = residential; } 
    boolean residential;
    @JsonProperty("hoursOfOperationStart") 
    public long getHoursOfOperationStart() { 
		 return this.hoursOfOperationStart; } 
    public void setHoursOfOperationStart(long hoursOfOperationStart) { 
		 this.hoursOfOperationStart = hoursOfOperationStart; } 
    long hoursOfOperationStart;
    @JsonProperty("hoursOfOperationEnd") 
    public long getHoursOfOperationEnd() { 
		 return this.hoursOfOperationEnd; } 
    public void setHoursOfOperationEnd(long hoursOfOperationEnd) { 
		 this.hoursOfOperationEnd = hoursOfOperationEnd; } 
    long hoursOfOperationEnd;
}
