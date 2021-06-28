package com.bl.integration.fedex.shipment.pojo;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Address{
    @JsonProperty("streetLines") 
    public List<String> getStreetLines() { 
		 return this.streetLines; } 
    public void setStreetLines(List<String> streetLines) { 
		 this.streetLines = streetLines; } 
    List<String> streetLines;
    @JsonProperty("city") 
    public String getCity() { 
		 return this.city; } 
    public void setCity(String city) { 
		 this.city = city; } 
    String city;
    @JsonProperty("stateOrProvinceCode") 
    public String getStateOrProvinceCode() { 
		 return this.stateOrProvinceCode; } 
    public void setStateOrProvinceCode(String stateOrProvinceCode) { 
		 this.stateOrProvinceCode = stateOrProvinceCode; } 
    String stateOrProvinceCode;
    @JsonProperty("countryCode") 
    public String getCountryCode() { 
		 return this.countryCode; } 
    public void setCountryCode(String countryCode) { 
		 this.countryCode = countryCode; } 
    String countryCode;
    @JsonProperty("county") 
    public Object getCounty() { 
		 return this.county; } 
    public void setCounty(Object county) { 
		 this.county = county; } 
    Object county;
    @JsonProperty("postalCode") 
    public String getPostalCode() { 
		 return this.postalCode; } 
    public void setPostalCode(String postalCode) { 
		 this.postalCode = postalCode; } 
    String postalCode;
}