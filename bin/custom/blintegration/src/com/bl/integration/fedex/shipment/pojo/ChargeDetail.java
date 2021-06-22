package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class ChargeDetail{
    @JsonProperty("name") 
    public String getName() { 
		 return this.name; } 
    public void setName(String name) { 
		 this.name = name; } 
    String name;
    @JsonProperty("currencyAmount") 
    public CurrencyAmount getCurrencyAmount() { 
		 return this.currencyAmount; } 
    public void setCurrencyAmount(CurrencyAmount currencyAmount) { 
		 this.currencyAmount = currencyAmount; } 
    CurrencyAmount currencyAmount;
}
