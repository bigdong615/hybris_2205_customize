package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class TotalDeclaredValue{
    @JsonProperty("currencyCode") 
    public String getCurrencyCode() { 
		 return this.currencyCode; } 
    public void setCurrencyCode(String currencyCode) { 
		 this.currencyCode = currencyCode; } 
    String currencyCode;
    @JsonProperty("amount") 
    public int getAmount() { 
		 return this.amount; } 
    public void setAmount(int amount) { 
		 this.amount = amount; } 
    int amount;
}
