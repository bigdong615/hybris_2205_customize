package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Sm{
    @JsonProperty("recipientType") 
    public String getRecipientType() { 
		 return this.recipientType; } 
    public void setRecipientType(String recipientType) { 
		 this.recipientType = recipientType; } 
    String recipientType;
    @JsonProperty("notifyOnShipment") 
    public boolean getNotifyOnShipment() { 
		 return this.notifyOnShipment; } 
    public void setNotifyOnShipment(boolean notifyOnShipment) { 
		 this.notifyOnShipment = notifyOnShipment; } 
    boolean notifyOnShipment;
    @JsonProperty("notifyOnInTransit") 
    public boolean getNotifyOnInTransit() { 
		 return this.notifyOnInTransit; } 
    public void setNotifyOnInTransit(boolean notifyOnInTransit) { 
		 this.notifyOnInTransit = notifyOnInTransit; } 
    boolean notifyOnInTransit;
    @JsonProperty("notifyOnNextStop") 
    public boolean getNotifyOnNextStop() { 
		 return this.notifyOnNextStop; } 
    public void setNotifyOnNextStop(boolean notifyOnNextStop) { 
		 this.notifyOnNextStop = notifyOnNextStop; } 
    boolean notifyOnNextStop;
    @JsonProperty("notifyOnException") 
    public boolean getNotifyOnException() { 
		 return this.notifyOnException; } 
    public void setNotifyOnException(boolean notifyOnException) { 
		 this.notifyOnException = notifyOnException; } 
    boolean notifyOnException;
    @JsonProperty("notifyOnDelivery") 
    public boolean getNotifyOnDelivery() { 
		 return this.notifyOnDelivery; } 
    public void setNotifyOnDelivery(boolean notifyOnDelivery) { 
		 this.notifyOnDelivery = notifyOnDelivery; } 
    boolean notifyOnDelivery;
    @JsonProperty("locale") 
    public String getLocale() { 
		 return this.locale; } 
    public void setLocale(String locale) { 
		 this.locale = locale; } 
    String locale;
    @JsonProperty("phoneNumber") 
    public String getPhoneNumber() { 
		 return this.phoneNumber; } 
    public void setPhoneNumber(String phoneNumber) { 
		 this.phoneNumber = phoneNumber; } 
    String phoneNumber;
    @JsonProperty("recipientOptInTimestamp") 
    public long getRecipientOptInTimestamp() { 
		 return this.recipientOptInTimestamp; } 
    public void setRecipientOptInTimestamp(long recipientOptInTimestamp) { 
		 this.recipientOptInTimestamp = recipientOptInTimestamp; } 
    long recipientOptInTimestamp;
}
