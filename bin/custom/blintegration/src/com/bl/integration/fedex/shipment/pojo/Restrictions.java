package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Restrictions{
    @JsonProperty("noHAL") 
    public boolean getNoHAL() { 
		 return this.noHAL; } 
    public void setNoHAL(boolean noHAL) { 
		 this.noHAL = noHAL; } 
    boolean noHAL;
    @JsonProperty("noRecipientRedirect") 
    public boolean getNoRecipientRedirect() { 
		 return this.noRecipientRedirect; } 
    public void setNoRecipientRedirect(boolean noRecipientRedirect) { 
		 this.noRecipientRedirect = noRecipientRedirect; } 
    boolean noRecipientRedirect;
    @JsonProperty("noRemoteSignature") 
    public boolean getNoRemoteSignature() { 
		 return this.noRemoteSignature; } 
    public void setNoRemoteSignature(boolean noRemoteSignature) { 
		 this.noRemoteSignature = noRemoteSignature; } 
    boolean noRemoteSignature;
}
