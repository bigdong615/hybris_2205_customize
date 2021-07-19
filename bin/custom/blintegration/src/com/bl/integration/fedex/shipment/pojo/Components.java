package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Components{
    @JsonProperty("securitySchemes") 
    public SecuritySchemes getSecuritySchemes() { 
		 return this.securitySchemes; } 
    public void setSecuritySchemes(SecuritySchemes securitySchemes) { 
		 this.securitySchemes = securitySchemes; } 
    SecuritySchemes securitySchemes;
}
