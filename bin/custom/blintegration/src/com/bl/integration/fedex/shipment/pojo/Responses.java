package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Responses {
	@JsonProperty("500") 
    public Class500 get_500() { 
		 return this._500; } 
    public void set_500(Class500 _500) { 
		 this._500 = _500; } 
    Class500 _500;
    @JsonProperty("200") 
    public Class200 getClass200() { 
		 return this._200; } 
    public void set_200(Class200 _200) { 
		 this._200 = _200; } 
    Class200 _200;
    @JsonProperty("400") 
    public Class400 get_400() { 
		 return this._400; } 
    public void set_400(Class400 _400) { 
		 this._400 = _400; } 
    Class400 _400;
}
