package com.bl.integration.fedex.shipment.pojo;

import com.fasterxml.jackson.annotation.JsonProperty;

public class V1RouteTrackingNumberCancellation {
	@JsonProperty("post") 
    public Post getPost() { 
		 return this.post; } 
    public void setPost(Post post) { 
		 this.post = post; } 
    Post post;
}
