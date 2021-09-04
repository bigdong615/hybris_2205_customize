package com.bl.core.services.esp.models;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

import javax.annotation.Generated;


@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
		"eventInstanceId"
})
@Generated("jsonschema2pojo")
public class OrderConfirmationResponse {

	@JsonProperty("eventInstanceId")
	private String eventInstanceId;

	@JsonProperty("eventInstanceId")
	public String getEventInstanceId() {
		return eventInstanceId;
	}

	@JsonProperty("eventInstanceId")
	public void setEventInstanceId(String eventInstanceId) {
		this.eventInstanceId = eventInstanceId;
	}

}
