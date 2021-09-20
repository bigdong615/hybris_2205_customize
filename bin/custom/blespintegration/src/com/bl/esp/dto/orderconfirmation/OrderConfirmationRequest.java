package com.bl.esp.dto.orderconfirmation;

import com.bl.esp.dto.OrderData;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

import javax.annotation.Generated;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({

})
@Generated("jsonschema2pojo")
public class OrderConfirmationRequest {

	@JsonProperty("contactKey")
	private String contactKey;

	@JsonProperty("eventDefinitionKey")
	private String eventDefinitionKey;

	@JsonProperty("data")
	private OrderData data = null;

	@JsonProperty("contactKey")
	public String getContactKey() {
		return contactKey;
	}

	@JsonProperty("contactKey")
	public void setContactKey(String contactKey) {
		this.contactKey = contactKey;
	}

	@JsonProperty("eventDefinitionKey")
	public String getEventDefinitionKey() {
		return eventDefinitionKey;
	}

	@JsonProperty("eventDefinitionKey")
	public void setEventDefinitionKey(String eventDefinitionKey) {
		this.eventDefinitionKey = eventDefinitionKey;
	}

	@JsonProperty("data")
	public OrderData getData() {
		return data;
	}

	@JsonProperty("data")
	public void setData(OrderData data) {
		this.data = data;
	}
}
