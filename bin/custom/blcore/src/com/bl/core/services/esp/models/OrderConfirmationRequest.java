package com.bl.core.services.esp.models;

import com.bl.core.subscription.models.AttributeSet;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import org.apache.poi.hpsf.Decimal;

import javax.annotation.Generated;
import java.util.List;

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
