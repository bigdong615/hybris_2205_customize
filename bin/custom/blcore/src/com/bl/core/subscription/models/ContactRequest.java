package com.bl.core.subscription.models;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.annotation.Generated;
import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
		"contactKey",
		"attributeSets"
})
@Generated("jsonschema2pojo")
public class ContactRequest {

	@JsonProperty("contactKey")
	private String contactKey;
	@JsonProperty("attributeSets")
	private List<AttributeSet> attributeSets = null;
	@JsonIgnore
	private Map<String, Object> additionalProperties = new HashMap<String, Object>();

	@JsonProperty("contactKey")
	public String getContactKey() {
		return contactKey;
	}

	@JsonProperty("contactKey")
	public void setContactKey(String contactKey) {
		this.contactKey = contactKey;
	}

	@JsonProperty("attributeSets")
	public List<AttributeSet> getAttributeSets() {
		return attributeSets;
	}

	@JsonProperty("attributeSets")
	public void setAttributeSets(List<AttributeSet> attributeSets) {
		this.attributeSets = attributeSets;
	}

	@JsonAnyGetter
	public Map<String, Object> getAdditionalProperties() {
		return this.additionalProperties;
	}

	@JsonAnySetter
	public void setAdditionalProperty(String name, Object value) {
		this.additionalProperties.put(name, value);
	}

}
