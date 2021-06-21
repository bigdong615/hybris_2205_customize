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
		"operationStatus",
		"rowsAffected",
		"contactKey",
		"contactID",
		"contactTypeID",
		"isNewContactKey",
		"requestServiceMessageID",
		"responseDateTime",
		"hasErrors",
		"resultMessages",
		"serviceMessageID"
})
@Generated("jsonschema2pojo")
public class ContactResponse {

	@JsonProperty("operationStatus")
	private String operationStatus;
	@JsonProperty("rowsAffected")
	private Long rowsAffected;
	@JsonProperty("contactKey")
	private String contactKey;
	@JsonProperty("contactID")
	private Long contactID;
	@JsonProperty("contactTypeID")
	private Long contactTypeID;
	@JsonProperty("isNewContactKey")
	private Boolean isNewContactKey;
	@JsonProperty("requestServiceMessageID")
	private String requestServiceMessageID;
	@JsonProperty("responseDateTime")
	private String responseDateTime;
	@JsonProperty("hasErrors")
	private Boolean hasErrors;
	@JsonProperty("resultMessages")
	private List<Object> resultMessages = null;
	@JsonProperty("serviceMessageID")
	private String serviceMessageID;
	@JsonIgnore
	private Map<String, Object> additionalProperties = new HashMap<String, Object>();

	@JsonProperty("operationStatus")
	public String getOperationStatus() {
		return operationStatus;
	}

	@JsonProperty("operationStatus")
	public void setOperationStatus(String operationStatus) {
		this.operationStatus = operationStatus;
	}

	@JsonProperty("rowsAffected")
	public Long getRowsAffected() {
		return rowsAffected;
	}

	@JsonProperty("rowsAffected")
	public void setRowsAffected(Long rowsAffected) {
		this.rowsAffected = rowsAffected;
	}

	@JsonProperty("contactKey")
	public String getContactKey() {
		return contactKey;
	}

	@JsonProperty("contactKey")
	public void setContactKey(String contactKey) {
		this.contactKey = contactKey;
	}

	@JsonProperty("contactID")
	public Long getContactID() {
		return contactID;
	}

	@JsonProperty("contactID")
	public void setContactID(Long contactID) {
		this.contactID = contactID;
	}

	@JsonProperty("contactTypeID")
	public Long getContactTypeID() {
		return contactTypeID;
	}

	@JsonProperty("contactTypeID")
	public void setContactTypeID(Long contactTypeID) {
		this.contactTypeID = contactTypeID;
	}

	@JsonProperty("isNewContactKey")
	public Boolean getIsNewContactKey() {
		return isNewContactKey;
	}

	@JsonProperty("isNewContactKey")
	public void setIsNewContactKey(Boolean isNewContactKey) {
		this.isNewContactKey = isNewContactKey;
	}

	@JsonProperty("requestServiceMessageID")
	public String getRequestServiceMessageID() {
		return requestServiceMessageID;
	}

	@JsonProperty("requestServiceMessageID")
	public void setRequestServiceMessageID(String requestServiceMessageID) {
		this.requestServiceMessageID = requestServiceMessageID;
	}

	@JsonProperty("responseDateTime")
	public String getResponseDateTime() {
		return responseDateTime;
	}

	@JsonProperty("responseDateTime")
	public void setResponseDateTime(String responseDateTime) {
		this.responseDateTime = responseDateTime;
	}

	@JsonProperty("hasErrors")
	public Boolean getHasErrors() {
		return hasErrors;
	}

	@JsonProperty("hasErrors")
	public void setHasErrors(Boolean hasErrors) {
		this.hasErrors = hasErrors;
	}

	@JsonProperty("resultMessages")
	public List<Object> getResultMessages() {
		return resultMessages;
	}

	@JsonProperty("resultMessages")
	public void setResultMessages(List<Object> resultMessages) {
		this.resultMessages = resultMessages;
	}

	@JsonProperty("serviceMessageID")
	public String getServiceMessageID() {
		return serviceMessageID;
	}

	@JsonProperty("serviceMessageID")
	public void setServiceMessageID(String serviceMessageID) {
		this.serviceMessageID = serviceMessageID;
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
