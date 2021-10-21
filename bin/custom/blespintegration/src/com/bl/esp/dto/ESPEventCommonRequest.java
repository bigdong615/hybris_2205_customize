package com.bl.esp.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import javax.annotation.Generated;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({

})
@Generated("jsonschema2pojo")
public class ESPEventCommonRequest {

	@JsonProperty("order id")
	private String orderid;

	@JsonProperty("template")
	private String template;

	@JsonProperty("subscriber id")
	private String subscriberid;

	@JsonProperty("email address")
	private String emailaddress;

	@JsonProperty("verification level")
	private int verificationlevel;

	@JsonProperty("order id")
	public String getOrderid() {
		return orderid;
	}

	@JsonProperty("order id")
	public void setOrderid(String orderid) {
		this.orderid = orderid;
	}

	@JsonProperty("template")
	public String getTemplate() {
		return template;
	}

	@JsonProperty("template")
	public void setTemplate(String template) {
		this.template = template;
	}

	@JsonProperty("subscriber id")
	public String getSubscriberid() {
		return subscriberid;
	}

	@JsonProperty("subscriber id")
	public void setSubscriberid(String subscriberid) {
		this.subscriberid = subscriberid;
	}

	@JsonProperty("email address")
	public String getEmailaddress() {
		return emailaddress;
	}

	@JsonProperty("email address")
	public void setEmailaddress(String emailaddress) {
		this.emailaddress = emailaddress;
	}

	@JsonProperty("verification level")
	public int getVerificationlevel() {
		return verificationlevel;
	}

	@JsonProperty("verification level")
	public void setVerificationlevel(int verificationlevel) {
		this.verificationlevel = verificationlevel;
	}
}
