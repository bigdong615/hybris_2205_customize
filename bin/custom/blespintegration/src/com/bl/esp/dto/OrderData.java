package com.bl.esp.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

import javax.annotation.Generated;





@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
		"order id",
		"old order id",
		"template",
		"subscriber id",
		"email address",
		"type",
		"replacement",
		"status",
		"date placed",
		"shipping method type",
		"shipping method",
		"shipping method text",
		"tracking info",
		"item cost",
		"damage waiver cost",
		"subtotal",
		"shipping amount",
		"tax amount",
		"discount amount",
		"total cost",
		"discount text",
		"expected shipping date",
		"arrival date",
		"return date",
		"actual return date",
		"rental duration",
		"customer name",
		"verification level",
		"payment type",
		"payment text",
		"extension total",
		"shipping info",
		"billing info",
		"order items info"
})
@Generated("jsonschema2pojo")
public class OrderData {
	@JsonProperty("order id")
	private String orderid;

	@JsonProperty("old order id")
	private String oldorderid;

	@JsonProperty("template")
	private String template;

	@JsonProperty("subscriber id")
	private String subscriberid;

	@JsonProperty("email address")
	private String emailaddress;

	@JsonProperty("type")
	private String type;

	@JsonProperty("replacement")
	private String replacement;

	@JsonProperty("status")
	private String status;

	@JsonProperty("dateplaced")
	private String dateplaced;

	@JsonProperty("shipping method type")
	private String shippingmethodtype;

	@JsonProperty("shipping method")
	private String shippingmethod;

	@JsonProperty("shipping method text")
	private String shippingmethodtext;

	@JsonProperty("tracking info")
	private String trackinginfo;

	@JsonProperty("item cost")
	private Double itemcost;

	@JsonProperty("damage waiver cost")
	private Double damagewaivercost;

	@JsonProperty("subtotal")
	private Double subtotal;

	@JsonProperty("shipping amount")
	private Double shippingamount;

	@JsonProperty("tax amount")
	private Double taxamount;

	@JsonProperty("discoun tamount")
	private Double discountamount;

	@JsonProperty("total cost")
	private Double totalcost;

	@JsonProperty("discount text")
	private String discounttext;

	@JsonProperty("expected shipping date")
	private String expectedshippingdate;

	@JsonProperty("arrival date")
	private String arrivaldate;

	@JsonProperty("return date")
	private String returndate;

	@JsonProperty("actual return date")
	private String actualreturndate;

	@JsonProperty("rental duration")
	private int rentalduration;

	@JsonProperty("customer name")
	private String customername;

	@JsonProperty("verification level")
	private int verificationlevel;

	@JsonProperty("payment type")
	private String paymenttype;

	@JsonProperty("payment text")
	private String paymenttext;

	@JsonProperty("extension total")
	private Double extensiontotal;

	@JsonProperty("shipping info")
	private String shippinginfo;

	@JsonProperty("billing info")
	private String billinginfo;

	@JsonProperty("order items info")
	private String orderitemsinfo;

	@JsonProperty("order id")
	public String getOrderid() {
		return orderid;
	}

	@JsonProperty("order id")
	public void setOrderid(String orderid) {
		this.orderid = orderid;
	}

	@JsonProperty("old order id")
	public String getOldorderid() {
		return oldorderid;
	}

	@JsonProperty("old order id")
	public void setOldorderid(String oldorderid) {
		this.oldorderid = oldorderid;
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

	@JsonProperty("type")
	public String getType() {
		return type;
	}

	@JsonProperty("type")
	public void setType(String type) {
		this.type = type;
	}

	@JsonProperty("replacement")
	public String getReplacement() {
		return replacement;
	}

	@JsonProperty("replacement")
	public void setReplacement(String replacement) {
		this.replacement = replacement;
	}

	@JsonProperty("status")
	public String getStatus() {
		return status;
	}

	@JsonProperty("status")
	public void setStatus(String status) {
		this.status = status;
	}

	@JsonProperty("date placed")
	public String getDateplaced() {
		return dateplaced;
	}

	@JsonProperty("date placed")
	public void setDateplaced(String dateplaced) {
		this.dateplaced = dateplaced;
	}

	@JsonProperty("shipping method type")
	public String getShippingmethodtype() {
		return shippingmethodtype;
	}

	@JsonProperty("shipping method type")
	public void setShippingmethodtype(String shippingmethodtype) {
		this.shippingmethodtype = shippingmethodtype;
	}

	@JsonProperty("shipping method")
	public String getShippingmethod() {
		return shippingmethod;
	}

	@JsonProperty("shippin gmethod")
	public void setShippingmethod(String shippingmethod) {
		this.shippingmethod = shippingmethod;
	}

	@JsonProperty("shipping method text")
	public String getShippingmethodtext() {
		return shippingmethodtext;
	}

	@JsonProperty("shipping method text")
	public void setShippingmethodtext(String shippingmethodtext) {
		this.shippingmethodtext = shippingmethodtext;
	}

	@JsonProperty("tracking info")
	public String getTrackinginfo() {
		return trackinginfo;
	}

	@JsonProperty("tracking info")
	public void setTrackinginfo(String trackinginfo) {
		this.trackinginfo = trackinginfo;
	}

	@JsonProperty("item cost")
	public Double getItemcost() {
		return itemcost;
	}

	@JsonProperty("item cost")
	public void setItemcost(Double itemcost) {
		this.itemcost = itemcost;
	}

	@JsonProperty("damage waiver cost")
	public Double getDamagewaivercost() {
		return damagewaivercost;
	}

	@JsonProperty("damage waiver cost")
	public void setDamagewaivercost(Double damagewaivercost) {
		this.damagewaivercost = damagewaivercost;
	}

	@JsonProperty("subtotal")
	public Double getSubtotal() {
		return subtotal;
	}

	@JsonProperty("subtotal")
	public void setSubtotal(Double subtotal) {
		this.subtotal = subtotal;
	}

	@JsonProperty("shipping amount")
	public Double getShippingamount() {
		return shippingamount;
	}

	@JsonProperty("shipping amount")
	public void setShippingamount(Double shippingamount) {
		this.shippingamount = shippingamount;
	}

	@JsonProperty("tax amount")
	public Double getTaxamount() {
		return taxamount;
	}

	@JsonProperty("tax amount")
	public void setTaxamount(Double taxamount) {
		this.taxamount = taxamount;
	}

	@JsonProperty("discount amount")
	public Double getDiscountamount() {
		return discountamount;
	}

	@JsonProperty("discount amount")
	public void setDiscountamount(Double discountamount) {
		this.discountamount = discountamount;
	}

	@JsonProperty("total cost")
	public Double getTotalcost() {
		return totalcost;
	}

	@JsonProperty("total cost")
	public void setTotalcost(Double totalcost) {
		this.totalcost = totalcost;
	}

	@JsonProperty("discount text")
	public String getDiscounttext() {
		return discounttext;
	}

	@JsonProperty("discount text")
	public void setDiscounttext(String discounttext) {
		this.discounttext = discounttext;
	}

	@JsonProperty("expected shipping date")
	public String getExpectedshippingdate() {
		return expectedshippingdate;
	}

	@JsonProperty("expected shipping date")
	public void setExpectedshippingdate(String expectedshippingdate) {
		this.expectedshippingdate = expectedshippingdate;
	}

	@JsonProperty("arrival date")
	public String getArrivaldate() {
		return arrivaldate;
	}

	@JsonProperty("arrival date")
	public void setArrivaldate(String arrivaldate) {
		this.arrivaldate = arrivaldate;
	}

	@JsonProperty("return date")
	public String getReturndate() {
		return returndate;
	}

	@JsonProperty("return date")
	public void setReturndate(String returndate) {
		this.returndate = returndate;
	}

	@JsonProperty("actual return date")
	public String getActualreturndate() {
		return actualreturndate;
	}

	@JsonProperty("actual return date")
	public void setActualreturndate(String actualreturndate) {
		this.actualreturndate = actualreturndate;
	}

	@JsonProperty("rental duration")
	public int getRentalduration() {
		return rentalduration;
	}

	@JsonProperty("rental duration")
	public void setRentalduration(int rentalduration) {
		this.rentalduration = rentalduration;
	}

	@JsonProperty("customer name")
	public String getCustomername() {
		return customername;
	}

	@JsonProperty("customer name")
	public void setCustomername(String customername) {
		this.customername = customername;
	}

	@JsonProperty("verification level")
	public int getVerificationlevel() {
		return verificationlevel;
	}

	@JsonProperty("verification level")
	public void setVerificationlevel(int verificationlevel) {
		this.verificationlevel = verificationlevel;
	}

	@JsonProperty("payment type")
	public String getPaymenttype() {
		return paymenttype;
	}

	@JsonProperty("payment type")
	public void setPaymenttype(String paymenttype) {
		this.paymenttype = paymenttype;
	}

	@JsonProperty("payment text")
	public String getPaymenttext() {
		return paymenttext;
	}

	@JsonProperty("payment text")
	public void setPaymenttext(String paymenttext) {
		this.paymenttext = paymenttext;
	}

	@JsonProperty("extension total")
	public Double getExtensiontotal() {
		return extensiontotal;
	}

	@JsonProperty("extension total")
	public void setExtensiontotal(Double extensiontotal) {
		this.extensiontotal = extensiontotal;
	}

	@JsonProperty("shipping info")
	public String getShippinginfo() {
		return shippinginfo;
	}

	@JsonProperty("shipping info")
	public void setShippinginfo(String shippinginfo) {
		this.shippinginfo = shippinginfo;
	}

	@JsonProperty("billing info")
	public String getBillinginfo() {
		return billinginfo;
	}

	@JsonProperty("billing info")
	public void setBillinginfo(String billinginfo) {
		this.billinginfo = billinginfo;
	}

	@JsonProperty("order items info")
	public String getOrderitemsinfo() {
		return orderitemsinfo;
	}

	@JsonProperty("order items info")
	public void setOrderitemsinfo(String orderitemsinfo) {
		this.orderitemsinfo = orderitemsinfo;
	}
}
