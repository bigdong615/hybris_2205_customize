package com.bl.tax.billing;

import com.bl.core.enums.ItemBillingChargeTypeEnum;
import de.hybris.platform.core.model.order.OrderModel;

import java.io.Serializable;
import java.util.List;


/**
 * @author Jyoti Swamy
 * This POJO class is to get the billing related required information
 */
public class BillingPojo implements Serializable
{
	private OrderModel order;

	private String productName;

	private Double amount;

	private String serialNo;

	private Boolean isBillPaid;
	private ItemBillingChargeTypeEnum billingChargesReason;

	public OrderModel getOrder() {
		return order;
	}

	public void setOrder(OrderModel order) {
		this.order = order;
	}

	public String getProductName() {
		return productName;
	}

	public void setProductName(String productName) {
		this.productName = productName;
	}

	public Double getAmount() {
		return amount;
	}

	public void setAmount(Double amount) {
		this.amount = amount;
	}

	public String getSerialNo() {
		return serialNo;
	}

	public void setSerialNo(String serialNo) {
		this.serialNo = serialNo;
	}

	public Boolean getBillPaid() {
		return isBillPaid;
	}

	public void setBillPaid(Boolean billPaid) {
		isBillPaid = billPaid;
	}
	public ItemBillingChargeTypeEnum getBillingChargesReason() {
		return billingChargesReason;
	}

	public void setBillingChargesReason(ItemBillingChargeTypeEnum billingChargesReason) {
		this.billingChargesReason = billingChargesReason;
	}





}
