package com.braintree.command.request.beans;

import java.math.BigDecimal;


public class BrainTreeLineItemBean
{
	private String name;
	private BigDecimal quantity;
	private BigDecimal unitAmount;
	private String unitOfMeasure;
	private BigDecimal totalAmount;
	private BigDecimal taxAmount;
	private BigDecimal discountAmount;
	private String productCode;
	private String commodityCode;

	public String getName()
	{
		return name;
	}

	public void setName(String name)
	{
		this.name = name;
	}

	public BigDecimal getQuantity()
	{
		return quantity;
	}

	public void setQuantity(BigDecimal quantity)
	{
		this.quantity = quantity;
	}

	public BigDecimal getUnitAmount()
	{
		return unitAmount;
	}

	public void setUnitAmount(BigDecimal unitAmount)
	{
		this.unitAmount = unitAmount;
	}

	public String getUnitOfMeasure()
	{
		return unitOfMeasure;
	}

	public void setUnitOfMeasure(String unitOfMeasure)
	{
		this.unitOfMeasure = unitOfMeasure;
	}

	public BigDecimal getTotalAmount()
	{
		return totalAmount;
	}

	public void setTotalAmount(BigDecimal totalAmount)
	{
		this.totalAmount = totalAmount;
	}

	public BigDecimal getTaxAmount()
	{
		return taxAmount;
	}

	public void setTaxAmount(BigDecimal taxAmount)
	{
		this.taxAmount = taxAmount;
	}

	public BigDecimal getDiscountAmount()
	{
		return discountAmount;
	}

	public void setDiscountAmount(BigDecimal discountAmount)
	{
		this.discountAmount = discountAmount;
	}

	public String getProductCode()
	{
		return productCode;
	}

	public void setProductCode(String productCode)
	{
		this.productCode = productCode;
	}

	public String getCommodityCode()
	{
		return commodityCode;
	}

	public void setCommodityCode(String commodityCode)
	{
		this.commodityCode = commodityCode;
	}
}
