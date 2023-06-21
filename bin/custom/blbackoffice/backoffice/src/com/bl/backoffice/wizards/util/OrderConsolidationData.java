/**
 *
 */
package com.bl.backoffice.wizards.util;

/**
 * @author Admin
 *
 */
public class OrderConsolidationData
{
	private String productName;
	private String location;
	private String parentLocation;
	private String barCode;
	private String warehouse;
	private String orderNumber;
	private String rentalEndDate;
	private String shippingMethod;

	/**
	 * @return the productName
	 */
	public String getProductName()
	{
		return productName;
	}

	/**
	 * @param productName
	 *           the productName to set
	 */
	public void setProductName(final String productName)
	{
		this.productName = productName;
	}

	/**
	 * @return the location
	 */
	public String getLocation()
	{
		return location;
	}

	/**
	 * @param location
	 *           the location to set
	 */
	public void setLocation(final String location)
	{
		this.location = location;
	}

	/**
	 * @return the parentLocation
	 */
	public String getParentLocation()
	{
		return parentLocation;
	}

	/**
	 * @param parentLocation
	 *           the parentLocation to set
	 */
	public void setParentLocation(final String parentLocation)
	{
		this.parentLocation = parentLocation;
	}

	/**
	 * @return the barCode
	 */
	public String getBarCode()
	{
		return barCode;
	}

	/**
	 * @param barCode
	 *           the barCode to set
	 */
	public void setBarCode(final String barCode)
	{
		this.barCode = barCode;
	}

	/**
	 * @return the orderNumber
	 */
	public String getOrderNumber()
	{
		return orderNumber;
	}

	/**
	 * @param orderNumber
	 *           the orderNumber to set
	 */
	public void setOrderNumber(final String orderNumber)
	{
		this.orderNumber = orderNumber;
	}

	/**
	 * @return the shippingMethod
	 */
	public String getShippingMethod()
	{
		return shippingMethod;
	}

	/**
	 * @param shippingMethod
	 *           the shippingMethod to set
	 */
	public void setShippingMethod(final String shippingMethod)
	{
		this.shippingMethod = shippingMethod;
	}


	public String getWarehouse() {
		return warehouse;
	}

	public void setWarehouse(String warehouse) {
		this.warehouse = warehouse;
	}

	public String getRentalEndDate() {
		return rentalEndDate;
	}

	public void setRentalEndDate(String rentalEndDate) {
		this.rentalEndDate = rentalEndDate;
	}
}
