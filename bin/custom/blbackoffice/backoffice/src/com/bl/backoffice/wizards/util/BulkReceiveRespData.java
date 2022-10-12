/**
 *
 */
package com.bl.backoffice.wizards.util;

/**
 * @author ravinder
 *
 */
public class BulkReceiveRespData
{
	private String serialProductId;

	private String serialProductName;

	private String productType;

	private String orderNumber;

	private String mainProductId;

	private String barcode;


	/**
	 * @return the barcode
	 */
	public String getBarcode()
	{
		return barcode;
	}

	/**
	 * @param barcode
	 *           the barcode to set
	 */
	public void setBarcode(final String barcode)
	{
		this.barcode = barcode;
	}

	/**
	 * @return the serialProductId
	 */
	public String getSerialProductId()
	{
		return serialProductId;
	}

	/**
	 * @param serialProductId
	 *           the serialProductId to set
	 */
	public void setSerialProductId(final String serialProductId)
	{
		this.serialProductId = serialProductId;
	}

	/**
	 * @return the serialProductName
	 */
	public String getSerialProductName()
	{
		return serialProductName;
	}

	/**
	 * @param serialProductName
	 *           the serialProductName to set
	 */
	public void setSerialProductName(final String serialProductName)
	{
		this.serialProductName = serialProductName;
	}

	/**
	 * @return the productType
	 */
	public String getProductType()
	{
		return productType;
	}

	/**
	 * @param productType
	 *           the productType to set
	 */
	public void setProductType(final String productType)
	{
		this.productType = productType;
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
	 * @return the mainProductId
	 */
	public String getMainProductId()
	{
		return mainProductId;
	}

	/**
	 * @param mainProductId
	 *           the mainProductId to set
	 */
	public void setMainProductId(final String mainProductId)
	{
		this.mainProductId = mainProductId;
	}



}
