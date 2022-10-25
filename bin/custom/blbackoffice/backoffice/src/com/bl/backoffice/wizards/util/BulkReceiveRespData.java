/**
 *
 */
package com.bl.backoffice.wizards.util;

import java.util.List;


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

	private String skuFirmwareVersion;

	private String firmwareVersion;

	private String functionalRatingValue;

	private String cosmeticRatingValue;

	private List<String> cosmeticRating;

	private List<String> testingStatus;

	private List<String> functionalRating;;

	private String testingStatusValue;

	private String orderNotes;

	/**
	 * @return the cosmeticRating
	 */
	public List<String> getCosmeticRating()
	{
		return cosmeticRating;
	}

	/**
	 * @return the functionalRating
	 */
	public List<String> getFunctionalRating()
	{
		return functionalRating;
	}

	/**
	 * @return the functionalRatingValue
	 */
	public String getFunctionalRatingValue()
	{
		return functionalRatingValue;
	}

	/**
	 * @param functionalRatingValue
	 *           the functionalRatingValue to set
	 */
	public void setFunctionalRatingValue(final String functionalRatingValue)
	{
		this.functionalRatingValue = functionalRatingValue;
	}

	/**
	 * @return the cosmeticRatingValue
	 */
	public String getCosmeticRatingValue()
	{
		return cosmeticRatingValue;
	}

	/**
	 * @param cosmeticRatingValue
	 *           the cosmeticRatingValue to set
	 */
	public void setCosmeticRatingValue(final String cosmeticRatingValue)
	{
		this.cosmeticRatingValue = cosmeticRatingValue;
	}

	/**
	 * @param cosmeticRating
	 *           the cosmeticRating to set
	 */
	public void setCosmeticRating(final List<String> cosmeticRating)
	{
		this.cosmeticRating = cosmeticRating;
	}

	/**
	 * @param functionalRating
	 *           the functionalRating to set
	 */
	public void setFunctionalRating(final List<String> functionalRating)
	{
		this.functionalRating = functionalRating;
	}


	/**
	 * @return the skuFirmwareVersion
	 */
	public String getSkuFirmwareVersion()
	{
		return skuFirmwareVersion;
	}

	/**
	 * @param skuFirmwareVersion
	 *           the skuFirmwareVersion to set
	 */
	public void setSkuFirmwareVersion(final String skuFirmwareVersion)
	{
		this.skuFirmwareVersion = skuFirmwareVersion;
	}

	/**
	 * @return the firmwareVersion
	 */
	public String getFirmwareVersion()
	{
		return firmwareVersion;
	}

	/**
	 * @param firmwareVersion
	 *           the firmwareVersion to set
	 */
	public void setFirmwareVersion(final String firmwareVersion)
	{
		this.firmwareVersion = firmwareVersion;
	}


	/**
	 * @return the testingStatus
	 */
	public List<String> getTestingStatus()
	{
		return testingStatus;
	}

	/**
	 * @param testingStatus
	 *           the testingStatus to set
	 */
	public void setTestingStatus(final List<String> testingStatus)
	{
		this.testingStatus = testingStatus;
	}

	/**
	 * @return the testingStatusValue
	 */
	public String getTestingStatusValue()
	{
		return testingStatusValue;
	}

	/**
	 * @param testingStatusValue
	 *           the testingStatusValue to set
	 */
	public void setTestingStatusValue(final String testingStatusValue)
	{
		this.testingStatusValue = testingStatusValue;
	}

	/**
	 * @return the barcode
	 */
	public String getBarcode()
	{
		return barcode;
	}

	/**
	 * @return the orderNotes
	 */
	public String getOrderNotes()
	{
		return orderNotes;
	}

	/**
	 * @param orderNotes
	 *           the orderNotes to set
	 */
	public void setOrderNotes(final String orderNotes)
	{
		this.orderNotes = orderNotes;
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
