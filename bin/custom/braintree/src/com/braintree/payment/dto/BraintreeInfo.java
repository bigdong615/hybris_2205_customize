package com.braintree.payment.dto;

import de.hybris.platform.payment.dto.BillingInfo;


public class BraintreeInfo
{
	private String expirationYear;

	private Boolean liabilityShifted;

	private String cardType;

	private String deviceData;

	private String expirationMonth;

	private String nonce;

	private String imageSource;

	private String paymentProvider;

	private String paymentMethodToken;

	private String cardNumber;

	private String cardholderName;

	private String email;

	private boolean savePaymentInfo;

	private boolean shouldBeSaved;

	private boolean duplicatedPayment;

	private BillingInfo billingInfo;

	private String intent;

	private String amount;
	
	private Boolean isDefault;

    private String braintreeAddressId;

	private boolean isDepositPayment;

	private Double depositAmount;

	public BraintreeInfo()
	{
		this.duplicatedPayment = false;
	}

	/**
	 * @return the expirationYear
	 */
	public String getExpirationYear()
	{
		return expirationYear;
	}

	/**
	 * @param expirationYear the expirationYear to set
	 */
	public void setExpirationYear(final String expirationYear)
	{
		this.expirationYear = expirationYear;
	}

	/**
	 * @return the liabilityShifted
	 */
	public Boolean getLiabilityShifted()
	{
		return liabilityShifted;
	}

	/**
	 * @param liabilityShifted the liabilityShifted to set
	 */
	public void setLiabilityShifted(final Boolean liabilityShifted)
	{
		this.liabilityShifted = liabilityShifted;
	}

	/**
	 * @return the cardType
	 */
	public String getCardType()
	{
		return cardType;
	}

	/**
	 * @param cardType the cardType to set
	 */
	public void setCardType(final String cardType)
	{
		this.cardType = cardType;
	}

	/**
	 * @return the deviceData
	 */
	public String getDeviceData()
	{
		return deviceData;
	}

	/**
	 * @param deviceData the deviceData to set
	 */
	public void setDeviceData(final String deviceData)
	{
		this.deviceData = deviceData;
	}

	/**
	 * @return the expirationMonth
	 */
	public String getExpirationMonth()
	{
		return expirationMonth;
	}

	/**
	 * @param expirationMonth the expirationMonth to set
	 */
	public void setExpirationMonth(final String expirationMonth)
	{
		this.expirationMonth = expirationMonth;
	}

	/**
	 * @return the nonce
	 */
	public String getNonce()
	{
		return nonce;
	}

	/**
	 * @param nonce the nonce to set
	 */
	public void setNonce(final String nonce)
	{
		this.nonce = nonce;
	}

	/**
	 * @return the imageSource
	 */
	public String getImageSource()
	{
		return imageSource;
	}

	/**
	 * @param imageSource the imageSource to set
	 */
	public void setImageSource(final String imageSource)
	{
		this.imageSource = imageSource;
	}

	/**
	 * @return the paymentProvider
	 */
	public String getPaymentProvider()
	{
		return paymentProvider;
	}

	/**
	 * @param paymentProvider the paymentProvider to set
	 */
	public void setPaymentProvider(final String paymentProvider)
	{
		this.paymentProvider = paymentProvider;
	}

	/**
	 * @return the paymentMethodToken
	 */
	public String getPaymentMethodToken()
	{
		return paymentMethodToken;
	}

	/**
	 * @param paymentMethodToken the paymentMethodToken to set
	 */
	public void setPaymentMethodToken(final String paymentMethodToken)
	{
		this.paymentMethodToken = paymentMethodToken;
	}

	/**
	 * @return the cardNumber
	 */
	public String getCardNumber()
	{
		return cardNumber;
	}

	/**
	 * @param cardNumber the cardNumber to set
	 */
	public void setCardNumber(final String cardNumber)
	{
		this.cardNumber = cardNumber;
	}

	/**
	 * @return the cardholderName
	 */
	public String getCardholderName()
	{
		return cardholderName;
	}

	/**
	 * @param cardholderName the cardholderName to set
	 */
	public void setCardholderName(final String cardholderName)
	{
		this.cardholderName = cardholderName;
	}

	/**
	 * @return the email
	 */
	public String getEmail()
	{
		return email;
	}

	/**
	 * @param email the email to set
	 */
	public void setEmail(final String email)
	{
		this.email = email;
	}

	/**
	 * @return the savePaymentInfo
	 */
	public boolean isSavePaymentInfo()
	{
		return savePaymentInfo;
	}

	/**
	 * @param savePaymentInfo the savePaymentInfo to set
	 */
	public void setSavePaymentInfo(final boolean savePaymentInfo)
	{
		this.savePaymentInfo = savePaymentInfo;
	}

	/**
	 * @return the shouldBeSaved
	 */
	public boolean isShouldBeSaved()
	{
		return shouldBeSaved;
	}

	/**
	 * @param shouldBeSaved the shouldBeSaved to set
	 */
	public void setShouldBeSaved(boolean shouldBeSaved)
	{
		this.shouldBeSaved = shouldBeSaved;
	}

	/**
	 * @return the billingInfo
	 */
	public BillingInfo getBillingInfo()
	{
		return billingInfo;
	}

	/**
	 * @param billingInfo the billingInfo to set
	 */
	public void setBillingInfo(final BillingInfo billingInfo)
	{
		this.billingInfo = billingInfo;
	}

	public boolean isDuplicatedPayment()
	{
		return duplicatedPayment;
	}

	public void setDuplicatedPayment(boolean duplicatedPayment)
	{
		this.duplicatedPayment = duplicatedPayment;
	}

	public String getIntent()
	{
		return intent;
	}

	public void setIntent(String intent)
	{
		this.intent = intent;
	}

	public String getAmount()
	{
		return amount;
	}

	public void setAmount(String amount)
	{
		this.amount = amount;
	}

  /**
   * @return the braintreeAddressId
   */
  public String getBraintreeAddressId()
  {
    return braintreeAddressId;
  }

  /**
   * @param braintreeAddressId the braintreeAddressId to set
   */
  public void setBraintreeAddressId(String braintreeAddressId)
  {
    this.braintreeAddressId = braintreeAddressId;
  }
  
  /**
	 * @return the isDefault
	 */
	public Boolean getIsDefault() {
		return isDefault;
	}

	/**
	 * @param isDefault the isDefault to set
	 */
	public void setIsDefault(Boolean isDefault) {
		this.isDefault = isDefault;
	}


	public boolean isDepositPayment() {
		return isDepositPayment;
	}

	public void setDepositPayment(boolean depositPayment) {
		isDepositPayment = depositPayment;
	}

	public Double getDepositAmount() {
		return depositAmount;
	}

	public void setDepositAmount(Double depositAmount) {
		this.depositAmount = depositAmount;
	}

}
