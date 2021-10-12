package com.braintree.command.result;

public class BrainTreeCreatePaymentMethodResult extends BrainTreeAbstractResult
{
	private String paymentMethodToken;
	private String imageSource;
	private String cardType;
	private String cardNumber;
	private String cardholderName;
	private String expirationMonth;
	private String expirationYear;
	private String email;
	private Boolean isDefault;
	private String braintreeAddressId;
	private String cvvValidationCode;

	public String getPaymentMethodToken()
	{
		return paymentMethodToken;
	}

	public void setPaymentMethodToken(final String paymentMethodToken)
	{
		this.paymentMethodToken = paymentMethodToken;
	}

	public String getImageSource()
	{
		return imageSource;
	}

	public void setImageSource(final String imageSource)
	{
		this.imageSource = imageSource;
	}

	public String getCardType()
	{
		return cardType;
	}

	public void setCardType(final String cardType)
	{
		this.cardType = cardType;
	}

	public String getCardNumber()
	{
		return cardNumber;
	}

	public void setCardNumber(final String cardNumber)
	{
		this.cardNumber = cardNumber;
	}

	public String getCardholderName()
	{
		return cardholderName;
	}

	public void setCardholderName(final String cardholderName)
	{
		this.cardholderName = cardholderName;
	}

	public String getExpirationMonth()
	{
		return expirationMonth;
	}

	public void setExpirationMonth(final String expirationMonth)
	{
		this.expirationMonth = expirationMonth;
	}

	public String getExpirationYear()
	{
		return expirationYear;
	}

	public void setExpirationYear(final String expirationYear)
	{
		this.expirationYear = expirationYear;
	}

	public String getEmail()
	{
		return email;
	}

	public void setEmail(final String email)
	{
		this.email = email;
	}

	@Override
	public String toString()
	{
		return "BrainTreeCreatePaymentMethodResult [paymentMethodToken=" + paymentMethodToken + ", imageSource=" + imageSource
				+ ", cardType=" + cardType + ", cardNumber="
				+ cardNumber + ", cardholderName=" + cardholderName + ", expirationMonth=" + expirationMonth + ", expirationYear="
        + expirationYear + ", email=" + email + ", brainTreeAddressId=" + braintreeAddressId 
        + ", cvvValidationCode=" + cvvValidationCode+ "]";
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

  /**
   * @return the cvvValidationCode
   */
  public String getCvvValidationCode()
  {
    return cvvValidationCode;
  }

  /**
   * @param cvvValidationCode the cvvValidationCode to set
   */
  public void setCvvValidationCode(String cvvValidationCode)
  {
    this.cvvValidationCode = cvvValidationCode;
  }


}
