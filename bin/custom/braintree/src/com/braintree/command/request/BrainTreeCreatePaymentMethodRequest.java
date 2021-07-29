/**
 *
 */
package com.braintree.command.request;

import de.hybris.platform.payment.dto.BillingInfo;


public class BrainTreeCreatePaymentMethodRequest extends BrainTreeAbstractRequest
{
	private String methodNonce;
	private String customerId;
	private String billingAddressId;
	private String cardholderName;
	private BillingInfo billingInfo;
	private String amount;
	private Boolean defaultCard; 
	
	public BrainTreeCreatePaymentMethodRequest(String merchantTransactionCode, String methodNonce, String customerId,
			String billingAddressId, String cardholderName, BillingInfo billingInfo, String amount,
			Boolean defaultCard) {
		super(merchantTransactionCode);
		this.methodNonce = methodNonce;
		this.customerId = customerId;
		this.billingAddressId = billingAddressId;
		this.cardholderName = cardholderName;
		this.billingInfo = billingInfo;
		this.amount = amount;
		this.defaultCard = defaultCard;
	}

	/**
	 * @return the defaultCard
	 */
	public Boolean getDefaultCard() {
		return defaultCard;
	}

	/**
	 * @param defaultCard the defaultCard to set
	 */
	public void setDefaultCard(Boolean defaultCard) {
		this.defaultCard = defaultCard;
	}

	protected BrainTreeCreatePaymentMethodRequest(final String merchantTransactionCode)
	{
		super(merchantTransactionCode);
	}

	public BrainTreeCreatePaymentMethodRequest(final String merchantTransactionCode, final String methodNonce,
			final String customerId)
	{
		super(merchantTransactionCode);
		this.methodNonce = methodNonce;
		this.customerId = customerId;
	}

	public BrainTreeCreatePaymentMethodRequest(final String merchantTransactionCode, final String methodNonce,
			final String customerId, final String billingAddressId, final BillingInfo billingInfo)
	{
		super(merchantTransactionCode);
		this.methodNonce = methodNonce;
		this.customerId = customerId;
		this.billingAddressId = billingAddressId;
		this.billingInfo = billingInfo;
	}

	public BrainTreeCreatePaymentMethodRequest(final String merchantTransactionCode, final String methodNonce,
			final String cardholderName, final String customerId, final String billingAddressId, final BillingInfo billingInfo)
	{
		super(merchantTransactionCode);
		this.methodNonce = methodNonce;
		this.customerId = customerId;
		this.cardholderName = cardholderName;
		this.billingAddressId = billingAddressId;
		this.billingInfo = billingInfo;
	}

	public BrainTreeCreatePaymentMethodRequest(final String merchantTransactionCode, final String methodNonce,
			final String cardholderName, final String customerId, final String billingAddressId, final BillingInfo billingInfo,
			final String amount)
	{
		super(merchantTransactionCode);
		this.methodNonce = methodNonce;
		this.customerId = customerId;
		this.cardholderName = cardholderName;
		this.billingAddressId = billingAddressId;
		this.billingInfo = billingInfo;
		this.amount = amount;
	}

	public String getMethodNonce()
	{
		return methodNonce;
	}

	public void setMethodNonce(final String methodNonce)
	{
		this.methodNonce = methodNonce;
	}

	public String getCustomerId()
	{
		return customerId;
	}

	public void setCustomerId(final String customerId)
	{
		this.customerId = customerId;
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
	 * @return the billingAddressId
	 */
	public String getBillingAddressId()
	{
		return billingAddressId;
	}

	/**
	 * @param billingAddressId
	 *           the billingAddressId to set
	 */
	public void setBillingAddressId(final String billingAddressId)
	{
		this.billingAddressId = billingAddressId;
	}

	/**
	 * @return the cardholderName
	 */
	public String getCardholderName()
	{
		return cardholderName;
	}

	/**
	 * @param cardholderName
	 *           the cardholderName to set
	 */
	public void setCardholderName(final String cardholderName)
	{
		this.cardholderName = cardholderName;
	}

	/**
	 * @return the billingInfo
	 */
	public BillingInfo getBillingInfo()
	{
		return billingInfo;
	}

	/**
	 * @param billingInfo
	 *           the billingInfo to set
	 */
	public void setBillingInfo(final BillingInfo billingInfo)
	{
		this.billingInfo = billingInfo;
	}

}
