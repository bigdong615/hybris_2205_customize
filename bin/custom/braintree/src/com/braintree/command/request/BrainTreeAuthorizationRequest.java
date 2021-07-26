package com.braintree.command.request;

import com.braintree.command.request.beans.BrainTreeLineItemBean;
import de.hybris.platform.payment.commands.request.AuthorizationRequest;
import de.hybris.platform.payment.dto.BillingInfo;
import de.hybris.platform.payment.dto.CardInfo;

import java.math.BigDecimal;
import java.util.Currency;
import java.util.List;
import java.util.Map;


public class BrainTreeAuthorizationRequest extends AuthorizationRequest
{

	private String deviceData;
	private String methodNonce;
	private String customerId;
	private String paymentType;
	private Boolean liabilityShifted;
	private String paymentMethodToken;
	private String venmoProfileId;
	private Boolean usePaymentMethodToken;
	private Boolean submitForSettlement;
	private Boolean threeDSecureConfiguration;
	private Boolean advancedFraudTools;
	private Boolean isSkip3dSecureLiabilityResult;
	private String creditCardStatementName;
	private String merchantAccountIdForCurrentSite;
	private String brainTreeChannel;
	private boolean storeInVault;
	private String brainTreeAddressId;
	private String cardholderName;
	private BillingInfo billingInfo;
	private String brainTreeBilligAddressId;
	private Map<String, String> customFields;
	//	Level 2 data
	private String purchaseOrderNumber;
	private Double taxAmountAuthorize;
	//	Level 3 data
	private Double shippingAmount;
	private Double discountAmount;
	private String shipsFromPostalCode;
	private String shippingPostalCode;
	private String shippingCountryCodeAlpha3;
	private List<BrainTreeLineItemBean> lineItems;
	public BrainTreeAuthorizationRequest(final String merchantTransactionCode, final CardInfo card, final Currency currency,
			final BigDecimal totalAmount, final BillingInfo shippingInfo)
	{
		super(merchantTransactionCode, card, currency, totalAmount, shippingInfo);
	}

	public Map<String, String> getCustomFields()
	{
		return customFields;
	}

	public void setCustomFields(Map<String, String> customFields)
	{
		this.customFields = customFields;
	}

	public String getBrainTreeAddressId()
	{
		return brainTreeAddressId;
	}

	public void setBrainTreeAddressId(final String brainTreeAddressId)
	{
		this.brainTreeAddressId = brainTreeAddressId;
	}

	public boolean isStoreInVault()
	{
		return storeInVault;
	}

	public void setStoreInVault(final boolean storeInVault)
	{
		this.storeInVault = storeInVault;
	}

	public String getDeviceData()
	{
		return deviceData;
	}

	public void setDeviceData(final String deviceData)
	{
		this.deviceData = deviceData;
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

	public String getVenmoProfileId()
	{
		return venmoProfileId;
	}

	public void setVenmoProfileId(String venmoProfileId)
	{
		this.venmoProfileId = venmoProfileId;
	}

	public String getPaymentType()
	{
		return paymentType;
	}

	public void setPaymentType(final String paymentType)
	{
		this.paymentType = paymentType;
	}

	public String getCreditCardStatementName()
	{
		return creditCardStatementName;
	}

	public void setCreditCardStatementName(final String creditCardStatementName)
	{
		this.creditCardStatementName = creditCardStatementName;
	}

	public String getMerchantAccountIdForCurrentSite()
	{
		return merchantAccountIdForCurrentSite;
	}

	public void setMerchantAccountIdForCurrentSite(final String merchantAccountIdForCurrentSite)
	{
		this.merchantAccountIdForCurrentSite = merchantAccountIdForCurrentSite;
	}

	public String getBrainTreeChannel()
	{
		return brainTreeChannel;
	}

	public void setBrainTreeChannel(final String brainTreeChannel)
	{
		this.brainTreeChannel = brainTreeChannel;
	}

	public Boolean getLiabilityShifted()
	{
		return liabilityShifted;
	}

	public void setLiabilityShifted(final Boolean liabilityShifted)
	{
		this.liabilityShifted = liabilityShifted;
	}

	public String getPaymentMethodToken()
	{
		return paymentMethodToken;
	}

	public void setPaymentMethodToken(final String paymentMethodToken)
	{
		this.paymentMethodToken = paymentMethodToken;
	}

	public Boolean getUsePaymentMethodToken()
	{
		return usePaymentMethodToken;
	}

	public void setUsePaymentMethodToken(final Boolean usePaymentMethodToken)
	{
		this.usePaymentMethodToken = usePaymentMethodToken;
	}

	public Boolean getSubmitForSettlement() {
		return submitForSettlement;
	}

	public void setSubmitForSettlement(Boolean submitForSettlement) {
		this.submitForSettlement = submitForSettlement;
	}

	public Boolean getThreeDSecureConfiguration()
	{
		return threeDSecureConfiguration;
	}

	public void setThreeDSecureConfiguration(final Boolean threeDSecureConfiguration)
	{
		this.threeDSecureConfiguration = threeDSecureConfiguration;
	}

	public Boolean getAdvancedFraudTools()
	{
		return advancedFraudTools;
	}

	public void setAdvancedFraudTools(final Boolean advancedFraudTools)
	{
		this.advancedFraudTools = advancedFraudTools;
	}

	public Boolean getIsSkip3dSecureLiabilityResult()
	{
		return isSkip3dSecureLiabilityResult;
	}

	public void setIsSkip3dSecureLiabilityResult(final Boolean isSkip3dSecureLiabilityResult)
	{
		this.isSkip3dSecureLiabilityResult = isSkip3dSecureLiabilityResult;
	}

	public String getCardholderName()
	{
		return cardholderName;
	}

	public void setCardholderName(final String cardholderName)
	{
		this.cardholderName = cardholderName;
	}

	public BillingInfo getBillingInfo()
	{
		return billingInfo;
	}

	public void setBillingInfo(final BillingInfo billingInfo)
	{
		this.billingInfo = billingInfo;
	}

	public String getBrainTreeBilligAddressId()
	{
		return brainTreeBilligAddressId;
	}

	public void setBrainTreeBilligAddressId(final String brainTreeBilligAddressId)
	{
		this.brainTreeBilligAddressId = brainTreeBilligAddressId;
	}

	public String getPurchaseOrderNumber()
	{
		return purchaseOrderNumber;
	}

	public void setPurchaseOrderNumber(String purchaseOrderNumber)
	{
		this.purchaseOrderNumber = purchaseOrderNumber;
	}

	public Double getTaxAmountAuthorize()
	{
		return taxAmountAuthorize;
	}

	public void setTaxAmountAuthorize(Double taxAmount)
	{
		this.taxAmountAuthorize = taxAmount;
	}

	public Double getShippingAmount()
	{
		return shippingAmount;
	}

	public void setShippingAmount(Double shippingAmount)
	{
		this.shippingAmount = shippingAmount;
	}

	public Double getDiscountAmount()
	{
		return discountAmount;
	}

	public void setDiscountAmount(Double discountAmount)
	{
		this.discountAmount = discountAmount;
	}

	public String getShipsFromPostalCode()
	{
		return shipsFromPostalCode;
	}

	public void setShipsFromPostalCode(String shipsFromPostalCode)
	{
		this.shipsFromPostalCode = shipsFromPostalCode;
	}

	public String getShippingPostalCode()
	{
		return shippingPostalCode;
	}

	public void setShippingPostalCode(String shippingPostalCode)
	{
		this.shippingPostalCode = shippingPostalCode;
	}

	public String getShippingCountryCodeAlpha3()
	{
		return shippingCountryCodeAlpha3;
	}

	public void setShippingCountryCodeAlpha3(String shippingCountryCodeAlpha3)
	{
		this.shippingCountryCodeAlpha3 = shippingCountryCodeAlpha3;
	}

	public List<BrainTreeLineItemBean> getLineItems()
	{
		return lineItems;
	}

	public void setLineItems(List<BrainTreeLineItemBean> lineItems)
	{
		this.lineItems = lineItems;
	}

}
