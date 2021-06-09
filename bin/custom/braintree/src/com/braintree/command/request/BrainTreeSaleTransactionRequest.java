package com.braintree.command.request;

import com.braintree.enums.StoreInVault;
import de.hybris.platform.payment.dto.BillingInfo;
import de.hybris.platform.payment.dto.CardInfo;

import java.math.BigDecimal;
import java.util.Currency;
import java.util.Map;


public class BrainTreeSaleTransactionRequest extends BrainTreeAuthorizationRequest
{
	private BigDecimal taxAmount;
	private Map<String, String> customFields;
	private String customerFirstName;
	private String customerLastName;
	private String customerEmail;
	private StoreInVault storeInVault;
	private String requestId;


	public BrainTreeSaleTransactionRequest(final String merchantTransactionCode, final CardInfo card, final Currency currency,
			final BigDecimal totalAmount, final BillingInfo shippingInfo)
	{
		super(merchantTransactionCode, card, currency, totalAmount, shippingInfo);
	}

	public BigDecimal getTaxAmount()
	{
		return taxAmount;
	}

	public void setTaxAmount(final BigDecimal taxAmount)
	{
		this.taxAmount = taxAmount;
	}

	public Map<String, String> getCustomFields()
	{
		return customFields;
	}

	public void setCustomFields(final Map<String, String> customFields)
	{
		this.customFields = customFields;
	}

	public String getCustomerFirstName()
	{
		return customerFirstName;
	}

	public void setCustomerFirstName(final String customerFirstName)
	{
		this.customerFirstName = customerFirstName;
	}

	public String getCustomerLastName()
	{
		return customerLastName;
	}

	public void setCustomerLastName(final String customerLastName)
	{
		this.customerLastName = customerLastName;
	}

	public String getCustomerEmail()
	{
		return customerEmail;
	}

	public void setCustomerEmail(final String customerEmail)
	{
		this.customerEmail = customerEmail;
	}

	public StoreInVault getStoreInVault()
	{
		return storeInVault;
	}

	public void setStoreInVault(final StoreInVault storeInVault)
	{
		this.storeInVault = storeInVault;
	}

	public String getRequestId()
	{
		return requestId;
	}

	public void setRequestId(final String requestId)
	{
		this.requestId = requestId;
	}
}
