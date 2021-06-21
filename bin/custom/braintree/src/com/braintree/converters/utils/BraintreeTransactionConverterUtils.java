package com.braintree.converters.utils;

import com.braintree.constants.BraintreeConstants;
import com.braintree.hybris.data.BraintreeTransactionEntryCustomerData;
import com.braintree.hybris.data.BraintreeTransactionEntryDetailData;
import com.braintree.hybris.data.BraintreeTransactionEntryPayPalData;
import com.braintree.hybris.data.BraintreeTransactionEntryPaymentData;
import com.braintree.hybris.data.BraintreeTransactionOriginEntryData;
import com.braintreegateway.CreditCard;
import com.braintreegateway.Customer;
import com.braintreegateway.PayPalDetails;
import com.braintreegateway.Transaction;
import de.hybris.platform.jalo.JaloSession;
import org.apache.commons.lang.StringUtils;

import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Locale;


public class BraintreeTransactionConverterUtils
{
	public static final String TRANSACTION_REFUND_SIGN = "yes";

	public static BraintreeTransactionEntryDetailData convertDetails(final Transaction transaction)
	{
		final BraintreeTransactionEntryDetailData data = new BraintreeTransactionEntryDetailData();
		data.setCustomerInfo(convertCustomerInfo(transaction));
		data.setPaymentInfo(convertPaymentInfo(transaction));
		data.setPaymentPayPalInfo(convertPaymentPayPalInfo(transaction));
		data.setTransactionInfo(convertOriginalTransactionInfo(transaction));
		return data;
	}

	static BraintreeTransactionEntryCustomerData convertCustomerInfo(final Transaction transaction)
	{
		final BraintreeTransactionEntryCustomerData customerData = new BraintreeTransactionEntryCustomerData();
		final Customer customer = transaction.getCustomer();
		if (customer != null)
		{
			customerData.setCustomerID(customer.getId());
			customerData.setName(formedName(customer));
			customerData.setEmail(customer.getEmail());
		}
		return customerData;
	}

	private static BraintreeTransactionEntryPaymentData convertPaymentInfo(final Transaction transaction)
	{
		final BraintreeTransactionEntryPaymentData paymentData = new BraintreeTransactionEntryPaymentData();
		if (BraintreeConstants.CREDIT_CARD_PAYMENT_INSTRUMENT_TYPE.equals(transaction.getPaymentInstrumentType()))
		{
			final CreditCard creditCard = transaction.getCreditCard();
			if (creditCard != null)
			{
				paymentData.setCardholderName(creditCard.getCardholderName());
				paymentData.setCardType(creditCard.getCardType());
				paymentData.setCreditCardNumber(String.format("%s******%s", creditCard.getBin(), creditCard.getLast4()));
				paymentData.setExpirationDate(creditCard.getExpirationDate());
				paymentData.setPaymentType(BraintreeConstants.CREDIT_CARD_PAYMENT_TYPE);
				paymentData.setToken(creditCard.getToken());
				paymentData.setUniqueNumberIdentifier(creditCard.getUniqueNumberIdentifier());
			}
			return paymentData;
		}
		return null;
	}

	private static BraintreeTransactionEntryPayPalData convertPaymentPayPalInfo(final Transaction transaction)
	{
		final BraintreeTransactionEntryPayPalData payPalData = new BraintreeTransactionEntryPayPalData();
		if (BraintreeConstants.PAYPAL_PAYMENT_INSTRUMENT_TYPE.equals(transaction.getPaymentInstrumentType()))
		{
			final PayPalDetails payPalDetails = transaction.getPayPalDetails();
			if (payPalDetails != null)
			{
				payPalData.setToken(payPalDetails.getToken());
				payPalData.setAuthorizationUniqueTransactionID(payPalDetails.getAuthorizationId());
				payPalData.setPayeeEmail(payPalDetails.getPayeeEmail());
				payPalData.setPayerEmail(payPalDetails.getPayerEmail());
				payPalData.setPayerFirstName(payPalDetails.getPayerFirstName());
				payPalData.setPayerLastName(payPalDetails.getPayerLastName());
				payPalData.setPayerID(payPalDetails.getPayerId());
				payPalData.setPaymentID(payPalDetails.getPaymentId());
				payPalData.setPayPalCaptureID(payPalDetails.getCaptureId());
				payPalData.setPayPalCustomField(payPalDetails.getCustomField());
				payPalData.setPayPalDebugID(payPalDetails.getDebugId());
				payPalData.setPayPalRefundID(payPalDetails.getRefundId());
				payPalData.setPayPalSellerProtectionStatus(payPalDetails.getSellerProtectionStatus());
				payPalData.setPaymentType(BraintreeConstants.PAYPAL_PAYMENT_TYPE_NAME);
			}
			return payPalData;
		}
		return null;
	}

	private static BraintreeTransactionOriginEntryData convertOriginalTransactionInfo(final Transaction transaction)
	{
		final BraintreeTransactionOriginEntryData entryData = new BraintreeTransactionOriginEntryData();
		//TODO: find merchant, for now - just EMPTY
		entryData.setMerchant(StringUtils.EMPTY);
		entryData.setMerchantAccount(transaction.getMerchantAccountId());
		entryData.setProcessorAuthorizationCode(transaction.getProcessorAuthorizationCode());
		entryData.setSettlementBatch(transaction.getSettlementBatchId());
		entryData.setStatus(transaction.getStatus().name());
		entryData.setCvvResponse(transaction.getCvvResponseCode());
		entryData.setAmount(formedAmount(transaction));
		entryData.setTransactionDate(formedDate(transaction));
		if (StringUtils.isNotBlank(transaction.getAvsErrorResponseCode()))
		{
			entryData.setAvsResponse(transaction.getAvsErrorResponseCode());
		}
		else
		{
			entryData.setAvsResponse(String.format("%s %s", transaction.getAvsPostalCodeResponseCode(),
					transaction.getAvsStreetAddressResponseCode()));
		}
		if (transaction.getRefundedTransactionId() != null)
		{
			entryData.setRefund(TRANSACTION_REFUND_SIGN);
		}
		if (transaction.getRiskData() != null)
		{
			entryData.setRiskDecision(transaction.getRiskData().getDecision());
		}
		return entryData;
	}

	public static String formedName(final Customer customer)
	{
		final String firstName = customer.getFirstName();
		final String lastName = customer.getLastName();
		if (StringUtils.isNotBlank(firstName))
		{
			if (StringUtils.isNotBlank(lastName))
			{
				return String.format("%s %s", firstName, lastName);
			}
			return firstName;
		}
		return StringUtils.EMPTY;
	}

	public static String formedPaymentInfo(final Transaction transaction)
	{
		final String paymentInstrumentType = transaction.getPaymentInstrumentType();

		if (BraintreeConstants.PAYPAL_PAYMENT_INSTRUMENT_TYPE.equals(paymentInstrumentType))
		{
			final PayPalDetails payPalDetails = transaction.getPayPalDetails();
			if (payPalDetails != null)
			{
				return payPalDetails.getPayerEmail();
			}
		}
		if (BraintreeConstants.CREDIT_CARD_PAYMENT_INSTRUMENT_TYPE.equals(paymentInstrumentType))
		{
			final CreditCard creditCard = transaction.getCreditCard();
			if (creditCard != null)
			{
				return String.format("%s******%s", creditCard.getBin(), creditCard.getLast4());
			}
		}
		return StringUtils.EMPTY;
	}

	public static String formedAmount(final Transaction transaction)
	{
		final String currencyIsoCode = transaction.getCurrencyIsoCode();
		final BigDecimal amount = transaction.getAmount();
		return String.format("%s %s", amount, currencyIsoCode);
	}

	public static String formedDate(final Transaction transaction)
	{
		final Calendar createdAt = transaction.getCreatedAt();
		final Locale userLocale = JaloSession.getCurrentSession().getSessionContext().getLocale();
		final DateFormat dateFormat = new SimpleDateFormat(BraintreeConstants.TRANSACTION_SEARCH_DATE_FORMAT, userLocale);
		return dateFormat.format(createdAt.getTime());
	}
}
