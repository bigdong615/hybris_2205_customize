package com.braintree.converters;

import com.braintree.command.result.BrainTreeFindTransactionResult;
import com.braintree.converters.utils.BraintreeTransactionConverterUtils;
import com.braintree.hybris.data.BraintreeTransactionData;
import com.braintree.hybris.data.BraintreeTransactionEntryData;
import com.braintreegateway.ResourceCollection;
import com.braintreegateway.Transaction;
import de.hybris.platform.converters.impl.AbstractPopulatingConverter;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

import java.util.ArrayList;
import java.util.List;


public class BraintreeTransactionConverter extends
		AbstractPopulatingConverter<BrainTreeFindTransactionResult, BraintreeTransactionData>
{
	@Override
	public void populate(final BrainTreeFindTransactionResult brainTreeFindTransactionResult,
			final BraintreeTransactionData braintreeTransactionData) throws ConversionException
	{
		final ResourceCollection<Transaction> transactions = brainTreeFindTransactionResult.getTransactions();

		final List<BraintreeTransactionEntryData> transactionEntries = new ArrayList<BraintreeTransactionEntryData>();

		for (final Transaction transaction : transactions)
		{
			if (transaction != null)
			{
				final BraintreeTransactionEntryData braintreeTransactionEntryData = new BraintreeTransactionEntryData();
				braintreeTransactionEntryData.setId(transaction.getId());
				braintreeTransactionEntryData.setDate(BraintreeTransactionConverterUtils.formedDate(transaction));
				braintreeTransactionEntryData.setPaymentInfo(BraintreeTransactionConverterUtils.formedPaymentInfo(transaction));
				braintreeTransactionEntryData.setAmount(BraintreeTransactionConverterUtils.formedAmount(transaction));
				braintreeTransactionEntryData.setCurrencyIsoCode(transaction.getCurrencyIsoCode());
				braintreeTransactionEntryData.setTotal(transaction.getAmount());

				if (transaction.getCustomer() != null)
				{
					braintreeTransactionEntryData
							.setCustomer(BraintreeTransactionConverterUtils.formedName(transaction.getCustomer()));
				}
				if (transaction.getStatus() != null)
				{
					braintreeTransactionEntryData.setStatus(transaction.getStatus().name());
				}
				if (transaction.getType() != null)
				{
					braintreeTransactionEntryData.setType(transaction.getType().name());
				}
				if (transaction.getRiskData() != null)
				{
					braintreeTransactionEntryData.setRiskDecision(transaction.getRiskData().getDecision());
				}
				braintreeTransactionEntryData.setDetails(BraintreeTransactionConverterUtils.convertDetails(transaction));
				transactionEntries.add(braintreeTransactionEntryData);
			}
		}
		braintreeTransactionData.setTransactionEntries(transactionEntries);
	}

}
