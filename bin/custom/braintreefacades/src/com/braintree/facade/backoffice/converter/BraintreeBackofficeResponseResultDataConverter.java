package com.braintree.facade.backoffice.converter;

import com.braintree.command.result.BrainTreeAbstractTransactionResult;
import com.braintree.converters.utils.BraintreeTransactionConverterUtils;
import com.braintree.hybris.data.BrainTreeResponseResultData;
import com.braintree.hybris.data.BraintreeTransactionEntryData;
import com.braintreegateway.Transaction;

import de.hybris.platform.converters.impl.AbstractPopulatingConverter;


public class BraintreeBackofficeResponseResultDataConverter extends
		AbstractPopulatingConverter<BrainTreeAbstractTransactionResult, BrainTreeResponseResultData>
{

	@Override
	public void populate(final BrainTreeAbstractTransactionResult source, final BrainTreeResponseResultData target)
	{
		target.setSuccess(source.isSuccess());
		target.setErrorMessage(source.getErrorMessage());
		target.setErrorCode(source.getErrorCode());
		target.setTransactionId(source.getTransactionId());
		convertTransaction(source, target);
	}

	private void convertTransaction(final BrainTreeAbstractTransactionResult source, final BrainTreeResponseResultData target)
	{
		final Transaction transaction = source.getTransaction();
		if (transaction != null)
		{
			final BraintreeTransactionEntryData braintreeTransactionEntryData = new BraintreeTransactionEntryData();
			braintreeTransactionEntryData.setId(transaction.getId());
			braintreeTransactionEntryData.setDate(BraintreeTransactionConverterUtils.formedDate(transaction));
			braintreeTransactionEntryData.setPaymentInfo(BraintreeTransactionConverterUtils.formedPaymentInfo(transaction));
			braintreeTransactionEntryData.setAmount(BraintreeTransactionConverterUtils.formedAmount(transaction));

			if (transaction.getCustomer() != null)
			{
				braintreeTransactionEntryData.setCustomer(BraintreeTransactionConverterUtils.formedName(transaction.getCustomer()));
			}
			if (transaction.getStatus() != null)
			{
				braintreeTransactionEntryData.setStatus(transaction.getStatus().name());
			}
			if (transaction.getType() != null)
			{
				braintreeTransactionEntryData.setType(transaction.getType().name());
			}
			braintreeTransactionEntryData.setDetails(BraintreeTransactionConverterUtils.convertDetails(transaction));
			target.setTransactionEntryData(braintreeTransactionEntryData);
		}
	}
}
