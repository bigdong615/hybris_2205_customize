/**
 *
 */
package com.bl.facades.populators;

import de.hybris.platform.converters.Populator;
import de.hybris.platform.ordermanagementfacades.payment.data.PaymentTransactionEntryData;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

import com.bl.facades.process.email.impl.DefaultBlDomoFailureNotificationService;


public class BlPaymentTransactionEntryPopulator implements Populator<PaymentTransactionEntryModel, PaymentTransactionEntryData>
{
	private DefaultBlDomoFailureNotificationService defaultBlDomoFailureNotificationService;

	@Override
	public void populate(final PaymentTransactionEntryModel source, final PaymentTransactionEntryData target)
			throws ConversionException
	{
		try
		{
			if (source != null && target != null)
			{
				target.setAmount(source.getAmount());
				if (source.getCurrency() != null)
				{
					target.setCurrencyIsocode(source.getCurrency().getIsocode());
				}
				target.setTime(source.getTime());
				target.setTransactionStatus(source.getTransactionStatus());
				target.setTransactionStatusDetails(source.getTransactionStatusDetails());
				target.setType(source.getType());
				target.setCreatedTS(source.getCreationtime());
				target.setModifiedTS(source.getModifiedtime());
				if (source.getConsignment() != null)
				{
					target.setConsignment(source.getConsignment().getCode());
				}
				target.setRequestToken(source.getRequestToken());
				target.setRequestId(source.getRequestId());
				target.setSubscriptionID(source.getSubscriptionID());
				target.setCode(source.getCode());
				target.setVersionID(source.getVersionID());
				if (source.getPaymentTransaction() != null)
				{
					target.setPaymentTransaction(source.getPaymentTransaction().getCode());
				}
				target.setRefundedamount(source.getRefundedAmount());
				//target.setSubmittedforsettlementamount(source.getSubmittedForSettlementAmount());
				target.setPrimaryKey(source.getPk().toString());
			}
		}
		catch (final Exception exception)
		{
			getDefaultBlDomoFailureNotificationService().send(exception.toString(), source.getPk().toString(),
					"PaymentTransaction api");
			exception.printStackTrace();
		}
	}

	/**
	 * @return the defaultBlDomoFailureNotificationService
	 */
	public DefaultBlDomoFailureNotificationService getDefaultBlDomoFailureNotificationService()
	{
		return defaultBlDomoFailureNotificationService;
	}

	/**
	 * @param defaultBlDomoFailureNotificationService
	 *           the defaultBlDomoFailureNotificationService to set
	 */
	public void setDefaultBlDomoFailureNotificationService(
			final DefaultBlDomoFailureNotificationService defaultBlDomoFailureNotificationService)
	{
		this.defaultBlDomoFailureNotificationService = defaultBlDomoFailureNotificationService;
	}
}
