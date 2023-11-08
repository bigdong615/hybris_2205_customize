/**
 *
 */
package com.bl.facades.populators;

import de.hybris.platform.converters.Populator;
import de.hybris.platform.ordermanagementfacades.payment.data.PaymentTransactionData;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

import com.bl.facades.process.email.impl.DefaultBlDomoFailureNotificationService;


public class BlPaymentTransactionPopulator implements Populator<PaymentTransactionModel, PaymentTransactionData>
{
	private DefaultBlDomoFailureNotificationService defaultBlDomoFailureNotificationService;

	@Override
	public void populate(final PaymentTransactionModel source, final PaymentTransactionData target) throws ConversionException
	{
		try
		{
			target.setCreatedTS(source.getCreationtime());
			target.setModifiedTS(source.getModifiedtime());
			target.setCode(source.getCode());
			target.setRequestId(source.getRequestId());
			target.setRequestToken(source.getRequestToken());
			target.setPaymentProvider(source.getPaymentProvider());
			target.setPlannedAmount(source.getPlannedAmount());
			if (source.getCurrency() != null)
			{
				target.setCurrencyIsocode(source.getCurrency().getIsocode());
			}
			target.setVersionID(source.getVersionID());
			if (source.getOrder() != null)
			{
				target.setOrder(source.getOrder().getCode());
			}
			if (source.getTransactionType() != null)
			{
				target.setTransactionType(source.getTransactionType().getCode());
			}
			//target.setLegacytransaction(source.);
			if (source.getInfo() != null)
			{
				target.setPaymentInfoCode(source.getInfo().getCode());
			}
			target.setPrimaryKey(source.getPk().toString());
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
