package com.bl.facades.populators;

import de.hybris.platform.commercefacades.BlItemsBillingChargeData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

import com.bl.core.model.BlItemsBillingChargeModel;
import com.bl.facades.process.email.impl.DefaultBlDomoFailureNotificationService;


public class BlItemsBillingChargePopulator implements Populator<BlItemsBillingChargeModel, BlItemsBillingChargeData>
{

	private DefaultBlDomoFailureNotificationService defaultBlDomoFailureNotificationService;

	@Override
	public void populate(final BlItemsBillingChargeModel source, final BlItemsBillingChargeData target) throws ConversionException
	{
		try
		{
			if (source.getBillChargeType() != null)
			{
				target.setBillChargeType(source.getBillChargeType().getCode());
			}
			target.setBillPaid(source.isBillPaid());
			if (source.getBillStatus() != null)
			{
				target.setBillStatus(source.getBillStatus().getCode());
			}
			target.setChargedAmount(source.getChargedAmount());
			target.setCode(source.getCode());
			target.setCreatedTS(source.getCreationtime());
			if (source.getCustomer() != null)
			{
				target.setCustomer(source.getCustomer().getName());
			}
			target.setModifiedTS(source.getModifiedtime());
			target.setOrderCode(source.getOrderCode());
			target.setSerialCode(source.getSerialCode());
			target.setTaxAmount(source.getTaxAmount());
			target.setUnPaidBillNotes(source.getUnPaidBillNotes());
			target.setUpdatedBillTime(source.getUpdatedBillTime());
			target.setPrimaryKey(source.getPk().toString());
		}
		catch (final Exception exception)
		{
			getDefaultBlDomoFailureNotificationService().send(exception.toString(), source.getPk().toString(),
					"BlItemsBillingCharge api");
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
