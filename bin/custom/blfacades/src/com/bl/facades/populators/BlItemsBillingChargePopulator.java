package com.bl.facades.populators;

import de.hybris.platform.commercefacades.BlItemsBillingChargeData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

import com.bl.core.model.BlItemsBillingChargeModel;


public class BlItemsBillingChargePopulator implements Populator<BlItemsBillingChargeModel, BlItemsBillingChargeData>
{
	@Override
	public void populate(final BlItemsBillingChargeModel source, final BlItemsBillingChargeData target) throws ConversionException
	{
		target.setBillChargeType(source.getBillChargeType().getCode());
		target.setBillPaid(source.isBillPaid());
		target.setBillStatus(source.getBillStatus().getCode());
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
	}

}
