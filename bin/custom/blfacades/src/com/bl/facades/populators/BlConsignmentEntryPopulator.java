/**
 *
 */
package com.bl.facades.populators;

import de.hybris.platform.commercefacades.order.converters.populator.ConsignmentEntryPopulator;
import de.hybris.platform.commercefacades.order.data.ConsignmentEntryData;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.BlItemsBillingChargeModel;
import com.bl.core.model.BlOptionsModel;
import com.bl.core.model.BlProductModel;
import com.bl.logging.BlLogger;


public class BlConsignmentEntryPopulator extends ConsignmentEntryPopulator
{
	private static final Logger LOG = Logger.getLogger(BlConsignmentEntryPopulator.class);

	@Override
	public void populate(final ConsignmentEntryModel source, final ConsignmentEntryData target) throws ConversionException
	{
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "ConsignmentEntryModel : {} ", source.getPk());
		if (source.getOrderEntry() != null)
		{
			super.populate(source, target);
		}
		target.setQuantity(source.getQuantity());
		target.setShippedQuantity(source.getShippedQuantity());
		target.setConsignment(source.getConsignment().getCode());
		target.setPK(source.getPk().getLongValueAsString());
		target.setShippedQuantity(source.getShippedQuantity());
		target.setCreatedTS(source.getCreationtime());
		target.setModifiedTS(source.getModifiedtime());
		target.setGearrated(source.isGearRated());
		target.setOrder_entry(source.getOrderEntry().getOrder().getCode() + "." + source.getOrderEntry().getEntryNumber());

		final List<String> items = new ArrayList<>();
		if (!source.getItems().isEmpty())
		{
			source.getItems().forEach((k, v) -> items.add((k + ":" + v.getCode())));
		}
		target.setItems(StringUtils.join(items, ','));
		final List<String> billingcharges = new ArrayList<>();
		if (!source.getBillingCharges().isEmpty())
		{
			source.getBillingCharges().forEach((k, v) -> billingcharges.add((k + ":" + getCode(v))));
		}
		target.setBillingcharges(StringUtils.join(billingcharges, ','));
		final List<String> consignmententrystatus = new ArrayList<>();
		if (!source.getConsignmentEntryStatus().isEmpty())
		{
			source.getConsignmentEntryStatus().forEach((k, v) -> consignmententrystatus.add((k + ":" + v.getCode())));
		}
		target.setConsignmententrystatus(StringUtils.join(consignmententrystatus, ','));

		target.setSerialproducts(
				source.getSerialProducts().stream().map(BlProductModel::getCode).collect(Collectors.joining(", ")));
		target.setOptions(source.getOptions().stream().map(BlOptionsModel::getOptionId).collect(Collectors.joining(", ")));
		target.setTestingstatus(source.getTestingStatus().getCode());
		target.setQuantityDeclined(source.getQuantityDeclined());
		target.setQuantityPending(source.getQuantityPending());
		target.setQuantityShipped(source.getQuantityShipped());
		target.setMainItemNotScannedCount(source.getMainItemNotScannedCount());
		target.setSubpartsNotScannedCount(source.getSubpartsNotScannedCount());

	}

	private String getCode(final List<BlItemsBillingChargeModel> v)
	{
		return v.isEmpty() ? StringUtils.EMPTY : v.get(0).getCode();
	}

}
