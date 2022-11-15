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

import com.bl.core.enums.ConsignmentEntryStatusEnum;
import com.bl.core.enums.ItemStatusEnum;
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
			target.setOrder_entry(source.getOrderEntry().getOrder().getCode() + "." + source.getOrderEntry().getEntryNumber());
		}
		target.setQuantity(source.getQuantity());
		target.setShippedQuantity(source.getShippedQuantity());
		target.setConsignment(source.getConsignment() != null ? source.getConsignment().getCode() : StringUtils.EMPTY);
		target.setPK(source.getPk().getLongValueAsString());
		target.setShippedQuantity(source.getShippedQuantity());
		target.setCreatedTS(source.getCreationtime());
		target.setModifiedTS(source.getModifiedtime());
		target.setGearrated(source.isGearRated());

		final List<String> items = new ArrayList<>();
		if (!source.getItems().isEmpty())
		{
			source.getItems().forEach((k, v) -> items.add((k + ":" + getItemsCode(v))));
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
			source.getConsignmentEntryStatus()
					.forEach((k, v) -> consignmententrystatus.add((k + ":" + getConsignmentEntryStatus(v))));
		}
		target.setConsignmententrystatus(StringUtils.join(consignmententrystatus, ','));

		target.setSerialproducts(
				source.getSerialProducts().stream().map(BlProductModel::getCode).collect(Collectors.joining(", ")));
		target.setOptions(source.getOptions().stream().map(BlOptionsModel::getOptionId).collect(Collectors.joining(", ")));
		target.setTestingstatus(source.getTestingStatus() != null ? source.getTestingStatus().getCode() : StringUtils.EMPTY);

		if (source.getConsignment() != null)
		{
			target.setQuantityDeclined(source.getQuantityDeclined());
			target.setQuantityPending(source.getQuantityPending());
			target.setQuantityShipped(source.getQuantityShipped());
			//target.setMainItemNotScannedCount(source.getMainItemNotScannedCount());
			//target.setSubpartsNotScannedCount(source.getSubpartsNotScannedCount());
		}
	}

	/**
	 * @param v
	 * @return
	 */
	private String getConsignmentEntryStatus(final ConsignmentEntryStatusEnum v)
	{
		System.out.println("ConsignmentEntryStatusEnum " + v);

		if (v != null)
		{
			return v.getCode();
		}
		else
		{
			return StringUtils.EMPTY;
		}
	}

	/**
	 * @param v
	 * @return
	 */
	private String getItemsCode(final ItemStatusEnum v)
	{
		System.out.println("ItemStatusEnum " + v);

		if (v != null)
		{
			return v.getCode();
		}
		else
		{
			return StringUtils.EMPTY;
		}
	}

	private String getCode(final List<BlItemsBillingChargeModel> v)
	{
		return v.isEmpty() ? StringUtils.EMPTY : v.get(0).getCode();
	}

}
