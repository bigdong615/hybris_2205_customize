package com.bl.core.model.handler;

import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.servicelayer.model.attribute.DynamicAttributeHandler;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;

import javax.annotation.Resource;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.ItemStatusEnum;
import com.bl.core.services.consignment.entry.BlConsignmentEntryService;
import com.bl.logging.BlLogger;


/**
 * @author Ravikumar
 *
 */
public class BLSubpartsNotScannedCountHandler implements DynamicAttributeHandler<Integer, ConsignmentEntryModel>
{
	private static final Logger LOG = Logger.getLogger(BLSubpartsNotScannedCountHandler.class);

	@Resource(name = "blConsignmentEntryService")
	private BlConsignmentEntryService blConsignmentEntryService;

	@Override
	public Integer get(final ConsignmentEntryModel consignmentEntryModel)
	{
		if (Objects.isNull(consignmentEntryModel))
		{
			BlLogger.logMessage(LOG, Level.ERROR, "BLSubpartsNotScannedCountHandler : Consignment Entry is null");
			return Integer.valueOf(BlCoreConstants.INT_ZERO);
		}
		if (MapUtils.isEmpty(consignmentEntryModel.getItems()))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"BLSubpartsNotScannedCountHandler : Items Map is Empty for ConsignmentEntry : {}", consignmentEntryModel.getPk());
			return Integer.valueOf(BlCoreConstants.INT_ZERO);
		}
		return getSubpartsRemainingScanCount(consignmentEntryModel);
	}

	/**
	 * Gets the subparts remaining scan count.
	 *
	 * @param entry
	 *           the entry
	 * @return the subparts remaining scan count
	 */
	private Integer getSubpartsRemainingScanCount(final ConsignmentEntryModel entry)
	{
		final List<String> subPartItemsList = getBlConsignmentEntryService().getSubpartItemsList(entry);
		if (CollectionUtils.isEmpty(subPartItemsList))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"BLSubpartsNotScannedCountHandler :: getSubpartsRemainingScanCount :: No Subpart Items found in ConsignmentEntry : {}",
					entry.getPk());
			return Integer.valueOf(BlCoreConstants.INT_ZERO);
		}
		final Map<String, ItemStatusEnum> items = entry.getItems();
		final AtomicInteger count = new AtomicInteger(BlCoreConstants.INT_ZERO);
		subPartItemsList.forEach(subPartItem -> {
			if (items.containsKey(subPartItem) && items.get(subPartItem).getCode().equals(ItemStatusEnum.NOT_INCLUDED.getCode()))
			{
				count.addAndGet(BlCoreConstants.INT_ONE);
			}
		});
		return Integer.valueOf(count.get());
	}

	@Override
	public void set(final ConsignmentEntryModel consignmentEntryModel, final Integer value)
	{
		BlLogger.logMessage(LOG, Level.ERROR, "Setter for attribute ConsignmentEntry.subpartsNotScannedCount is not supported");
		throw new UnsupportedOperationException();

	}

	/**
	 * @return the blConsignmentEntryService
	 */
	public BlConsignmentEntryService getBlConsignmentEntryService()
	{
		return blConsignmentEntryService;
	}

	/**
	 * @param blConsignmentEntryService
	 *           the blConsignmentEntryService to set
	 */
	public void setBlConsignmentEntryService(final BlConsignmentEntryService blConsignmentEntryService)
	{
		this.blConsignmentEntryService = blConsignmentEntryService;
	}

}
