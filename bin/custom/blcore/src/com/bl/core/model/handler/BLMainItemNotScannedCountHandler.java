package com.bl.core.model.handler;

import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.servicelayer.model.attribute.DynamicAttributeHandler;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.ItemStatusEnum;
import com.bl.core.enums.ProductTypeEnum;
import com.bl.core.model.BlSerialProductModel;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;


/**
 * @author Ravikumar
 *
 */
public class BLMainItemNotScannedCountHandler implements DynamicAttributeHandler<Integer, ConsignmentEntryModel>
{
	private static final Logger LOG = Logger.getLogger(BLMainItemNotScannedCountHandler.class);

	@Override
	public Integer get(final ConsignmentEntryModel consignmentEntryModel)
	{
		if (Objects.isNull(consignmentEntryModel))
		{
			BlLogger.logMessage(LOG, Level.ERROR, "BLMainItemNotScannedCountHandler : Consignment Entry is null");
			return Integer.valueOf(BlCoreConstants.INT_ZERO);
		}
		if (MapUtils.isEmpty(consignmentEntryModel.getItems()))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"BLMainItemNotScannedCountHandler : Items Map is Empty for ConsignmentEntry : {}", consignmentEntryModel.getPk());
			return Integer.valueOf(BlCoreConstants.INT_ZERO);
		}

		return getRemainingScanCount(consignmentEntryModel);
	}

	/**
	 * Gets the remaining scan count from the main item list.
	 *
	 * @param entry
	 *           the entry
	 * @return the remaining scan count
	 */
	private Integer getRemainingScanCount(final ConsignmentEntryModel entry)
	{
		final List<BlSerialProductModel> mainItemsList = getMainItemsList(entry);
		if (CollectionUtils.isEmpty(mainItemsList))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"BLMainItemNotScannedCountHandler : No Main Items found in ConsignmentEntry : {}", entry.getPk());
		}
		final Map<String, ItemStatusEnum> items = entry.getItems();
		final AtomicInteger count = new AtomicInteger(BlCoreConstants.INT_ZERO);
		mainItemsList.forEach(mainItem -> {
			if (items.containsKey(mainItem.getCode())
					&& items.get(mainItem.getCode()).getCode().equals(ItemStatusEnum.NOT_INCLUDED.getCode()))
			{
				count.addAndGet(BlCoreConstants.INT_ONE);
			}
		});
		return Integer.valueOf(count.get());
	}

	/**
	 * Gets the main items list from consignment entry.
	 *
	 * @param entry
	 *           the entry
	 * @return the main items list
	 */
	private List<BlSerialProductModel> getMainItemsList(final ConsignmentEntryModel entry)
	{
		if (CollectionUtils.isEmpty(entry.getSerialProducts()))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
					"BLMainItemNotScannedCountHandler :: getMainItemsList :: Serial Products is Empty for ConsignmentEntry : {}",
					entry.getPk());
			return Lists.newArrayList();
		}
		final List<BlSerialProductModel> mainItemsList = Lists.newArrayList();
		entry.getSerialProducts().forEach(product -> {
			if (product instanceof BlSerialProductModel && Objects.nonNull(product.getProductType())
					&& BooleanUtils.isFalse(product.getProductType().getCode().equals(ProductTypeEnum.SUBPARTS.getCode())))
			{
				mainItemsList.add(((BlSerialProductModel) product));
			}
		});
		return mainItemsList;
	}

	@Override
	public void set(final ConsignmentEntryModel consignmentEntryModel, final Integer value)
	{
		BlLogger.logMessage(LOG, Level.ERROR, "Setter for attribute ConsignmentEntry.mainItemNotScannedCount is not supported");
		throw new UnsupportedOperationException();

	}

}
