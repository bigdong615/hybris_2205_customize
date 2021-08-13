package com.bl.core.model.handler;

import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.servicelayer.model.attribute.DynamicAttributeHandler;

import java.util.List;
import java.util.Objects;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.BlRepairLogModel;
import com.bl.logging.BlLogger;
import com.google.common.util.concurrent.AtomicDouble;


/**
 * @author Ravikumar
 *
 */
public class BlDamageWaiverForRepairLogHandler implements DynamicAttributeHandler<Double, BlRepairLogModel>
{
	private static final Logger LOG = Logger.getLogger(BlDamageWaiverForRepairLogHandler.class);

	@Override
	public Double get(final BlRepairLogModel blRepairLogModel)
	{
		if (Objects.isNull(blRepairLogModel))
		{
			BlLogger.logMessage(LOG, Level.ERROR,
					"Cannot evaluate the value for Damage Waiver taken by Customer for the item because Repair Log is null");
			return Double.valueOf(0.0d);
		}
		if (Objects.isNull(blRepairLogModel.getSerialProduct())
				|| (Objects.isNull(blRepairLogModel.getSerialProduct().getBlProduct())
						|| Objects.isNull(blRepairLogModel.getOrder())))
		{
			BlLogger.logMessage(LOG, Level.ERROR,
					"Cannot evaluate the value for Damage Waiver taken by Customer for the item because no Serial or SKU on Serial or Order found on Repair Log");
			return Double.valueOf(0.0d);
		}
		final List<AbstractOrderEntryModel> orderEntries = blRepairLogModel.getOrder().getEntries();
		if (CollectionUtils.isNotEmpty(orderEntries))
		{
			final String skuCode = blRepairLogModel.getSerialProduct().getBlProduct().getCode();
			final AtomicDouble damageWaiverTaken = new AtomicDouble(0.0d);
			orderEntries.forEach(entry -> getDamageWaiverForRepairLog(skuCode, damageWaiverTaken, entry));
			return damageWaiverTaken.get();
		}
		return Double.valueOf(0.0d);
	}

	/**
	 * Gets the damage waiver for repair log.
	 *
	 * @param skuCode
	 *           the sku code
	 * @param damageWaiverTaken
	 *           the damage waiver taken
	 * @param entry
	 *           the entry
	 * @return the damage waiver for repair log
	 */
	private void getDamageWaiverForRepairLog(final String skuCode, final AtomicDouble damageWaiverTaken,
			final AbstractOrderEntryModel entry)
	{
		final ProductModel product = entry.getProduct();
		if (Objects.nonNull(product) && skuCode.equals(product.getCode()))
		{
			if (BooleanUtils.toBoolean(entry.getGearGuardWaiverSelected()))
			{
				damageWaiverTaken.addAndGet(ObjectUtils.defaultIfNull(entry.getGearGuardWaiverPrice(), Double.valueOf(0.0d)));
			}
			else if (BooleanUtils.toBoolean(entry.getGearGuardProFullWaiverSelected()))
			{
				damageWaiverTaken.addAndGet(ObjectUtils.defaultIfNull(entry.getGearGuardProFullWaiverPrice(), Double.valueOf(0.0d)));
			}
			else
			{
				damageWaiverTaken.addAndGet(0.0d);
			}
		}
	}

	@Override
	public void set(final BlRepairLogModel blRepairLogModel, final Double value)
	{
		BlLogger.logMessage(LOG, Level.ERROR, "Setter for attribute BlRepairLogModel.damageWaiverPaid is not supported");
		throw new UnsupportedOperationException("Setter for attribute BlRepairLogModel.damageWaiverPaid is not supported");
	}

}
