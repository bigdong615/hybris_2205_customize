package com.bl.core.model.handler;

import de.hybris.platform.servicelayer.model.attribute.DynamicAttributeHandler;

import java.util.Objects;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.BlRepairLogModel;
import com.bl.logging.BlLogger;


/**
 * This class is responsible to get dynamic value of Total Charge for parts cost and labour by calculating parts cost
 * and labour charge.
 *
 * @author Ravikumar
 *
 */
public class BlTotalForPartsAndLabourChargeHandler implements DynamicAttributeHandler<Double, BlRepairLogModel>
{

	private static final Logger LOG = Logger.getLogger(BlTotalForPartsAndLabourChargeHandler.class);

	@Override
	public Double get(final BlRepairLogModel blRepairLogModel)
	{
		if (Objects.isNull(blRepairLogModel))
		{
			BlLogger.logMessage(LOG, Level.ERROR,
					"Cannot evaluate the value for BlRepairLogModel.totalForPartsAndLaborCost because BlRepairLog is null");
			return Double.valueOf(0.0d);
		}
		final Double costForParts = ObjectUtils.defaultIfNull(blRepairLogModel.getCostForParts(), 0.0d);
		final Double costForLabour = ObjectUtils.defaultIfNull(blRepairLogModel.getCostForLabor(), 0.0d);
		final Double totalCostForPartsAndLabour = costForParts + costForLabour;
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Cost For Parts : {} and Cost for Labour : {} and Total is : {}",
				costForParts, costForLabour, totalCostForPartsAndLabour);
		return totalCostForPartsAndLabour;
	}

	@Override
	public void set(final BlRepairLogModel arg0, final Double arg1)
	{
		BlLogger.logMessage(LOG, Level.ERROR, "Setter for attribute BlRepairLog.totalForPartsAndLaborCost is not supported");
		throw new UnsupportedOperationException();
	}

}
