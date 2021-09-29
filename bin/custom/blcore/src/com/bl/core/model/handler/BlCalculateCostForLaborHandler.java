package com.bl.core.model.handler;

import de.hybris.platform.servicelayer.model.attribute.DynamicAttributeHandler;

import java.util.Objects;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.BlRepairLogModel;
import com.bl.logging.BlLogger;


/**
 * This class is responsible to get dynamic calculated value of cost and labour by calculating labor hours and labor
 * rate value
 *
 * @author Ravikumar
 *
 */
public class BlCalculateCostForLaborHandler implements DynamicAttributeHandler<Double, BlRepairLogModel>
{
	private static final Logger LOG = Logger.getLogger(BlCalculateCostForLaborHandler.class);

	@Override
	public Double get(final BlRepairLogModel blRepairLogModel)
	{
		if (Objects.isNull(blRepairLogModel))
		{
			BlLogger.logMessage(LOG, Level.ERROR,
					"Cannot evaluate the value for BlRepairLogModel.costOfLabor because BlRepairLog is null");
			return Double.valueOf(0.0d);
		}
		final Double laborHours = ObjectUtils.defaultIfNull(blRepairLogModel.getLabourHours(), 0.0d);
		final Double laborRate = ObjectUtils.defaultIfNull(blRepairLogModel.getLabourRate(), 0.0d);
		final Double totalCostOfLabour = laborHours * laborRate;
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Labor Hours : {} and Labor Rate : {} and Total for Cost of Labor is : {}",
				laborHours, laborRate, totalCostOfLabour);
		return totalCostOfLabour;
	}

	@Override
	public void set(final BlRepairLogModel arg0, final Double arg1)
	{
		BlLogger.logMessage(LOG, Level.ERROR, "Setter for attribute BlRepairLog.costOfLabor is not supported");
		throw new UnsupportedOperationException();
	}

}
