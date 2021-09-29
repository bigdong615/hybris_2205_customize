package com.bl.core.model.handler;

import de.hybris.platform.servicelayer.model.attribute.DynamicAttributeHandler;

import java.util.Objects;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.BlRepairLogModel;
import com.bl.logging.BlLogger;


/**
 * This handler is used to get dynamic value of Condition Rating of Serial
 *
 * @author Ravikumar
 *
 */
public class BlRepairLogSerialConditionRatingHandler implements DynamicAttributeHandler<Double, BlRepairLogModel>
{
	private static final Logger LOG = Logger.getLogger(BlRepairLogSerialConditionRatingHandler.class);

	@Override
	public Double get(final BlRepairLogModel blRepairLogModel)
	{
		if (Objects.isNull(blRepairLogModel))
		{
			BlLogger.logMessage(LOG, Level.ERROR,
					"Cannot evaluate the value for BlRepairLogModel.conditionRating because BlRepairLog is null");
			return Double.valueOf(0.0d);
		}
		if (Objects.nonNull(blRepairLogModel.getSerialProduct()))
		{
			return blRepairLogModel.getSerialProduct().getConditionRatingOverallScore();
		}
		return Double.valueOf(0.0d);
	}

	@Override
	public void set(final BlRepairLogModel blRepairLogModel, final Double ratingValue)
	{
		BlLogger.logMessage(LOG, Level.ERROR, "Setter for attribute BlRepairLog.conditionRating is not supported");
		throw new UnsupportedOperationException();
	}

}
