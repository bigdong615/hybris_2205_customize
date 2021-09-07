package com.bl.core.blackout.date.dao;

import java.util.Date;
import java.util.List;

import com.bl.core.enums.BlackoutDateTypeEnum;
import com.bl.core.model.BlBlackoutDateModel;


/**
 * This Dao Class is used to get list of blackout dates.
 *
 * @author Ravikumar
 *
 */
public interface BlBlackoutDatesDao
{

	/**
	 * Gets all blackout dates for given Blackout Date Type.
	 *
	 * @param blackoutDateType
	 *           the blackout date type
	 * @return all blackout dates for given Blackout Date Type.
	 */
	public List<BlBlackoutDateModel> getAllBlackoutDatesForGivenType(final BlackoutDateTypeEnum blackoutDateType);

	/**
	 * Gets the all blackout dates for shipping methods.
	 *
	 * @param deliveryModeCodes
	 *           the delivery mode codes
	 * @return the all blackout dates for shipping methods
	 */
	public List<BlBlackoutDateModel> getAllBlackoutDatesForShippingMethods(final List<String> deliveryModeCodes);

	/**
	 * Gets the all blackout dates for given type and from date provided.
	 *
	 * @param fromDate
	 *           the from date
	 * @param blackoutDateType
	 *           the blackout date type
	 * @return the all blackout dates for given type and from date
	 */
	public List<BlBlackoutDateModel> getAllBlackoutDatesForGivenTypeAndFromDate(final Date fromDate,
			final BlackoutDateTypeEnum blackoutDateType);
}
