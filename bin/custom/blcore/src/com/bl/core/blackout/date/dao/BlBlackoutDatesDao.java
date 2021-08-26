package com.bl.core.blackout.date.dao;

import java.util.List;

import com.bl.core.enums.BlackoutDateTypeEnum;
import com.bl.core.model.BlBlackoutDateModel;


/**
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
}
