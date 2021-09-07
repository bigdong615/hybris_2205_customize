package com.bl.core.services.blackout;

import java.util.Date;
import java.util.List;


/**
 * Service class to perform bussiness logic for Blackout Dates
 *
 * @author Ravikumar
 *
 */
public interface BlBlackoutDateService
{

	/**
	 * Perform order return date change with next available date.
	 *
	 * @param forDate
	 *           the for date
	 * @param blackoutDates
	 *           the blackout dates
	 */
	public void performOrderReturnDateChange(final Date forDate, List<Date> blackoutDates);
}
