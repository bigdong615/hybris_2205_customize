package com.bl.core.repair.log.dao;

import java.util.List;

import com.bl.core.model.BlRepairLogModel;


/**
 * Bl Repair Log Dao class to perform logic to get data fron DB with specific requirements
 *
 * @author Ravikumar
 *
 */
public interface BlRepairLogDao
{

	/**
	 * Gets the repair log for order code.
	 *
	 * @param orderCode
	 *           the order code
	 * @return the repair log for order code
	 */
	List<BlRepairLogModel> getRepairLogForOrderCode(final String orderCode);
}
