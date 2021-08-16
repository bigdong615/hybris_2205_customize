package com.bl.core.repair.log.service;

import com.bl.core.model.BlRepairLogModel;
import com.bl.core.model.BlSerialProductModel;


/**
 * This Service is used to perform bussiness logics for Repair Log
 *
 * @author Ravikumar
 *
 */
public interface BlRepairLogService
{

	/**
	 * Adds the generated repair log on serial.
	 *
	 * @param repairLogType
	 *           the repair log type
	 * @param blSerialProduct
	 *           the bl serial product
	 */
	public void addGeneratedRepairLog(final Class repairLogType, final BlSerialProductModel blSerialProduct);

	/**
	 * Sets the other data to repair log from serial.
	 *
	 * @param repairLog
	 *           the repair log
	 * @param blSerialProduct
	 *           the bl serial product
	 * @param ctx
	 *           the ctx
	 */
	public void setRepairReasonOnRepairLog(final BlRepairLogModel repairLog, final BlSerialProductModel blSerialProduct);
}
