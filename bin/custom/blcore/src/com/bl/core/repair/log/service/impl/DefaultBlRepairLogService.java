package com.bl.core.repair.log.service.impl;

import de.hybris.platform.servicelayer.model.ModelService;

import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.BlRepairLogModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.repair.log.service.BlRepairLogService;
import com.bl.logging.BlLogger;


/**
 * This Service is used to perform bussiness logics for Repair Log
 *
 * @author Ravikumar
 *
 */
public class DefaultBlRepairLogService implements BlRepairLogService
{
	private static final Logger LOG = Logger.getLogger(DefaultBlRepairLogService.class);

	private ModelService modelService;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void addGeneratedRepairLog(final Class repairLogType, final BlSerialProductModel blSerialProduct)
	{
		final BlRepairLogModel repairLog = getModelService().create(repairLogType);
		repairLog.setItemBarcode(blSerialProduct.getBarcode());
		repairLog.setSerialCode(blSerialProduct.getCode());
		getModelService().save(repairLog);
		setOtherDataToRepairLog(repairLog, blSerialProduct);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Repair Log of type : {} is created with PK : {} for Serial with code : {}",
				repairLogType.getName(), repairLog.getPk().toString(), blSerialProduct.getCode());
	}

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
	private void setOtherDataToRepairLog(final BlRepairLogModel repairLog, final BlSerialProductModel blSerialProduct)
	{
		setRepairReasonOnRepairLog(repairLog, blSerialProduct);
		getModelService().save(repairLog);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void setRepairReasonOnRepairLog(final BlRepairLogModel repairLog, final BlSerialProductModel blSerialProduct)
	{
		if (Objects.nonNull(blSerialProduct.getRepairReasons()))
		{
			repairLog.setRepairReasons(blSerialProduct.getRepairReasons());
		}
		repairLog.setOtherRepairReasons(StringUtils.stripToEmpty(blSerialProduct.getOtherRepairReasons()));
	}

	/**
	 * @return the modelService
	 */
	public ModelService getModelService()
	{
		return modelService;
	}

	/**
	 * @param modelService
	 *           the modelService to set
	 */
	public void setModelService(final ModelService modelService)
	{
		this.modelService = modelService;
	}

}
