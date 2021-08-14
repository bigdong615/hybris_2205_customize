package com.bl.core.repair.log.service.impl;

import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;

import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.BlRepairLogModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.dao.BlProductDao;
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

	private BlProductDao blProductDao;
	private UserService userService;
	private ModelService modelService;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void addNecessaryDataToRepairLog(final BlRepairLogModel blRepairLogModel, final InterceptorContext interceptorContext)
			throws InterceptorException
	{
		try
		{
			final String itemBarcode = blRepairLogModel.getItemBarcode();
			if (interceptorContext.isNew(blRepairLogModel))
			{
				final BlSerialProductModel blSerialProductModel = getBlProductDao()
						.getSerialByBarcode(blRepairLogModel.getItemBarcode());
				if (Objects.isNull(blSerialProductModel))
				{
					BlLogger.logFormatMessageInfo(LOG, Level.ERROR, "No Serial found for barcode : {}", itemBarcode);
					throw new IllegalStateException("No Serial found for barcode : " + itemBarcode);
				}
				blRepairLogModel.setSerialProduct(blSerialProductModel);
				blRepairLogModel.setSerialCode(blSerialProductModel.getCode());
				blRepairLogModel.setOrder(blSerialProductModel.getAssociatedOrder());
				blRepairLogModel.setAssociatedConsignment(blSerialProductModel.getAssociatedConsignment());
				blRepairLogModel.setConsignmentEntry(blSerialProductModel.getConsignmentEntry());
				addCurrentUserToRepairLog(blRepairLogModel);
				setRepairReasonOnRepairLog(blRepairLogModel, blSerialProductModel);
			}
		}
		catch (final Exception exception)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception,
					"Error while adding necessary data to repair log : {}", blRepairLogModel.getItemtype());
			throw new InterceptorException("Error while adding necessary data to repair log");
		}
	}

	/**
	 * Adds the current user to repair log.
	 *
	 * @param blRepairLogModel
	 *           the bl repair log model
	 */
	private void addCurrentUserToRepairLog(final BlRepairLogModel blRepairLogModel)
	{
		final UserModel currentUser = getUserService().getCurrentUser();
		if (Objects.nonNull(currentUser))
		{
			final String currentUserUid = currentUser.getUid();
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Current user id : {}", currentUserUid);
			blRepairLogModel.setUserId(currentUserUid);
		}
		else
		{
			BlLogger.logMessage(LOG, Level.ERROR, "Unable to fetch current user from session");
		}
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
	 * @return the blProductDao
	 */
	public BlProductDao getBlProductDao()
	{
		return blProductDao;
	}

	/**
	 * @param blProductDao
	 *           the blProductDao to set
	 */
	public void setBlProductDao(final BlProductDao blProductDao)
	{
		this.blProductDao = blProductDao;
	}

	/**
	 * @return the userService
	 */
	public UserService getUserService()
	{
		return userService;
	}

	/**
	 * @param userService
	 *           the userService to set
	 */
	public void setUserService(final UserService userService)
	{
		this.userService = userService;
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
