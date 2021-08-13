package com.bl.core.model.interceptor;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import de.hybris.platform.servicelayer.user.UserService;

import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.Validate;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.BlRepairLogModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.dao.BlProductDao;
import com.bl.logging.BlLogger;


/**
 * The Class BlRepairLogPrepareInterceptor used to intercept the model and modify the attributes before saving the data.
 *
 * @author Ravikumar
 *
 */
public class BlRepairLogPrepareInterceptor implements PrepareInterceptor<BlRepairLogModel>
{
	private static final Logger LOG = Logger.getLogger(BlRepairLogPrepareInterceptor.class);

	private BlProductDao blProductDao;
	private UserService userService;

	@Override
	public void onPrepare(final BlRepairLogModel blRepairLogModel, final InterceptorContext interceptorContext)
			throws InterceptorException
	{
		validateParameterNotNull(blRepairLogModel, "ERROR : BlRepairLogPrepareInterceptor : Parameter BlRepairLogModel is NULL");
		Validate.notBlank(blRepairLogModel.getItemBarcode(),
				"ERROR : BlRepairLogPrepareInterceptor : No Barcode found on Repair Log");
		try
		{
			addNecessaryDataToRepairLog(blRepairLogModel, interceptorContext);
		}
		catch (final Exception exception)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception,
					"Error while adding necessary data to repair log : {}", blRepairLogModel.getItemtype());
			throw new InterceptorException("Error while adding necessary data to repair log");
		}

	}

	/**
	 * Adds the necessary data to repair log.
	 *
	 * @param blRepairLogModel
	 *           the bl repair log model
	 * @param interceptorContext
	 *           the interceptor context
	 * @throws Exception
	 *            the exception
	 */
	private void addNecessaryDataToRepairLog(final BlRepairLogModel blRepairLogModel, final InterceptorContext interceptorContext)
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
			setOtherDataToRepairLog(blRepairLogModel, blSerialProductModel);
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
		if (Objects.nonNull(blSerialProduct.getRepairReasons()))
		{
			repairLog.setRepairReasons(blSerialProduct.getRepairReasons());
		}
		repairLog.setOtherRepairReasons(StringUtils.stripToEmpty(blSerialProduct.getOtherRepairReasons()));
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


}
