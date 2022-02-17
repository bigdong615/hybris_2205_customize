package com.bl.core.model.interceptor;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import de.hybris.platform.servicelayer.user.UserService;

import java.util.Objects;
import java.util.UUID;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.Validate;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.BlRepairLogModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.dao.BlProductDao;
import com.bl.core.repair.log.service.BlRepairLogService;
import com.bl.core.services.consignment.entry.BlConsignmentEntryService;
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
	private BlRepairLogService blRepairLogService;
	private BlConsignmentEntryService blConsignmentEntryService;

	@Override
	public void onPrepare(final BlRepairLogModel blRepairLogModel, final InterceptorContext interceptorContext)
			throws InterceptorException
	{
		validateParameterNotNull(blRepairLogModel, "ERROR : BlRepairLogPrepareInterceptor : Parameter BlRepairLogModel is NULL");
		addUniqueRepairLogId(blRepairLogModel);
		addNecessaryDataToRepairLog(blRepairLogModel, interceptorContext);
	}
	
	/**
	 * Adds the unique repair log id.
	 *
	 * @param blRepairLogModel the bl repair log model
	 */
	private void addUniqueRepairLogId(final BlRepairLogModel blRepairLogModel)
	{
		if(StringUtils.isBlank(blRepairLogModel.getRepairLogId()))
		{
			blRepairLogModel.setRepairLogId(UUID.randomUUID().toString());
		}
	}

	/**
	 * Adds the necessary data to repair log.
	 *
	 * @param blRepairLogModel
	 *           the bl repair log model
	 * @param interceptorContext
	 *           the interceptor context
	 * @throws InterceptorException
	 *            the interceptor exception
	 */
	private void addNecessaryDataToRepairLog(final BlRepairLogModel blRepairLogModel, final InterceptorContext interceptorContext)
			throws InterceptorException
	{
		try
		{
			final String itemBarcode = blRepairLogModel.getItemBarcode();
			final String serialCode = blRepairLogModel.getSerialCode();			
			validateSerialCodeAndItemBarcode(serialCode, itemBarcode);
			if (interceptorContext.isNew(blRepairLogModel))
			{
				BlSerialProductModel blSerialProductModel = null;
				if(StringUtils.isNotBlank(serialCode))
				{
					blSerialProductModel = getBlProductDao().getSerialBySerialCode(serialCode);
				}
				if(Objects.isNull(blSerialProductModel) && StringUtils.isNotBlank(itemBarcode))
				{
					blSerialProductModel = getBlProductDao().getSerialByBarcode(itemBarcode);
				}
				if (Objects.isNull(blSerialProductModel))
				{
					BlLogger.logFormatMessageInfo(LOG, Level.ERROR, "No Serial found for barcode : {} or Serial Code : {}", itemBarcode,serialCode);
					throw new InterceptorException("No Serial found");
				}
				blRepairLogModel.setSerialProduct(blSerialProductModel);
				blRepairLogModel.setSerialCode(blSerialProductModel.getCode());
				blRepairLogModel.setItemBarcode(StringUtils.stripToEmpty(blSerialProductModel.getBarcode()));
				setOrderRelatedInformation(blRepairLogModel, blSerialProductModel);
				addCurrentUserToRepairLog(blRepairLogModel);
				getBlRepairLogService().getSelectedGearGaurdFromOrder(blRepairLogModel, blSerialProductModel);
				getBlRepairLogService().updateTrackingNumberOnRepairLog(blRepairLogModel, blSerialProductModel);
				getBlRepairLogService().setRepairReasonOnRepairLog(blRepairLogModel, blSerialProductModel);
				if(StringUtils.isBlank(blRepairLogModel.getLastUserChangedConditionRating()))
				{
					blRepairLogModel.setLastUserChangedConditionRating(blSerialProductModel.getUserChangedConditionRating());
				}
			}
		}
		catch (final Exception exception)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception,
					"Error while adding necessary data to repair log : {}", blRepairLogModel.getItemtype());
			throw exception;
		}
	}
	
	/**
	 * Sets the order related information.
	 *
	 * @param blRepairLogModel
	 *           the bl repair log model
	 * @param blSerialProductModel
	 *           the bl serial product model
	 */
	private void setOrderRelatedInformation(final BlRepairLogModel blRepairLogModel,
			final BlSerialProductModel blSerialProductModel)
	{
		if (Objects.isNull(blRepairLogModel.getOrder()) && Objects.nonNull(blSerialProductModel.getAssociatedOrder()))
		{
			blRepairLogModel.setOrder(blSerialProductModel.getAssociatedOrder());
		}
		if (Objects.nonNull(blSerialProductModel.getConsignmentEntry()))
		{
			blRepairLogModel.setConsignmentEntry(blSerialProductModel.getConsignmentEntry());
		}
		else if(blRepairLogModel.getOrder() instanceof OrderModel)
		{
			blRepairLogModel.setConsignmentEntry(getBlConsignmentEntryService()
					.getConsignmentEntryFromOrderForSerial(((OrderModel)blRepairLogModel.getOrder()), blSerialProductModel.getCode()));
		}
		if (Objects.isNull(blSerialProductModel.getAssociatedConsignment())
				&& Objects.nonNull(blRepairLogModel.getConsignmentEntry()))
		{
			blRepairLogModel.setAssociatedConsignment(blRepairLogModel.getConsignmentEntry().getConsignment());
		}
		else
		{
			blRepairLogModel.setAssociatedConsignment(blSerialProductModel.getAssociatedConsignment());
		}
		if(Objects.nonNull(blRepairLogModel.getAssociatedConsignment()) && Objects.nonNull(blRepairLogModel.getAssociatedConsignment().getWarehouse()))
		{
			blRepairLogModel.setWarehouse(blRepairLogModel.getAssociatedConsignment().getWarehouse());
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
		if(StringUtils.isBlank(blRepairLogModel.getUserId()))
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
	}
	
	/**
	 * Validate serial code and item barcode.
	 *
	 * @param serialCode the serial code
	 * @param itemBarcode the item barcode
	 * @throws InterceptorException the interceptor exception
	 */
	private void validateSerialCodeAndItemBarcode(final String serialCode, final String itemBarcode) throws InterceptorException
	{
		if(StringUtils.isBlank(serialCode) && StringUtils.isBlank(itemBarcode))
		{
			throw new InterceptorException("Either Itembarcode or Serial code is required to create Repair log");
		}
	}

	/**
	 * @return the blRepairLogService
	 */
	public BlRepairLogService getBlRepairLogService()
	{
		return blRepairLogService;
	}

	/**
	 * @param blRepairLogService
	 *           the blRepairLogService to set
	 */
	public void setBlRepairLogService(final BlRepairLogService blRepairLogService)
	{
		this.blRepairLogService = blRepairLogService;
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
	 * @return the blConsignmentEntryService
	 */
	public BlConsignmentEntryService getBlConsignmentEntryService()
	{
		return blConsignmentEntryService;
	}

	/**
	 * @param blConsignmentEntryService the blConsignmentEntryService to set
	 */
	public void setBlConsignmentEntryService(BlConsignmentEntryService blConsignmentEntryService)
	{
		this.blConsignmentEntryService = blConsignmentEntryService;
	}


}
