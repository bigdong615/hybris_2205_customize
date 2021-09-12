package com.bl.core.model.interceptor;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.util.Objects;

import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.Validate;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.enums.GearGaurdEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlRepairLogModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.VendorRepairLogModel;
import com.bl.core.product.dao.BlProductDao;
import com.bl.core.repair.log.service.BlRepairLogService;
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

	@Override
	public void onPrepare(final BlRepairLogModel blRepairLogModel, final InterceptorContext interceptorContext)
			throws InterceptorException
	{
		validateParameterNotNull(blRepairLogModel, "ERROR : BlRepairLogPrepareInterceptor : Parameter BlRepairLogModel is NULL");
		Validate.notBlank(blRepairLogModel.getItemBarcode(),
				"ERROR : BlRepairLogPrepareInterceptor : No Barcode found on Repair Log");
		addNecessaryDataToRepairLog(blRepairLogModel, interceptorContext);
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
				getSelectedGearGaurdFromOrder(blRepairLogModel, blSerialProductModel);
				updateTrackingNumberOnLog(blRepairLogModel, blSerialProductModel);
				getBlRepairLogService().setRepairReasonOnRepairLog(blRepairLogModel, blSerialProductModel);
				blRepairLogModel.setLastUserChangedConditionRating(blSerialProductModel.getUserChangedConditionRating());
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
	 * Gets the selected gear gaurd from order.
	 *
	 * @param blRepairLogModel
	 *           the bl repair log model
	 * @param blSerialProductModel
	 *           the bl serial product model
	 * @return the selected gear gaurd from order
	 */
	private void getSelectedGearGaurdFromOrder(final BlRepairLogModel blRepairLogModel,
			final BlSerialProductModel blSerialProductModel)
	{
		blRepairLogModel.setSelectedGearGaurd(GearGaurdEnum.NONE);
		if (Objects.nonNull(blSerialProductModel.getConsignmentEntry())
				&& Objects.nonNull(blSerialProductModel.getConsignmentEntry().getOrderEntry()))
		{
			final AbstractOrderEntryModel orderEntry = blSerialProductModel.getConsignmentEntry().getOrderEntry();
			if (BooleanUtils.toBoolean(orderEntry.getGearGuardWaiverSelected()))
			{
				blRepairLogModel.setSelectedGearGaurd(GearGaurdEnum.GEAR_GAURD);
			}
			else if (BooleanUtils.toBoolean(orderEntry.getGearGuardProFullWaiverSelected()))
			{
				blRepairLogModel.setSelectedGearGaurd(GearGaurdEnum.GEAR_GAURD_PRO);
			}
		}
	}

	/**
	 * Update tracking number on log.
	 *
	 * @param blRepairLogModel
	 *           the bl repair log model
	 * @param blSerialProductModel
	 *           the bl serial product model
	 */
	private void updateTrackingNumberOnLog(final BlRepairLogModel blRepairLogModel,
			final BlSerialProductModel blSerialProductModel)
	{
		if (blRepairLogModel instanceof VendorRepairLogModel && Objects.nonNull(blSerialProductModel.getAssociatedConsignment()))
		{
			final ConsignmentModel associatedConsignment = blSerialProductModel.getAssociatedConsignment();
			if (Objects.nonNull(associatedConsignment))
			{
				final PackagingInfoModel packageForSerial = getPackageForSerial(associatedConsignment,
						blSerialProductModel.getCode());
				if (Objects.nonNull(packageForSerial))
				{
					((VendorRepairLogModel) blRepairLogModel)
							.setTrackingNumber(StringUtils.stripToEmpty(packageForSerial.getTrackingNumber()));
				}
				else
				{
					BlLogger.logFormatMessageInfo(LOG, Level.ERROR, "No Package found for serial : {} on Consignment : {}",
							blSerialProductModel.getCode(), associatedConsignment.getCode());
				}
			}
		}
	}

	/**
	 * Gets the package for serial.
	 *
	 * @param consignment
	 *           the consignment
	 * @param serialCode
	 *           the serial code
	 * @return the package for serial
	 */
	private PackagingInfoModel getPackageForSerial(final ConsignmentModel consignment, final String serialCode)
	{
		for (final PackagingInfoModel blPackage : consignment.getPackaginginfos())
		{
			if (isSerialPresentInPackage(blPackage, serialCode))
			{
				return blPackage;
			}
		}
		return null;
	}

	/**
	 * Checks if is serial present in package.
	 *
	 * @param blPackage
	 *           the bl package
	 * @param serialCode
	 *           the serial code
	 * @return true, if is serial present in package
	 */
	private boolean isSerialPresentInPackage(final PackagingInfoModel blPackage, final String serialCode)
	{
		for (final BlProductModel product : blPackage.getSerialProducts())
		{
			if (product.getCode().equals(serialCode))
			{
				return true;
			}
		}
		return false;
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


}
