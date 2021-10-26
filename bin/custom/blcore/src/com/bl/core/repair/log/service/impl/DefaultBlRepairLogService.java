package com.bl.core.repair.log.service.impl;

import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.util.Objects;

import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.enums.GearGaurdEnum;
import com.bl.core.model.BlRepairLogModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.VendorRepairLogModel;
import com.bl.core.repair.log.service.BlRepairLogService;
import com.bl.integration.services.BLShipmentCreationService;
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
	private BLShipmentCreationService blShipmentCreationService;

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
	 * {@inheritDoc}
	 */
	@Override
	public void getSelectedGearGaurdFromOrder(final BlRepairLogModel blRepairLogModel,
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
	 * {@inheritDoc}
	 */
	@Override
	public void updateTrackingNumberOnRepairLog(final BlRepairLogModel blRepairLogModel,
			final BlSerialProductModel blSerialProductModel)
	{
		if (blRepairLogModel instanceof VendorRepairLogModel && Objects.nonNull(blSerialProductModel.getAssociatedConsignment()))
		{
			final ConsignmentModel associatedConsignment = blSerialProductModel.getAssociatedConsignment();
			if (Objects.nonNull(associatedConsignment))
			{
				final PackagingInfoModel packageForSerial = getBlShipmentCreationService().getPackageForSerial(associatedConsignment,
						blSerialProductModel.getCode());
				if (Objects.nonNull(packageForSerial))
				{
					((VendorRepairLogModel) blRepairLogModel)
							.setTrackingNumber(StringUtils.stripToEmpty(packageForSerial.getOutBoundTrackingNumber()));
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

	/**
	 * @return the blShipmentCreationService
	 */
	public BLShipmentCreationService getBlShipmentCreationService()
	{
		return blShipmentCreationService;
	}

	/**
	 * @param blShipmentCreationService
	 *           the blShipmentCreationService to set
	 */
	public void setBlShipmentCreationService(final BLShipmentCreationService blShipmentCreationService)
	{
		this.blShipmentCreationService = blShipmentCreationService;
	}

}
