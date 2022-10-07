package com.bl.core.model.handler;

import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.model.attribute.DynamicAttributeHandler;

import java.util.Date;
import java.util.Objects;

import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.services.order.BlOrderService;
import com.bl.logging.BlLogger;


/**
 * This class is responsible to get value of Rental Start Date from Order
 *
 * @author Ravikumar
 *
 */
public class BLConsignmentRentalStartDateHandler implements DynamicAttributeHandler<Date, ConsignmentModel>
{
	private static final Logger LOG = Logger.getLogger(BLConsignmentRentalStartDateHandler.class);

	private BlOrderService blOrderService;

	@Override
	public Date get(final ConsignmentModel consignmentModel)
	{
		if (Objects.isNull(consignmentModel))
		{
			BlLogger.logMessage(LOG, Level.ERROR,
					"Cannot evaluate the value for ConsignmentModel.rentalStartDate because ConsignmentModel is null");
			return null;
		}
		if (Objects.isNull(consignmentModel.getOrder()))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
					"Cannot evaluate the value for ConsignmentModel.rentalStartDate because Order is missing on Consignment : {}",
					consignmentModel.getCode());
			return null;
		}
		if (BooleanUtils.isFalse(getBlOrderService().isRentalOrderOnly(consignmentModel.getOrder())))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
					"Cannot evaluate the value for ConsignmentModel.rentalStartDate because Order : {} is not a Rental Order",
					consignmentModel.getOrder().getCode());
			return null;
		}
		if (Objects.isNull(consignmentModel.getOrder().getRentalStartDate()))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"Cannot evaluate the value for ConsignmentModel.rentalStartDate because Rental Start Date is missing on Order : {}",
					consignmentModel.getOrder().getCode());
			return null;
		}
		return consignmentModel.getOrder().getRentalStartDate();
	}

	@Override
	public void set(final ConsignmentModel consignmentModel, final Date rentalStartDate)
	{
		BlLogger.logMessage(LOG, Level.ERROR, "Setter for attribute ConsignmentModel.rentalStartDate is not supported");
		throw new UnsupportedOperationException();
	}

	/**
	 * @return the blOrderService
	 */
	public BlOrderService getBlOrderService()
	{
		return blOrderService;
	}

	/**
	 * @param blOrderService
	 *           the blOrderService to set
	 */
	public void setBlOrderService(final BlOrderService blOrderService)
	{
		this.blOrderService = blOrderService;
	}

}
