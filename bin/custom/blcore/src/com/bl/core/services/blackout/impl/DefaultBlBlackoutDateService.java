package com.bl.core.services.blackout.impl;

import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.model.ModelService;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;
import java.util.List;
import java.util.Objects;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.dao.warehouse.BlConsignmentDao;
import com.bl.core.enums.BlackoutDateTypeEnum;
import com.bl.core.services.blackout.BlBlackoutDateService;
import com.bl.core.services.cart.BlCartService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.logging.BlLogger;


/**
 * Service implementation class to perform bussiness logic for Blackout Dates
 *
 * @author Ravikumar
 *
 */
public class DefaultBlBlackoutDateService implements BlBlackoutDateService
{
	private static final Logger LOG = Logger.getLogger(DefaultBlBlackoutDateService.class);
	private BlConsignmentDao blConsignmentDao;
	private ModelService modelService;
   private BlCartService blCartService;

	@Override
	public void performOrderReturnDateChange(final Date forDate, final List<Date> blackoutDates)
	{
		final List<ConsignmentModel> consignmentsForReturnDate = getBlConsignmentDao().getConsignmentForReturnDate(forDate);
		if (CollectionUtils.isNotEmpty(consignmentsForReturnDate))
		{
			final LocalDate localDateOptimizedShippingEndDate = LocalDate.ofInstant(forDate.toInstant(), ZoneId.systemDefault());
			final Date newOptimizedShippingEndDate = BlDateTimeUtils.addDaysInRentalDates(BlCoreConstants.ONE_DAY,
					localDateOptimizedShippingEndDate, blackoutDates);
			consignmentsForReturnDate.forEach(consignemt -> {
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Optimized End Date on consignment with code : {} is : {}",
						consignemt.getCode(), forDate);
				consignemt.setOptimizedShippingEndDate(newOptimizedShippingEndDate);
				consignemt.setOptimizedRentalEndDateChangedByJob(Boolean.TRUE);
				getModelService().save(consignemt);
				getModelService().refresh(consignemt);
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
						"Changed Optimized End Date on consignment with code : {} from : {} to : {}", consignemt.getCode(), forDate,
						newOptimizedShippingEndDate);
			});
		}

	}
	
	/**
 	 * {@inheritDoc}
 	 */
	@Override
	public boolean checkForBlackoutDate(final RentalDateDto rentalDateDto)
	{
		return Objects.nonNull(rentalDateDto) && isDatesAvailable(rentalDateDto) 
				&& (blCartService.isSelectedDateIsBlackoutDate(
						 BlDateTimeUtils.getDate(rentalDateDto.getSelectedFromDate(), BlCoreConstants.DATE_FORMAT),
						 BlackoutDateTypeEnum.RENTAL_START_DATE)
						 || blCartService.isSelectedDateIsBlackoutDate(
								 BlDateTimeUtils.getDate(rentalDateDto.getSelectedToDate(), BlCoreConstants.DATE_FORMAT),
								 BlackoutDateTypeEnum.RENTAL_END_DATE));
	}

	/**
	 * Checks if is dates available.
	 *
	 * @param rentalDateDto the rental date dto
	 * @return true, if is dates available
	 */
	private boolean isDatesAvailable(final RentalDateDto rentalDateDto)
	{
		return StringUtils.isNotBlank(rentalDateDto.getSelectedFromDate()) && StringUtils.isNotBlank(rentalDateDto.getSelectedToDate());
	}

	/**
	 * @return the blConsignmentDao
	 */
	public BlConsignmentDao getBlConsignmentDao()
	{
		return blConsignmentDao;
	}

	/**
	 * @param blConsignmentDao
	 *           the blConsignmentDao to set
	 */
	public void setBlConsignmentDao(final BlConsignmentDao blConsignmentDao)
	{
		this.blConsignmentDao = blConsignmentDao;
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
