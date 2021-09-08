package com.bl.core.services.consignment.entry.impl;

import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.Date;
import java.util.List;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.dao.warehouse.BlConsignmentDao;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.services.consignment.entry.BlConsignmentEntryService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;


/**
 * This service class is use to perform custom bussiness logic on consignment entry or consignment
 *
 * @author Ravikumar
 *
 */
public class DefaultBlConsignmentEntryService implements BlConsignmentEntryService
{
	private static final Logger LOG = Logger.getLogger(DefaultBlConsignmentEntryService.class);
	private BlConsignmentDao blConsignmentDao;
	private ModelService modelService;

	@Override
	public void removeSerialFromConsignmentEntry(final BlSerialProductModel blSerialProductModel)
	{
		final List<ConsignmentEntryModel> consignmentEntriesForSerialCodeAndDate = getBlConsignmentDao()
				.getConsignmentEntriesForSerialCodeAndDate(blSerialProductModel,
						BlDateTimeUtils.getFormattedStartDay(new Date()).getTime());
		if (CollectionUtils.isNotEmpty(consignmentEntriesForSerialCodeAndDate))
		{
			consignmentEntriesForSerialCodeAndDate.forEach(consignmentEntry -> {
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
						"Performing removal of serial : {} from consignment entry with PK : {}", blSerialProductModel.getCode(),
						consignmentEntry.getPk().toString());
				final List<BlProductModel> updatedSerialList = getUpdatedSerialList(consignmentEntry, blSerialProductModel);
				consignmentEntry.setSerialProducts(updatedSerialList);
				getModelService().save(consignmentEntry);
				getModelService().refresh(consignmentEntry);
				final ConsignmentModel consignment = consignmentEntry.getConsignment();
				changeStatusOnConsignment(consignment);
				final AbstractOrderModel order = consignment.getOrder();
				changeStatusOnOrder(order);
			});
		}
	}

	/**
	 * Gets the updated serial list.
	 *
	 * @param consignmentEntry
	 *           the consignment entry
	 * @param serial
	 *           the serial
	 * @return the updated serial list
	 */
	private List<BlProductModel> getUpdatedSerialList(final ConsignmentEntryModel consignmentEntry,
			final BlSerialProductModel serial)
	{
		final List<BlProductModel> updatedList = Lists.newArrayList();
		consignmentEntry.getSerialProducts().forEach(serialProduct -> {
			if (!serialProduct.getPk().toString().equals(serial.getPk().toString()))
			{
				updatedList.add(serialProduct);
			}
		});
		return updatedList;
	}

	/**
	 * Change status on consignment.
	 *
	 * @param consignment
	 *           the consignment
	 */
	private void changeStatusOnConsignment(final ConsignmentModel consignment)
	{
		consignment.setStatus(ConsignmentStatus.MANUAL_REVIEW);
		getModelService().save(consignment);
		getModelService().refresh(consignment);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Changing Consignment : {} status to MANUAL_REVIEW", consignment.getCode());
	}

	/**
	 * Change status on order.
	 *
	 * @param order
	 *           the order
	 */
	private void changeStatusOnOrder(final AbstractOrderModel order)
	{
		order.setStatus(OrderStatus.MANUAL_REVIEW);
		getModelService().save(order);
		getModelService().refresh(order);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Changing Order : {} status to MANUAL_REVIEW", order.getCode());
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
