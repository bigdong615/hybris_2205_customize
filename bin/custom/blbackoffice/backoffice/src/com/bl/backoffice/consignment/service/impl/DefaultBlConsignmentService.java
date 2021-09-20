/**
 *
 */
package com.bl.backoffice.consignment.service.impl;


import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import javax.annotation.Resource;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import com.bl.backoffice.consignment.service.BlConsignmentService;
import com.bl.backoffice.widget.controller.order.BlOrderEntryToCancelDto;
import com.bl.core.dao.warehouse.BlConsignmentDao;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.service.BlProductService;
import com.bl.core.stock.BlStockLevelDao;


/**
 * @author Aditi Sharma
 *
 */
public class DefaultBlConsignmentService implements BlConsignmentService
{
	private static final Logger LOGGER = Logger.getLogger(DefaultBlConsignmentService.class);

	@Autowired
	private transient ModelService modelService;

	@Resource(name = "blStockLevelDao")
	private BlStockLevelDao blStockLevelDao;

	@Resource(name = "productService")
	private BlProductService blProductService;

	@Resource(name = "blConsignmentDao")
	private BlConsignmentDao blConsignmentDao;


	@Override
	public void updateStockForPartialCancelledOrder(final Set<ConsignmentModel> consignments,
			final List<BlOrderEntryToCancelDto> cancelAndRefundEntries)
	{
		final List<AbstractOrderEntryModel> consignmentEntryList = new ArrayList<>();
		final List<AbstractOrderEntryModel> orderEntryList = new ArrayList<>();

		getConsignmentEntryList(consignments, consignmentEntryList);

		getCanclledOrderEntryList(orderEntryList, cancelAndRefundEntries);

		final boolean isEntryAvailable = orderEntryList.stream().allMatch(consignmentEntryList::contains);

		if (isEntryAvailable)
		{
			orderEntryList.forEach(orderEntry -> orderEntry.getSerialProducts().forEach(serialProduct -> {

				if (serialProduct instanceof BlSerialProductModel)
				{
					final BlSerialProductModel blSerialProduct = (BlSerialProductModel) serialProduct;
					 ConsignmentModel consignmentForSerialCode = getBlConsignmentDao()
							.getConsignmentEntriesForSerialCode(blSerialProduct.getPk().toString(), orderEntry.getOrder().getCode());
					
					  getBlProductService().updateStockForCancelledProduct(serialProduct,
							  consignmentForSerialCode.getOptimizedShippingStartDate(),
							  consignmentForSerialCode.getOptimizedShippingEndDate());
					 
				}
			}));
		}
	}

	/**
	 * method will used to release the stock for cancelled order
	 *
	 * @param consignments
	 */
	@Override
	public void updateStockForCancelledOrder(final Set<ConsignmentModel> consignments)
	{
		for (final ConsignmentModel consignment : consignments)
		{
			consignment.getConsignmentEntries()
					.forEach(consignmentEntry -> consignmentEntry.getSerialProducts()
							.forEach(serialProduct -> blProductService.updateStockForCancelledProduct(serialProduct,
									consignment.getOptimizedShippingStartDate(), consignment.getOptimizedShippingEndDate())));
		}
	}

	/**
	 * method will be used to get consignmentEntry for cancelled order
	 *
	 * @param consignments
	 * @param consignmentEntryList
	 */
	private void getConsignmentEntryList(final Set<ConsignmentModel> consignments,
			final List<AbstractOrderEntryModel> consignmentEntryList)
	{
		for (final ConsignmentModel consignment : consignments)
		{
			consignment.getConsignmentEntries()
					.forEach(consignmentEntry -> consignmentEntryList.add(consignmentEntry.getOrderEntry()));
		}
	}

	/**
	 * method will be used to get cancelled order entry
	 *
	 * @param orderEntryList
	 */
	private void getCanclledOrderEntryList(final List<AbstractOrderEntryModel> orderEntryList,
			final List<BlOrderEntryToCancelDto> cancelAndRefundEntries)
	{
		for (final BlOrderEntryToCancelDto orderEntryToCancelDto : cancelAndRefundEntries)
		{
			orderEntryList.add(orderEntryToCancelDto.getOrderEntry());
		}
	}

	/**
	 * method will used to get the consignment for serial
	 *
	 * @param consignments
	 * @param serialProduct
	 */
	private ConsignmentModel getConisgnmentForSerial(final Set<ConsignmentModel> consignments, final BlProductModel serialProduct)
	{
		for (final ConsignmentModel consignment : consignments)
		{
			for (final ConsignmentEntryModel consignmentEntry : consignment.getConsignmentEntries())
			{
				getConsignemtForSerialCode(serialProduct, consignment, consignmentEntry);
			}
		}
		return null;
	}



	/**
	 * method will be used to return consignment for serial product
	 *
	 * @param serialProduct
	 * @param consignment
	 * @param consignmentEntry
	 */
	private ConsignmentModel getConsignemtForSerialCode(final BlProductModel serialProduct, final ConsignmentModel consignment,
			final ConsignmentEntryModel consignmentEntry)
	{
		for (final BlProductModel item : consignmentEntry.getSerialProducts())
		{
			if (item.getCode().equals(serialProduct.getCode()))
			{
				return consignment;
			}
		}
		return null;
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
	 * @return the blProductService
	 */
	public BlProductService getBlProductService()
	{
		return blProductService;
	}

	/**
	 * @param blProductService
	 *           the blProductService to set
	 */
	public void setBlProductService(final BlProductService blProductService)
	{
		this.blProductService = blProductService;
	}

}
