package com.bl.core.services.order.impl;

import com.bl.core.product.service.BlProductService;
import com.bl.core.services.order.BlOrderService;
import com.bl.logging.BlLogger;
import com.google.common.collect.Sets;

import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.catalog.model.ProductReferenceModel;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.order.AbstractOrderEntryService;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;

import java.util.concurrent.atomic.AtomicInteger;
import javax.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This class is used to implement various order related functionalities.
 *
 * @author Sunil Sahu
 */
public class DefaultBlOrderService implements BlOrderService {
	
	private static final Logger LOG = Logger.getLogger(DefaultBlOrderService.class);

  private BlProductService productService;
  private ModelService modelService;
	@Resource(name="abstractOrderEntryService")
	private AbstractOrderEntryService abstractOrderEntryService;

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isAquatechProductsPresentInOrder(final AbstractOrderModel orderModel) {

    return orderModel.getEntries().stream().anyMatch(
        entry -> productService.isAquatechProduct(entry.getProduct()));
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isAquatechProductOrder(final AbstractOrderModel orderModel) {

    return orderModel.getEntries().stream().allMatch(
        entry -> productService.isAquatechProduct(entry.getProduct()));
  }
  
  /**
   * {@inheritDoc}
   */
  @Override
  public void checkAndUpdateOrderStatus(final AbstractOrderModel order)
  {
	  if(Objects.nonNull(order) && CollectionUtils.isNotEmpty(order.getConsignments()))
		{
			final HashSet<ConsignmentStatus> itemStatuses = Sets.newHashSet();
			order.getConsignments().forEach(consignment -> itemStatuses.add(consignment.getStatus()));
			if(CollectionUtils.isNotEmpty(itemStatuses)) 
			{
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Statuses found for order containing consignments : {} are {}", 
						order.getCode(), itemStatuses.toString());
				if(itemStatuses.size() == 1)
				{
					doChangeOrderStatusForSingleStatus(order, itemStatuses);
				}
				else
				{
					doChangeOrderStatusForMultipleStatuses(order, itemStatuses);
				}
			}
			
		}
  }
  
  /**
	 * Do change order status for single status.
	 *
	 * @param order the order
	 * @param ctx the ctx
	 * @param itemStatuses the item statuses
	 */
  private void doChangeOrderStatusForSingleStatus(final AbstractOrderModel order,
			final HashSet<ConsignmentStatus> itemStatuses)
	{
		final ConsignmentStatus consignmentStatus = itemStatuses.iterator().next();

		final List<OrderStatus> statusToCheck = Arrays.asList(OrderStatus.COMPLETED,OrderStatus.PARTIALLY_UNBOXED,
				OrderStatus.UNBOXED,OrderStatus.INCOMPLETE_ITEMS_IN_REPAIR,OrderStatus.INCOMPLETE_MISSING_ITEMS,
				OrderStatus.INCOMPLETE_MISSING_AND_BROKEN_ITEMS);
		statusToCheck.forEach(status -> {
			if(status.toString().equals(consignmentStatus.toString()))
			{
				changeStatusOnOrder(order, status);
				return;
			}
		});
	}
  
  /**
	 * Do change order status for multiple statuses.
	 *
	 * @param order the order
	 * @param ctx the ctx
	 * @param itemStatuses the item statuses
	 */
	private void doChangeOrderStatusForMultipleStatuses(final AbstractOrderModel order,
			final HashSet<ConsignmentStatus> itemStatuses)
	{
		if(itemStatuses.contains(ConsignmentStatus.INCOMPLETE_MISSING_AND_BROKEN_ITEMS))
		{
			changeStatusOnOrder(order, OrderStatus.INCOMPLETE_MISSING_AND_BROKEN_ITEMS);
		}
		else if(itemStatuses.contains(ConsignmentStatus.INCOMPLETE_ITEMS_IN_REPAIR) 
				&& itemStatuses.contains(ConsignmentStatus.INCOMPLETE_MISSING_ITEMS))
		{
			changeStatusOnOrder(order, OrderStatus.INCOMPLETE_MISSING_AND_BROKEN_ITEMS);
		}
		else if(itemStatuses.contains(ConsignmentStatus.INCOMPLETE_ITEMS_IN_REPAIR))
		{
			changeStatusOnOrder(order, OrderStatus.INCOMPLETE_ITEMS_IN_REPAIR);
		}
		else if(itemStatuses.contains(ConsignmentStatus.INCOMPLETE_MISSING_ITEMS))
		{
			changeStatusOnOrder(order, OrderStatus.INCOMPLETE_MISSING_ITEMS);
		}
		else if(itemStatuses.contains(ConsignmentStatus.PARTIALLY_UNBOXED))
		{
			changeStatusOnOrder(order, OrderStatus.PARTIALLY_UNBOXED);
		}
	}
	
	/**
	 * Change status on order.
	 *
	 * @param order the order
	 * @param orderStatus the order status
	 * @param ctx the ctx
	 */
	private void changeStatusOnOrder(final AbstractOrderModel order, final OrderStatus orderStatus)
	{
		try
		{
			order.setStatus(orderStatus);
			getModelService().save(order);
			getModelService().refresh(order);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Changing order status to : {} for order code : {}",
					orderStatus,order.getCode());
		}
		catch (final ModelSavingException exception)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception,
					"Error while changing the status on order : {}", order.getCode());
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public AbstractOrderEntryModel createBundleOrderEntry(final ProductReferenceModel productReferenceModel,
			final OrderModel orderModel,
			final AbstractOrderEntryModel existingEntry,final AtomicInteger entryNumber){
		BlLogger.logFormattedMessage(LOG,Level.DEBUG,"Creating entry for {} ",existingEntry.getProduct().getCode());
		final AbstractOrderEntryModel newEntryModel = abstractOrderEntryService.createEntry(orderModel);
		final Long quantity = productReferenceModel.getQuantity()!= null ? productReferenceModel.getQuantity():1L;
		newEntryModel.setQuantity(existingEntry.getQuantity()*quantity);
		newEntryModel.setProduct(productReferenceModel.getTarget());
		newEntryModel.setBundleEntry(Boolean.TRUE);
		newEntryModel.setBundleProductCode(existingEntry.getProduct().getCode());
		newEntryModel.setBasePrice(0.0d);
		newEntryModel.setTotalPrice(0.0d);
		newEntryModel.setUnit(existingEntry.getUnit());
		newEntryModel.setEntryNumber(entryNumber.get());
		getModelService().save(newEntryModel);
		return newEntryModel;
	}

  public BlProductService getProductService() {
    return productService;
  }

  public void setProductService(BlProductService productService) {
    this.productService = productService;
  }

/**
 * @return the modelService
 */
public ModelService getModelService()
{
	return modelService;
}

/**
 * @param modelService the modelService to set
 */
public void setModelService(ModelService modelService)
{
	this.modelService = modelService;
}
}
