package com.bl.core.services.order.impl;

import com.bl.core.enums.CustomerCollectionStatusEnum;
import com.bl.core.esp.service.impl.DefaultBlESPEventService;
import com.bl.core.model.BlRepairLogModel;
import com.bl.core.product.service.BlProductService;
import com.bl.core.repair.log.dao.BlRepairLogDao;
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import javax.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
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
  private BlRepairLogDao blRepairLogDao;
	private DefaultBlESPEventService defaultBlESPEventService;

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

		final List<OrderStatus> statusToCheck = Arrays.asList(OrderStatus.COMPLETED,OrderStatus.UNBOXED_PARTIALLY,
				OrderStatus.UNBOXED_COMPLETELY,OrderStatus.INCOMPLETE_ITEMS_IN_REPAIR,OrderStatus.INCOMPLETE_MISSING_ITEMS,
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
			changeStatusOnOrder(order, OrderStatus.UNBOXED_PARTIALLY);
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

			// To call Order Unboxed ESP event service
			if(OrderStatus.UNBOXED_COMPLETELY.equals(orderStatus)) {
				getDefaultBlESPEventService().sendOrderUnboxed((OrderModel) order);
			}
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
	public void setResolvedStatusOnRepairLog(final String orderCode)
	{
		try
		{
			final List<BlRepairLogModel> repairLogForOrderCode = getBlRepairLogDao().getRepairLogForOrderCode(orderCode);
			if (CollectionUtils.isNotEmpty(repairLogForOrderCode))
			{
				repairLogForOrderCode.forEach(repairLog -> {
					repairLog.setCustomerCollectionStatus(CustomerCollectionStatusEnum.RESOLVED);
					getModelService().save(repairLog);
					getModelService().refresh(repairLog);
					BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
							"Marked Customer Collection Status to RESOLVED on Repair log with PK : {} for Order with code : {}",
							repairLog.getPk(), orderCode);
				});
			}
		}
		catch (final Exception exception)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception,
					"Error occured while setting Customer Collection Status to Resolved on Repair Log for order : {}", orderCode);
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public AbstractOrderEntryModel createBundleOrderEntry(final ProductReferenceModel productReferenceModel,
			final AbstractOrderModel orderModel,
			final AbstractOrderEntryModel existingEntry,final AtomicInteger entryNumber){
		BlLogger.logFormattedMessage(LOG, Level.DEBUG,StringUtils.EMPTY,
				"Creating entry for Order {}, Parent bundle Product {} with entry number {}",
				orderModel.getCode(),existingEntry.getProduct().getCode(), entryNumber.get());
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

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void createAndSetBundleOrderEntriesInOrder(final OrderModel orderModel) {
		final List<AbstractOrderEntryModel> orderEntryModelList = new ArrayList<>();
		orderEntryModelList.addAll(
				orderModel.getEntries().stream().filter(orderEntryModel -> !orderEntryModel.isBundleEntry())
						.collect(Collectors.toList()));
		final AtomicInteger entryNumber = new AtomicInteger(orderModel.getEntries().size());
		orderModel.getEntries().forEach(existingEntry -> {
			if (existingEntry.isBundleMainEntry()) {
				final List<ProductReferenceModel> productReferenceModels = productService.getBundleProductReferenceModelFromEntry(existingEntry);
				productReferenceModels.forEach(productReferenceModel -> {
					final AbstractOrderEntryModel newEntryModel = createBundleOrderEntry(productReferenceModel, orderModel, existingEntry,entryNumber);
					orderEntryModelList.add(newEntryModel);
					entryNumber.getAndIncrement();
				});
				existingEntry.setEntryCreated(Boolean.TRUE);
				existingEntry.setBundleProductCode(existingEntry.getProduct().getCode());
				getModelService().save(existingEntry);
			}
		});
		orderModel.setEntries(orderEntryModelList);
		if(BooleanUtils.isFalse(orderModel.getCalculated())){
			orderModel.setCalculated(Boolean.TRUE);
		}
		getModelService().save(orderModel);
	}

	@Override
	public void createAllEntryForBundleProduct(final AbstractOrderEntryModel entryModel){
		final List<AbstractOrderEntryModel> orderEntryModelList = new ArrayList<>();
		orderEntryModelList.addAll(entryModel.getOrder().getEntries());
		final AtomicInteger entryNumber = new AtomicInteger(entryModel.getOrder().getEntries().size());
		final List<ProductReferenceModel> productReferenceModels = productService.getBundleProductReferenceModelFromEntry(entryModel);
		productReferenceModels.forEach(productReferenceModel -> {
			final AbstractOrderEntryModel newEntryModel = createBundleOrderEntry(productReferenceModel, entryModel.getOrder(), entryModel,entryNumber);
			orderEntryModelList.add(newEntryModel);
			entryNumber.getAndIncrement();
		});
		entryModel.setEntryCreated(Boolean.TRUE);
		entryModel.setBundleProductCode(entryModel.getProduct().getCode());
		getModelService().save(entryModel);
		entryModel.getOrder().setEntries(orderEntryModelList);
		if(BooleanUtils.isFalse(entryModel.getOrder().getCalculated())){
			entryModel.getOrder().setCalculated(Boolean.TRUE);
		}
		getModelService().save(entryModel.getOrder());
		getModelService().refresh(entryModel.getOrder());
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

/**
 * @return the blRepairLogDao
 */
public BlRepairLogDao getBlRepairLogDao()
{
	return blRepairLogDao;
}

/**
 * @param blRepairLogDao the blRepairLogDao to set
 */
public void setBlRepairLogDao(BlRepairLogDao blRepairLogDao)
{
	this.blRepairLogDao = blRepairLogDao;
}

	public DefaultBlESPEventService getDefaultBlESPEventService() {
		return defaultBlESPEventService;
	}

	public void setDefaultBlESPEventService(
			DefaultBlESPEventService defaultBlESPEventService) {
		this.defaultBlESPEventService = defaultBlESPEventService;
	}

}
