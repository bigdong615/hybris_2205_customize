package com.bl.core.model.interceptor;

import com.bl.core.model.BlProductModel;

import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import de.hybris.platform.servicelayer.model.ItemModelContextImpl;

import java.math.BigDecimal;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.RepairTypeEnum;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.jalo.BlSerialProduct;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.CustomerResponsibleRepairLogModel;
import com.bl.core.model.InHouseRepairLogModel;
import com.bl.core.model.PartsNeededRepairLogModel;
import com.bl.core.model.VendorRepairLogModel;
import com.bl.core.repair.log.service.BlRepairLogService;
import com.bl.core.services.calculation.BlPricingService;
import com.bl.core.stock.BlStockService;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;


/**
 * The Class BlSerialProductPrepareInterceptor used to intercept the model and modify the attributes before saving the data.
 *
 * @author Ritika
 */
public class BlSerialProductPrepareInterceptor implements PrepareInterceptor<BlSerialProductModel>
{
	/** The bl pricing service. */
	private BlPricingService blPricingService;
	private BlStockService blStockService;
	private BlRepairLogService blRepairLogService;

	private static final Logger LOG = Logger.getLogger(BlSerialProductPrepareInterceptor.class);

	/**
	 * On prepare.
	 *
	 * @param blSerialProduct
	 *           the bl serial product
	 * @param ctx
	 *           the ctx
	 * @throws InterceptorException
	 *            the interceptor exception
	 */
	@Override
	public void onPrepare(final BlSerialProductModel blSerialProduct, final InterceptorContext ctx) throws InterceptorException
	{
		if(Objects.nonNull(blSerialProduct))
		{
			//Intercepting the change in serialStatus and changing the consignment status accordingly if available
			doStatusChangeOnConsignment(blSerialProduct, ctx);
			createRepairLogIfRepairNeeded(blSerialProduct, ctx);
			//Intercepting forSaleBasePrice and conditionRatingOverallScore attribute to create finalSalePrice for serial
			calculateFinalSalePriceForSerial(blSerialProduct, ctx);
			//Intercepting finalSalePrice and forSaleDiscount attribute to create incentivizedPrice for serial
			calculateIncentivizedPriceForSerial(blSerialProduct, ctx);
			updateStockRecordsOnSerialStatusUpdate(blSerialProduct, ctx);
			updateStockRecordsOnForRentFlagUpdate(blSerialProduct, ctx);
			updateWarehouseInStockRecordsOnWHLocUpdate(blSerialProduct, ctx);
		}
	}

	/**
	 * It updates the warehouse in stock records when warehouse location is changed of a serial product
	 *
	 * @param blSerialProduct
	 *           the bl serial product
	 * @param ctx
	 *           the ctx
	 */
	private void updateWarehouseInStockRecordsOnWHLocUpdate(final BlSerialProductModel blSerialProduct,
			final InterceptorContext ctx)
	{
		try {
			final Object initialValue = getInitialValue(blSerialProduct, BlSerialProduct.WAREHOUSELOCATION);
			if (null != initialValue && ctx.isModified(blSerialProduct, BlSerialProductModel.WAREHOUSELOCATION) &&
					blSerialProduct.getWarehouseLocation() != null &&
					blSerialProduct.getSerialStatus().equals(SerialStatusEnum.ACTIVE)) {
					getBlStockService().findAndUpdateWarehouseInStockRecords(blSerialProduct);
			}
		} catch(final Exception ex) {
			BlLogger.logFormattedMessage(LOG, Level.ERROR, BlCoreConstants.EMPTY_STRING, ex,
					"Exception occurred while updating the warehouse {} in the stock record for serial product {} ",
					blSerialProduct.getWarehouseLocation(), blSerialProduct.getCode());
		}
	}

	/**
	 * It deletes the date specific stock records when 'forRent' flag  of a serial product is marked as false
	 * and creates one stock record where date is null
	 *
	 * @param blSerialProduct
	 *           the bl serial product
	 * @param ctx
	 *           the ctx
	 */
	private void updateStockRecordsOnForRentFlagUpdate(final BlSerialProductModel blSerialProduct, final InterceptorContext ctx) {
		try {
			final Object initialValue = getInitialValue(blSerialProduct, BlSerialProduct.FORRENT);
			if (null != initialValue && ctx.isModified(blSerialProduct, BlProductModel.FORRENT)
					&& isEligibleForStockUpdate(blSerialProduct)) {
					getBlStockService().findAndDeleteStockRecords(blSerialProduct);
				}
		} catch(final Exception ex) {
			BlLogger.logFormattedMessage(LOG, Level.ERROR, BlCoreConstants.EMPTY_STRING, ex,
					"Exception occurred while updating the stock records on 'For Rent' flag change event of serial product {} ",
					blSerialProduct.getCode());
		}
	}
	
	/**
	 * Checks if is eligible for stock update.
	 *
	 * @param blSerialProduct the bl serial product
	 * @return true, if is eligible for stock update
	 */
	private boolean isEligibleForStockUpdate(final BlSerialProductModel blSerialProduct)
	{
		return SerialStatusEnum.ACTIVE.equals(blSerialProduct.getSerialStatus()) && BooleanUtils.isTrue(blSerialProduct.getForSale()) 
				&& BooleanUtils.isFalse(blSerialProduct.getForRent());
	}

	/**
	 * It updates the stock records when serial status of a serial product is changed
	 *
	 * @param blSerialProduct
	 *           the bl serial product
	 * @param ctx
	 *           the ctx
	 */
	private void updateStockRecordsOnSerialStatusUpdate(final BlSerialProductModel blSerialProduct, final InterceptorContext ctx)
	{
		try {
			final Object initialValue = getInitialValue(blSerialProduct, BlSerialProduct.SERIALSTATUS);
			if (null != initialValue && ctx.isModified(blSerialProduct, BlSerialProductModel.SERIALSTATUS)) {
				if (SerialStatusEnum.ACTIVE.equals(blSerialProduct.getSerialStatus())) {
					updateStockRecordsAsAvailable(blSerialProduct, initialValue);
				} else if(SerialStatusEnum.ACTIVE.equals(initialValue)){
					getBlStockService().findAndUpdateStockRecords(blSerialProduct, true);
				}
			}
		} catch(final Exception ex) {
			BlLogger.logFormattedMessage(LOG, Level.ERROR, BlCoreConstants.EMPTY_STRING, ex,
					"Exception occurred while updating the stock records on serial status change event of serial product {} ",
					blSerialProduct.getCode());
		}
	}

	/**
	 * It updates the stock records as reserved status is false
	 *
	 * @param blSerialProduct
	 *           the bl serial product
	 * @param initialValue
	 */
	private void updateStockRecordsAsAvailable(final BlSerialProductModel blSerialProduct, final Object initialValue) {
		if (initialValue.equals(SerialStatusEnum.COMING_FROM_PURCHASE) && null != blSerialProduct.getWarehouseLocation()
				&& Boolean.TRUE.equals(blSerialProduct.getForRent()))
		{
			getBlStockService().createStockRecordsForNewSerialProducts(blSerialProduct);
		}
		else
		{
			getBlStockService().findAndUpdateStockRecords(blSerialProduct, false);
		}
	}

	/**
	 * It gets the initial value of the attribute before update
	 *
	 * @param blSerialProduct
	 *           the bl serial product
	 */
	private Object getInitialValue(final BlSerialProductModel blSerialProduct, final String status) {
		final ItemModelContextImpl itemModelCtx = (ItemModelContextImpl) blSerialProduct
				.getItemModelContext();
		return itemModelCtx.exists() ? itemModelCtx.getOriginalValue(status) : null;
	}

	/**
	 * Calculate final sale price for serial.
	 *
	 * @param blSerialProduct
	 *           the bl serial product
	 * @param ctx
	 *           the ctx
	 */
	private void calculateFinalSalePriceForSerial(final BlSerialProductModel blSerialProduct, final InterceptorContext ctx)
	{
		if (BooleanUtils.isTrue(blSerialProduct.getForSale()) && hasForSaleBaseAndConditionalRating(blSerialProduct)
				&& isForSalePriceCalculationRequired(blSerialProduct, ctx))
		{
			blSerialProduct.setFinalSalePrice(getBlPricingService().calculateFinalSalePriceForSerial(
					blSerialProduct.getBlProduct().getForSaleBasePrice(), blSerialProduct.getConditionRatingOverallScore()));
		}
	}

	/**
	 * Check if the sale price is not null and greater than zero and conditional rating is also not zero.
	 *
	 * @param blSerialProduct
	 *           the bl serial product
	 * @return true, if successful
	 */
	private boolean hasForSaleBaseAndConditionalRating(final BlSerialProductModel blSerialProduct)
	{
		BigDecimal forSaleBasePrice = BigDecimal.ZERO;
		final Double conditionRatingOverallScore = blSerialProduct.getConditionRatingOverallScore();
		if(blSerialProduct.getBlProduct() != null) {
			forSaleBasePrice = blSerialProduct.getBlProduct().getForSaleBasePrice();
		}

		return Objects.nonNull(forSaleBasePrice) && Objects.nonNull(conditionRatingOverallScore)
				&& forSaleBasePrice.compareTo(BigDecimal.ZERO) > 0 && conditionRatingOverallScore > 0.0D;
	}

	/**
	 * Checks if For sale price calculation required.
	 *
	 * @param blSerialProduct
	 *           the bl serial product
	 * @param ctx
	 *           the ctx
	 * @return true, if is for sale price calculation required
	 */
	private boolean isForSalePriceCalculationRequired(final BlSerialProductModel blSerialProduct, final InterceptorContext ctx)
	{
		return ctx.isNew(blSerialProduct) || Objects.isNull(blSerialProduct.getFinalSalePrice())
				|| ctx.isModified(blSerialProduct, BlSerialProductModel.FUNCTIONALRATING)
				|| ctx.isModified(blSerialProduct, BlSerialProductModel.COSMETICRATING);
	}

	/**
	 * Calculate incentivized price based on the finalSalePrice.
	 *
	 * @param blSerialProduct
	 *           the bl serial product
	 * @param ctx
	 *           the ctx
	 */
	private void calculateIncentivizedPriceForSerial(final BlSerialProductModel blSerialProduct, final InterceptorContext ctx)
	{
		BlProductModel skuProduct = blSerialProduct.getBlProduct();
		if (Objects.nonNull(blSerialProduct.getFinalSalePrice()) && Objects.nonNull(skuProduct) && Objects.nonNull(skuProduct.getForSaleDiscount()))
		{
			final BigDecimal finalSalePrice = blSerialProduct.getFinalSalePrice().setScale(BlCoreConstants.DECIMAL_PRECISION,
					BlCoreConstants.ROUNDING_MODE);
			final Integer forSaleDiscount = skuProduct.getForSaleDiscount();
			if (finalSalePrice.compareTo(BigDecimal.ZERO) > 0 && forSaleDiscount > 0
					&& isIncentivizedCalculationRequired(blSerialProduct, ctx))
			{
				final BigDecimal calculatedIncentivizedPrice = finalSalePrice.subtract(
						finalSalePrice.multiply(BigDecimal.valueOf(forSaleDiscount)).divide(BigDecimal.valueOf(BlCoreConstants.DIVIDE_BY_HUNDRED))
								.setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE));
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
						"Calculated Incentivized Price is {} for Serial Product {} with For Sale Discount {} and For Sale Final Price {}",
						calculatedIncentivizedPrice, blSerialProduct.getProductId(), forSaleDiscount.intValue(), finalSalePrice.doubleValue());
				blSerialProduct.setIncentivizedPrice(calculatedIncentivizedPrice);
			}
		}
	}

	/**
	 * Checks if Incentivized price calculation required.
	 *
	 * @param blSerialProduct
	 *           the bl serial product
	 * @param ctx
	 *           the ctx
	 * @return true, if is incentivized calculation required
	 */
	private boolean isIncentivizedCalculationRequired(final BlSerialProductModel blSerialProduct, final InterceptorContext ctx)
	{
		return Objects.isNull(blSerialProduct.getIncentivizedPrice())
				|| blSerialProduct.getIncentivizedPrice().compareTo(BigDecimal.ZERO) == 0
				|| ctx.isModified(blSerialProduct, BlSerialProductModel.FINALSALEPRICE) ;
	}
	
	/**
	 * Do status change on consignment once the status of serial is updated.
	 *
	 * @param blSerialProduct
	 *           the bl serial product
	 * @param ctx
	 *           the ctx
	 */
	private void doStatusChangeOnConsignment(final BlSerialProductModel blSerialProduct, final InterceptorContext ctx)
	{
		if (Objects.nonNull(blSerialProduct.getAssociatedConsignment())
				&& Objects.nonNull(blSerialProduct.getSerialStatus())
				&& ctx.isModified(blSerialProduct, BlSerialProductModel.SERIALSTATUS))
		{
			final HashSet<SerialStatusEnum> itemStatuses = Sets.newHashSet(blSerialProduct.getSerialStatus());
			final ConsignmentModel associatedConsignment = blSerialProduct.getAssociatedConsignment();

			associatedConsignment.getConsignmentEntries()
					.forEach(consignmentEntry -> consignmentEntry.getSerialProducts().forEach(entryProduct -> {
						if (entryProduct instanceof BlSerialProductModel 
								&& !entryProduct.getPk().toString().equals(blSerialProduct.getPk().toString()))
						{
							itemStatuses.add(((BlSerialProductModel) entryProduct).getSerialStatus());
						}
					}));
			if (CollectionUtils.isNotEmpty(itemStatuses))
			{
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Statuses found for consignment : {} are {}", 
						associatedConsignment.getCode(), itemStatuses.toString());
				if(itemStatuses.size() == BlCoreConstants.STATUS_LIST_SIZE_ONE)
				{
					doStatusChangeForSingleStatus(itemStatuses.iterator().next(), associatedConsignment, ctx);
				}
				else
				{
					doStatusChangeForMultipleStatuses(itemStatuses, associatedConsignment, ctx);
				}
				checkAndUpdateOrderStatus(associatedConsignment.getOrder(), ctx);
			}
		}
	}

	/**
	 * Do status change on consignment if single status found for items in consignment.
	 *
	 * @param serialStatus
	 *           the serial status
	 * @param associatedConsignment
	 *           the associated consignment
	 */
	private void doStatusChangeForSingleStatus(final SerialStatusEnum serialStatus, final ConsignmentModel associatedConsignment,
			final InterceptorContext ctx)
	{
		if (serialStatus.equals(SerialStatusEnum.RECEIVED_OR_RETURNED) || serialStatus.equals(SerialStatusEnum.IN_HOUSE))
		{
			changeStatusOnConsignment(associatedConsignment, ConsignmentStatus.COMPLETED, ctx);
		}
		else if (serialStatus.equals(SerialStatusEnum.REPAIR_NEEDED))
		{
			changeStatusOnConsignment(associatedConsignment, ConsignmentStatus.INCOMPLETE_ITEMS_IN_REPAIR, ctx);
		}
		else if (serialStatus.equals(SerialStatusEnum.PARTS_NEEDED))
		{
			changeStatusOnConsignment(associatedConsignment, ConsignmentStatus.INCOMPLETE_MISSING_ITEMS, ctx);
		}
	}

	/**
	 * Do status change on consignment if two statuses found for items in consignment.
	 *
	 * @param itemStatuses
	 *           the item statuses
	 * @param associatedConsignment
	 *           the associated consignment
	 */
	private void doStatusChangeForMultipleStatuses(final HashSet<SerialStatusEnum> itemStatuses,
			final ConsignmentModel associatedConsignment, final InterceptorContext ctx)
	{
		if(itemStatuses.size() == BlCoreConstants.STATUS_LIST_SIZE_TWO 
				&& itemStatuses.containsAll(Lists.newArrayList(SerialStatusEnum.RECEIVED_OR_RETURNED, SerialStatusEnum.IN_HOUSE)))
		{
			changeStatusOnConsignment(associatedConsignment, ConsignmentStatus.COMPLETED, ctx);
		}
		else if (itemStatuses.containsAll(Lists.newArrayList(SerialStatusEnum.REPAIR_NEEDED, SerialStatusEnum.PARTS_NEEDED)))
		{
			changeStatusOnConsignment(associatedConsignment, ConsignmentStatus.INCOMPLETE_MISSING_AND_BROKEN_ITEMS, ctx);
		}
		else if (itemStatuses.contains(SerialStatusEnum.PARTS_NEEDED))
		{
			changeStatusOnConsignment(associatedConsignment, ConsignmentStatus.INCOMPLETE_MISSING_ITEMS, ctx);
		}
		else if (itemStatuses.contains(SerialStatusEnum.REPAIR_NEEDED))
		{
			changeStatusOnConsignment(associatedConsignment, ConsignmentStatus.INCOMPLETE_ITEMS_IN_REPAIR, ctx);
		}
	}

	/**
	 * Change status on consignment.
	 *
	 * @param associatedConsignment
	 *           the associated consignment
	 * @param consignmentStatus
	 *           the consignment status
	 */
	private void changeStatusOnConsignment(final ConsignmentModel associatedConsignment, final ConsignmentStatus consignmentStatus,
			final InterceptorContext ctx)
	{
		try
		{
			associatedConsignment.setStatus(consignmentStatus);
			ctx.getModelService().save(associatedConsignment);
			ctx.getModelService().refresh(associatedConsignment);
		}
		catch (final ModelSavingException exception)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception,
					"Error while changing the status on consignment : {}", associatedConsignment.getCode());
		}
	}
	
	/**
	 * Check and update order status.
	 *
	 * @param order the order
	 * @param ctx the ctx
	 */
	private void checkAndUpdateOrderStatus(final AbstractOrderModel order, final InterceptorContext ctx)
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
					doChangeOrderStatusForSingleStatus(order, ctx, itemStatuses);
				}
				else
				{
					doChangeOrderStatusForMultipleStatuses(order, ctx, itemStatuses);
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
	private void doChangeOrderStatusForSingleStatus(final AbstractOrderModel order, final InterceptorContext ctx,
			final HashSet<ConsignmentStatus> itemStatuses)
	{
		final ConsignmentStatus consignmentStatus = itemStatuses.iterator().next();
		if(ConsignmentStatus.COMPLETED.equals(consignmentStatus))
		{
			changeStatusOnOrder(order, OrderStatus.COMPLETED, ctx);
		}
		else if(ConsignmentStatus.PARTIALLY_UNBOXED.equals(consignmentStatus))
		{
			changeStatusOnOrder(order, OrderStatus.PARTIALLY_UNBOXED, ctx);
		}
		else if(ConsignmentStatus.UNBOXED.equals(consignmentStatus))
		{
			changeStatusOnOrder(order, OrderStatus.UNBOXED, ctx);
		}
		else if(ConsignmentStatus.INCOMPLETE_ITEMS_IN_REPAIR.equals(consignmentStatus))
		{
			changeStatusOnOrder(order, OrderStatus.INCOMPLETE_ITEMS_IN_REPAIR, ctx);
		}
		else if(ConsignmentStatus.INCOMPLETE_MISSING_ITEMS.equals(consignmentStatus))
		{
			changeStatusOnOrder(order, OrderStatus.INCOMPLETE_MISSING_ITEMS, ctx);
		}
		else if(ConsignmentStatus.INCOMPLETE_MISSING_AND_BROKEN_ITEMS.equals(consignmentStatus))
		{
			changeStatusOnOrder(order, OrderStatus.INCOMPLETE_MISSING_AND_BROKEN_ITEMS, ctx);
		}
	}
	
	/**
	 * Do change order status for multiple statuses.
	 *
	 * @param order the order
	 * @param ctx the ctx
	 * @param itemStatuses the item statuses
	 */
	private void doChangeOrderStatusForMultipleStatuses(final AbstractOrderModel order, final InterceptorContext ctx,
			final HashSet<ConsignmentStatus> itemStatuses)
	{
		if(itemStatuses.contains(ConsignmentStatus.INCOMPLETE_MISSING_AND_BROKEN_ITEMS))
		{
			changeStatusOnOrder(order, OrderStatus.INCOMPLETE_MISSING_AND_BROKEN_ITEMS, ctx);
		}
		else if(itemStatuses.contains(ConsignmentStatus.INCOMPLETE_ITEMS_IN_REPAIR) 
				&& itemStatuses.contains(ConsignmentStatus.INCOMPLETE_MISSING_ITEMS))
		{
			changeStatusOnOrder(order, OrderStatus.INCOMPLETE_MISSING_AND_BROKEN_ITEMS, ctx);
		}
		else if(itemStatuses.contains(ConsignmentStatus.INCOMPLETE_ITEMS_IN_REPAIR))
		{
			changeStatusOnOrder(order, OrderStatus.INCOMPLETE_ITEMS_IN_REPAIR, ctx);
		}
		else if(itemStatuses.contains(ConsignmentStatus.INCOMPLETE_MISSING_ITEMS))
		{
			changeStatusOnOrder(order, OrderStatus.INCOMPLETE_MISSING_ITEMS, ctx);
		}
		else if(itemStatuses.contains(ConsignmentStatus.PARTIALLY_UNBOXED))
		{
			changeStatusOnOrder(order, OrderStatus.PARTIALLY_UNBOXED, ctx);
		}
	}
	
	/**
	 * Change status on order.
	 *
	 * @param order the order
	 * @param orderStatus the order status
	 * @param ctx the ctx
	 */
	private void changeStatusOnOrder(final AbstractOrderModel order, final OrderStatus orderStatus,
			final InterceptorContext ctx)
	{
		try
		{
			order.setStatus(orderStatus);
			ctx.getModelService().save(order);
			ctx.getModelService().refresh(order);
		}
		catch (final ModelSavingException exception)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception,
					"Error while changing the status on order : {}", order.getCode());
		}
	}
	
	/**
	 * Creates the repair log if repair needed.
	 *
	 * @param blSerialProduct
	 *           the bl serial product
	 * @param ctx
	 *           the ctx
	 */
	private void createRepairLogIfRepairNeeded(final BlSerialProductModel blSerialProduct, final InterceptorContext ctx)
	{
		if (isEligibleForRepairLogCreation(blSerialProduct, ctx)
				&& isRepairLogTypeAvailable(blSerialProduct))
		{
			switch (blSerialProduct.getRepairLogType().getCode())
			{
				case BlCoreConstants.IN_HOUSE_REPAIR:
					BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlCoreConstants.CREATING_REPAIR_LOG_MESSAGE,
							blSerialProduct.getRepairLogType().getCode());
					getBlRepairLogService().addGeneratedRepairLog(InHouseRepairLogModel.class, blSerialProduct);
					break;
				case BlCoreConstants.VENDOR_REPAIR:
					BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlCoreConstants.CREATING_REPAIR_LOG_MESSAGE,
							blSerialProduct.getRepairLogType().getCode());
					getBlRepairLogService().addGeneratedRepairLog(VendorRepairLogModel.class, blSerialProduct);
					break;
				case BlCoreConstants.CUSTOMER_RESPONSIBLE_REPAIR:
					BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlCoreConstants.CREATING_REPAIR_LOG_MESSAGE,
							blSerialProduct.getRepairLogType().getCode());
					getBlRepairLogService().addGeneratedRepairLog(CustomerResponsibleRepairLogModel.class, blSerialProduct);
					break;
				case BlCoreConstants.PARTS_NEEDED_REPAIR:
					BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlCoreConstants.CREATING_REPAIR_LOG_MESSAGE,
							blSerialProduct.getRepairLogType().getCode());
					getBlRepairLogService().addGeneratedRepairLog(PartsNeededRepairLogModel.class, blSerialProduct);
					break;
				default:
					BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
							"New Repair Log Type Found : {} , Please try to create Repair Log manually",
							blSerialProduct.getRepairLogType().getCode());
					break;
			}
		}
	}

	/**
	 * Checks if it is eligible for repair log creation.
	 *
	 * @param blSerialProduct the bl serial product
	 * @param ctx the ctx
	 * @return true, if is eligible for repair log creation
	 */
	private boolean isEligibleForRepairLogCreation(final BlSerialProductModel blSerialProduct, final InterceptorContext ctx)
	{
		return (ctx.isModified(blSerialProduct, BlSerialProductModel.SERIALSTATUS)
				|| ctx.isModified(blSerialProduct, BlSerialProductModel.REPAIRLOGTYPE))
				&& (SerialStatusEnum.REPAIR_NEEDED.equals(blSerialProduct.getSerialStatus())
						|| SerialStatusEnum.PARTS_NEEDED.equals(blSerialProduct.getSerialStatus()));
	}

	/**
	 * Checks if Repair log type available on item.
	 *
	 * @param blSerialProduct the bl serial product
	 * @return true, if is repair log type available
	 */
	private boolean isRepairLogTypeAvailable(final BlSerialProductModel blSerialProduct)
	{
		return Objects.nonNull(blSerialProduct.getRepairLogType())
				&& BooleanUtils.negate(RepairTypeEnum.NONE.equals(blSerialProduct.getRepairLogType()))
				&& SerialStatusEnum.REPAIR_NEEDED.equals(blSerialProduct.getSerialStatus());
	}

	/**
	 *
	 * Gets the bl pricing service.
	 *
	 * @return the bl pricing service
	 */
	public BlPricingService getBlPricingService()
	{
		return blPricingService;
	}

	/**
	 * Sets the bl pricing service.
	 *
	 * @param blPricingService
	 *           the new bl pricing service
	 */
	public void setBlPricingService(final BlPricingService blPricingService)
	{
		this.blPricingService = blPricingService;
	}

	/**
	 * @return the blStockService
	 */
	public BlStockService getBlStockService()
	{
		return blStockService;
	}

	/**
	 * @param blStockService
	 *           the blStockService to set
	 */
	public void setBlStockService(final BlStockService blStockService)
	{
		this.blStockService = blStockService;
	}

	/**
	 * @return the blRepairLogService
	 */
	public BlRepairLogService getBlRepairLogService()
	{
		return blRepairLogService;
	}

	/**
	 * @param blRepairLogService the blRepairLogService to set
	 */
	public void setBlRepairLogService(BlRepairLogService blRepairLogService)
	{
		this.blRepairLogService = blRepairLogService;
	}

}
