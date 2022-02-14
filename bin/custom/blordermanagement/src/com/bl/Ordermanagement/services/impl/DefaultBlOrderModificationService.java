package com.bl.Ordermanagement.services.impl;

import java.util.*;

import com.bl.Ordermanagement.actions.order.BlSourceOrderAction;
import com.bl.Ordermanagement.constants.BlOrdermanagementConstants;
import com.bl.Ordermanagement.services.BlSourcingService;

import com.bl.constants.BlloggingConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.order.CalculationService;
import de.hybris.platform.order.exceptions.CalculationException;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.warehousing.allocation.AllocationService;
import de.hybris.platform.warehousing.data.sourcing.SourcingResult;
import de.hybris.platform.warehousing.data.sourcing.SourcingResults;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This service used for any modification on order
 *
 * @author Aditi Sharma
 */
public class DefaultBlOrderModificationService
{
	private static final Logger LOG = Logger.getLogger(DefaultBlOrderModificationService.class);
	private BlSourcingService blSourcingService;
	private BlSourceOrderAction blSourceOrderAction;
	private ModelService modelService;
	private BlStockLevelDao blStockLevelDao;
	private AllocationService allocationService;
	private CalculationService calculationService;
	
	/**
	 * This method will be used to get sourcing result for used gear orders
	 * @param entry as orderEntry
	 * @return SourcingResults as sourcingResult
	 */
	public SourcingResults getResultsForUsedGearOrder(final OrderEntryModel entry) {

		final SourcingResults results = new SourcingResults();
		final Set<SourcingResult> resultSet = new HashSet<>();
		final Map<WarehouseModel, SourcingResult> warehouseSourcingResultMap = new HashMap<>();

		final WarehouseModel warehouseModel = ((BlSerialProductModel) entry.getProduct())
				.getWarehouseLocation();
		if (null == warehouseSourcingResultMap.get(warehouseModel)) {
			final SourcingResult sourcingResult = new SourcingResult();
			getBlSourceOrderAction().updateResultAndAssignSerials(resultSet, entry, warehouseModel, sourcingResult);
			warehouseSourcingResultMap.put(warehouseModel, sourcingResult);
		} else {
			getBlSourceOrderAction().updateResultAndAssignSerials(resultSet, entry, warehouseModel,
					warehouseSourcingResultMap.get(warehouseModel));
		}
		results.setResults(resultSet);
		final Calendar calendar = Calendar.getInstance();
		calendar.setTime(new Date());
		calendar.add(Calendar.DATE, BlOrdermanagementConstants.TWO);
		entry.getOrder().setActualRentalStartDate(calendar.getTime());
		getBlSourcingService().updateShippingDatesForInternalTransfers(entry.getOrder(), results);

		return results;
	}

	/**
	 * This method is used to create sourcing result for used gear order
	 * @param orderEntryModel as orderEntryModel
	 * @param resultsForUsedGearOrder as resultsForUsedGearOrder
	 * @return SourcingResult as sourcingResult
	 */
	public SourcingResult createSourcingResultForUsedGear(final OrderEntryModel orderEntryModel,
														   final SourcingResults resultsForUsedGearOrder)
	{
		final SourcingResult sourcingResult = new SourcingResult();
		resultsForUsedGearOrder.getResults().forEach(usedGearSourceResult-> {
			if(usedGearSourceResult.getAllocation().get(orderEntryModel) !=null)
			{
				sourcingResult.setAllocation(usedGearSourceResult.getAllocation());
				sourcingResult.setWarehouse(usedGearSourceResult.getWarehouse());
				sourcingResult.setSerialProductMap(usedGearSourceResult.getSerialProductMap());
			}
		});
		return sourcingResult;
	}


	/**
	 * method is used to remove entry from consignment
	 *
	 * @param orderModel as orderModel
	 * @param previousChangedOrderEntrysList as previousChangedOrderEntrysList
	 */
	public void removeEntryFromConsignment(final OrderModel orderModel,
											final List<AbstractOrderEntryModel> previousChangedOrderEntrysList)
	{
		if (CollectionUtils.isNotEmpty(previousChangedOrderEntrysList))
		{
			final AbstractOrderEntryModel previousChangedOrderEntry = previousChangedOrderEntrysList.iterator().next();
			String orderEntrySkuPk = previousChangedOrderEntry.getProduct().getPk().toString();
			final List<ConsignmentEntryModel> consignmentEntryToRemove = new ArrayList<>();
			final List<ConsignmentModel> consignmentToRemove = new ArrayList<>();

			getConsignmentToRemove(orderModel,orderEntrySkuPk,consignmentEntryToRemove,consignmentToRemove);
		}
	}
	/**
	 * This method will be used to get the consignment entry which is removed by CS Agent
	 * @param orderModel as orderModel
	 * @param orderEntrySkuPk as orderEntrySkuPk
	 * @param consignmentEntryToRemove as consignmentEntryToRemove
	 * @param consignmentToRemove as consignmentToRemove
	 */
	public void getConsignmentToRemove(final OrderModel orderModel, final String orderEntrySkuPk,final List<ConsignmentEntryModel> consignmentEntryToRemove,final List<ConsignmentModel> consignmentToRemove)
	{
		for (final ConsignmentModel consignment : orderModel.getConsignments())
		{
			removeConsignmentEntry(orderEntrySkuPk, consignmentEntryToRemove, consignment);
		}
		getModelService().removeAll(consignmentEntryToRemove);
		removeConsignment(orderModel, consignmentToRemove);
		getModelService().removeAll(consignmentToRemove);
		orderModel.setOrderModifiedDate(new Date());
		orderModel.setUpdatedTime(new Date());
	}
	/**
	 * method is used remove consignment entry if no serial is available in it
	 *
	 * @param orderEntrySkuPk
	 * @param consignmentEntryToRemove
	 * @param consignment
	 */
	private void removeConsignmentEntry(final String orderEntrySkuPk, final List<ConsignmentEntryModel> consignmentEntryToRemove,
										final ConsignmentModel consignment)
	{
		for (final ConsignmentEntryModel consignmentEntry : consignment.getConsignmentEntries())
		{
			final List<BlProductModel> updatedSerialList = new ArrayList<>();
			consignmentEntry.getSerialProducts().forEach(serial -> {
				if (serial instanceof BlSerialProductModel
						&& !orderEntrySkuPk.equals(getPkFromProduct(consignmentEntry, (BlSerialProductModel) serial)))
				{
					updatedSerialList.add(serial);
				}
				else 
				{
					boolean isUsedGearOrder = consignment.getOrder().getIsRentalCart();
					updateStockForSerial(consignment.getOptimizedShippingStartDate(),
							isUsedGearOrder ? consignment.getOptimizedShippingEndDate() : BlDateTimeUtils.getNextYearsSameDay(), serial,isUsedGearOrder);
				}
			});
			if (CollectionUtils.isEmpty(updatedSerialList))
			{
				consignmentEntryToRemove.add(consignmentEntry);
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Consignment Entry {} removed for consignment {} on order {}", consignmentEntry,consignment.getCode(),consignment.getOrder().getCode());
			}
			else
			{
				consignmentEntry.setSerialProducts(updatedSerialList);
				getModelService().save(consignmentEntry);
				getModelService().refresh(consignmentEntry);
			}
		}
	}

	/**
	 * method is used to remove consignment if all the consignment entries has been removed
	 *
	 * @param orderModel
	 * @param consignmentToRemove
	 */
	private void removeConsignment(final OrderModel orderModel, final List<ConsignmentModel> consignmentToRemove)
	{
		for (final ConsignmentModel consignment : orderModel.getConsignments())
		{
			getModelService().refresh(consignment);
			if (CollectionUtils.isEmpty(consignment.getConsignmentEntries()))
			{
				consignmentToRemove.add(consignment);
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Consignment {} removed for order {}", consignment.getCode(),consignment.getOrder());
			}
		}
	}

	/**
	 * method is used to get pk from product
	 *
	 * @param consignmentEntryModel
	 * @param serial
	 */
	private String getPkFromProduct(final ConsignmentEntryModel consignmentEntryModel,final BlSerialProductModel serial)
	{
		if (consignmentEntryModel.getConsignment().getOrder().getIsRentalCart())
		{
			return serial.getBlProduct().getPk().toString();
		}
		else
		{
			return serial.getPk().toString();
		}
	}

	/**
	 * method is used to update stock from removed entry
	 *
	 * @param consignment
	 * @param serial
	 */
	private void updateStockForSerial(final Date optimizedShippingStartDate, final Date optimizedShippingEndDate, final BlProductModel serial,final boolean isUsedGearOrder)
	{
		if (serial instanceof BlSerialProductModel)
		{
			final Collection<StockLevelModel> findSerialStockLevelForDate = getBlStockLevelDao().findSerialStockLevelForDate(
					serial.getCode(), optimizedShippingStartDate, optimizedShippingEndDate);
			if (CollectionUtils.isNotEmpty(findSerialStockLevelForDate))
			{
				findSerialStockLevelForDate.forEach(stockLevel -> {
					final BlSerialProductModel serialProductModel = ((BlSerialProductModel) serial);
					stockLevel.setHardAssigned(false);
					stockLevel.setReservedStatus(false);
					stockLevel.setOrder(null);
					serialProductModel.setHardAssigned(false);
					if(isUsedGearOrder)
					{
						serialProductModel.setSerialStatus(SerialStatusEnum.ACTIVE);
					}
					getModelService().save(stockLevel);
					getModelService().save(serial);
				});
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Stock level updated for serial {}", serial);
			}
		}
	}
	/** This method is used to check of consignment is present or not
	 * @param orderEntryModel as orderEntryModel
	 * @param sourceResult as sourceResult
	 * @return Optional<ConsignmentModel> as consignmentModel
	 */
	public Optional<ConsignmentModel> checkifConsignmentIsPresent(final OrderEntryModel orderEntryModel,final SourcingResult sourceResult)
	{
		return orderEntryModel.getOrder().getConsignments().stream()
				.filter(consignment -> consignment.getWarehouse().getCode().equals(sourceResult.getWarehouse().getCode())).findFirst();
	}

	/**
	 * method is used to get the original value for order entry
	 *
	 * @param orderModel as orderModel
	 * @return List<AbstractOrderEntryModel> as orderEntryModelList
	 */
	public List<AbstractOrderEntryModel> getPreviousChangedOrderEntrysList(final AbstractOrderModel orderModel)
	{
		final Object previousValue = orderModel.getItemModelContext().getOriginalValue(BlloggingConstants.ORIGINAL_VALUE);
		if (previousValue instanceof List)
		{
			return Lists.newArrayList((List) previousValue);
		}
		return Collections.emptyList();
	}

	/**
	 * This method is used to create consignment for modified order
	 * @param orderEntryModel as orderEntryModel
	 * @param  sourcingResults as sourcingResults
	 */
	public void createConsignmentForModifiedOrder(final OrderEntryModel orderEntryModel,final SourcingResults sourcingResults )
	{
		Collection<ConsignmentModel> consignment = getAllocationService().createConsignments(
				orderEntryModel.getOrder(),
				BlCoreConstants.CONSIGNMENT_PROCESS_PREFIX + orderEntryModel.getOrder().getCode(), sourcingResults);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Consignment created for order {}",orderEntryModel.getOrder().getCode());

		orderEntryModel.getOrder().getOrderProcess().forEach(orderProcess -> {
			if(BlOrdermanagementConstants.ORDER_PROCESS.equals(orderProcess.getProcessDefinitionName()))
			{
				getBlSourceOrderAction().startConsignmentSubProcess(consignment, orderProcess);
			}

		});
		recalculateOrder(orderEntryModel.getOrder());
	}


	/**
	 * This method will be used to recalculate order
	 * @param order as order
	 */
	public void recalculateOrder(final AbstractOrderModel order)
	{
		order.setCalculated(false);
		getModelService().save(order);
		getModelService().refresh(order);
		try
		{
			getCalculationService().recalculate(order);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Order {} recalculated successfully", order.getCode());
		}
		catch (final CalculationException cx)
		{
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR, "Exception {} occur while recalculating order {} ", cx.getMessage(),order.getCode());
		}
	}

	/**
	 * @return the blSourcingService
	 */
	public BlSourcingService getBlSourcingService()
	{
		return blSourcingService;
	}

	/**
	 * @param blSourcingService the blSourcingService to set
	 */
	public void setBlSourcingService(BlSourcingService blSourcingService)
	{
		this.blSourcingService = blSourcingService;
	}

	/**
	 * @return the blSourceOrderAction
	 */
	public BlSourceOrderAction getBlSourceOrderAction()
	{
		return blSourceOrderAction;
	}

	/**
	 * @param blSourceOrderAction the blSourceOrderAction to set
	 */
	public void setBlSourceOrderAction(BlSourceOrderAction blSourceOrderAction)
	{
		this.blSourceOrderAction = blSourceOrderAction;
	}

	public CalculationService getCalculationService() {
		return calculationService;
	}

	public void setCalculationService(CalculationService calculationService) {
		this.calculationService = calculationService;
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
	 * @return the blStockLevelDao
	 */
	public BlStockLevelDao getBlStockLevelDao()
	{
		return blStockLevelDao;
	}

	/**
	 * @param blStockLevelDao the blStockLevelDao to set
	 */
	public void setBlStockLevelDao(BlStockLevelDao blStockLevelDao)
	{
		this.blStockLevelDao = blStockLevelDao;
	}

	/**
	 * @return the allocationService
	 */
	public AllocationService getAllocationService()
	{
		return allocationService;
	}

	/**
	 * @param allocationService the allocationService to set
	 */
	public void setAllocationService(AllocationService allocationService)
	{
		this.allocationService = allocationService;
	}

}
