package com.bl.Ordermanagement.services.impl;

import com.bl.Ordermanagement.actions.order.BlSourceOrderAction;
import com.bl.Ordermanagement.constants.BlOrdermanagementConstants;
import com.bl.Ordermanagement.services.BlAllocationService;
import com.bl.Ordermanagement.services.BlSourcingService;
import com.bl.constants.BlloggingConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.ItemStatusEnum;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlItemsBillingChargeModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.BlSubpartsModel;
import com.bl.core.order.dao.BlOrderDao;
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
import de.hybris.platform.ordersplitting.model.ConsignmentProcessModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.task.TaskConditionModel;
import de.hybris.platform.warehousing.data.sourcing.SourcingResult;
import de.hybris.platform.warehousing.data.sourcing.SourcingResults;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
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
	private CalculationService calculationService;
	private BlAllocationService blAllocationService;
	private BlOrderDao blOrderDao;

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
			final String orderEntrySkuPk = previousChangedOrderEntry.getProduct().getPk().toString();
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
		final Set<ConsignmentProcessModel> consignmentProcessesToRemove = new HashSet<>();
		final Set<TaskConditionModel> taskConditions = new HashSet<>();
		removeConsignment(orderModel, consignmentToRemove, consignmentProcessesToRemove, taskConditions);
		getModelService().removeAll(consignmentToRemove);
		getModelService().removeAll(consignmentProcessesToRemove);
		getModelService().removeAll(taskConditions);
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
					final boolean isRentalOrder = consignment.getOrder().getIsRentalOrder();
					updateStockForSerial(consignment.getOptimizedShippingStartDate(),
							isRentalOrder ? consignment.getOptimizedShippingEndDate() : BlDateTimeUtils.getNextYearsSameDay(), serial, !isRentalOrder);
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
	 * @param orderModel the order model
	 * @param consignmentToRemove the consignment to remove
	 * @param consignmentProcessesToRemove list of consignment process to remove
	 * @param taskConditions list of associated task conditions
	 */
	public void removeConsignment(final OrderModel orderModel, final List<ConsignmentModel> consignmentToRemove,
			final Set<ConsignmentProcessModel> consignmentProcessesToRemove, final Set<TaskConditionModel> taskConditions)
	{
		for (final ConsignmentModel consignment : orderModel.getConsignments())
		{
			getModelService().refresh(consignment);
			if (CollectionUtils.isEmpty(consignment.getConsignmentEntries()))
			{
				consignmentToRemove.add(consignment);
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Consignment {} removed for order {}", consignment.getCode(),consignment.getOrder());
				consignmentProcessesToRemove.addAll(consignment.getConsignmentProcesses());
				consignmentProcessesToRemove.forEach(consignmentProcessModel -> {
						taskConditions.addAll(getBlOrderDao().getTaskCondition(consignment.getCode()));
				});
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
		if (consignmentEntryModel.getConsignment().getOrder().getIsRentalOrder())
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
	 * @param serial
	 */
	public void updateStockForSerial(final Date optimizedShippingStartDate, final Date optimizedShippingEndDate, final BlProductModel serial,final boolean isUsedGearOrder)
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
					getModelService().save(stockLevel);
				});
				if(isUsedGearOrder)
				{
					((BlSerialProductModel) serial).setSerialStatus(SerialStatusEnum.ACTIVE);
				}
				((BlSerialProductModel) serial).setHardAssigned(false);
				getModelService().save(serial);
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
		final OrderModel order = orderEntryModel.getOrder();
		final SourcingResult result =
				CollectionUtils.isNotEmpty(sourcingResults.getResults()) ? sourcingResults.getResults().iterator().next() :
						new SourcingResult();
		int size = order.getConsignments().size();
		if(CollectionUtils.isNotEmpty(order.getConsignments()) && order.getConsignments().iterator().next().getCode().contains("_1")) {
				size = 0;
		}
		final ConsignmentModel consignment = getBlAllocationService().createConsignment(order,
				BlCoreConstants.CONSIGNMENT_PROCESS_PREFIX + order.getCode()
						+ BlOrdermanagementConstants.UNDER_SCORE
						+ size, result);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Consignment created for order {}",order.getCode());

		if(Objects.nonNull(consignment) && order.getIsRentalOrder()) {
				if (CollectionUtils.isNotEmpty(consignment.getConsignmentEntries())) {
					final ConsignmentEntryModel consEntry = consignment.getConsignmentEntries()
							.iterator().next();
					//TODO: check it for AQuatech quantity assignment as earlier value was hardcoded 1
					consEntry.setQuantity(Long.valueOf(result.getSerialProductMap().get(orderEntryModel.getEntryNumber()).size()));
					getModelService().save(consEntry);
				}
				if(CollectionUtils.isNotEmpty(orderEntryModel.getModifiedSerialProductList())) {
					final List<BlProductModel> assignedSerialProducts = new ArrayList<>(
							orderEntryModel.getSerialProducts());
					assignedSerialProducts.addAll(orderEntryModel.getModifiedSerialProductList());
					orderEntryModel.setSerialProducts(assignedSerialProducts);
				}
		}

		order.getOrderProcess().forEach(orderProcess -> {
			if(BlOrdermanagementConstants.ORDER_PROCESS.equals(orderProcess.getProcessDefinitionName()))
			{
				getBlSourceOrderAction().startConsignmentSubProcess(Collections.singletonList(consignment), orderProcess);
			}

		});
		getModelService().refresh(consignment);
		getModelService().refresh(order);
		recalculateOrder(order);
	}


	/**
	 * This method will be used to recalculate order
	 * @param order as order
	 */
	public void recalculateOrder(final AbstractOrderModel order)
	{
		order.setCalculated(false);
		try
		{
			getModelService().save(order);
			getModelService().refresh(order);
			getCalculationService().recalculate(order);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Order {} recalculated successfully", order.getCode());
		}
		catch (final CalculationException cx)
		{
			 BlLogger.logMessage(LOG, Level.ERROR, "Exception occur while recalculating order {} ",
					 order.getCode(), cx);
		}
		catch (final Exception e)
		{
			BlLogger.logMessage(LOG, Level.ERROR, "Exception occur while trying to save order {} ",
					order.getCode(), e);
		}
	}

	/**
	 * It removes the serial products and associated subpart products as serial products are removed in order entry
	 * @param orderModel the order model
	 * @param serialProduct the serial product
	 * @param consignmentEntriesToRemove the consignment entries to remove
	 * @return the list of consignment entry to remove
	 */
	public List<ConsignmentEntryModel> removeConsignmentEntries(final OrderModel orderModel, final BlSerialProductModel serialProduct,
			final List<ConsignmentEntryModel> consignmentEntriesToRemove) {
		for(final ConsignmentModel consignmentModel : orderModel.getConsignments()) {
			for(final ConsignmentEntryModel consignmentEntry : consignmentModel.getConsignmentEntries()) {
				if(consignmentEntry.getSerialProducts().contains(serialProduct)) {
					if(consignmentEntry.getSerialProducts().stream().filter(blSerialProduct ->
							blSerialProduct instanceof BlSerialProductModel).collect(Collectors.toList()).size() > 1) {
						final List<BlProductModel> products = new ArrayList<>(consignmentEntry.getSerialProducts());
						final Map<String, ItemStatusEnum> itemMap = new HashMap<>(consignmentEntry.getItems());
						final Map<String, List<BlItemsBillingChargeModel>> billingCharges = new HashMap<>(consignmentEntry.getBillingCharges());
						billingCharges.entrySet().removeIf(code -> code.getKey().equals(serialProduct.getCode()));
						consignmentEntry.setBillingCharges(billingCharges);
						products.remove(serialProduct);
						consignmentEntry.setQuantity(consignmentEntry.getQuantity()-1);
						updateConsignmentEntry(serialProduct, consignmentEntry, products, itemMap);
						updateStockForSerial(consignmentModel.getOptimizedShippingStartDate(), consignmentModel.getOptimizedShippingEndDate(),
								serialProduct, false);
					} else {
						consignmentEntriesToRemove.add(consignmentEntry);
						updateStockForSerial(consignmentModel.getOptimizedShippingStartDate(), consignmentModel.getOptimizedShippingEndDate(),
								serialProduct, false);
					}
					break;
				}
			}
		}
		return consignmentEntriesToRemove;
	}

	/**
	 * It removes the serial products and associated subpart products as serial products are removed in order entry
	 * @param serialProduct the serial product
	 * @param consignmentEntry the consignment entry
	 * @param products the products
	 * @param itemMap the item map
	 */
	private void updateConsignmentEntry(final BlSerialProductModel serialProduct,
			final ConsignmentEntryModel consignmentEntry, final List<BlProductModel> products, final Map<String, ItemStatusEnum> itemMap) {
		itemMap.entrySet().removeIf(item -> item.getKey().contains(serialProduct.getCode()));
		final BlProductModel skuProduct = serialProduct.getBlProduct();
		final Collection<BlSubpartsModel> subpartProducts = skuProduct.getSubpartProducts();
		subpartProducts.forEach(subpartProduct -> {
			final BlProductModel subpart = subpartProduct.getSubpartProduct();
			for(int count = subpartProduct.getQuantity(); count > 0; count--) {
				products.remove(subpart);
				Iterator<String> subpartsToRemove = itemMap.keySet().iterator();
				while(subpartsToRemove.hasNext()) {
					final String subpartCode = subpartsToRemove.next();
					if(subpartCode.contains(subpart.getName())) {
						subpartsToRemove.remove();
						break;
					}
				}
			}
		});
		consignmentEntry.setSerialProducts(products);
		consignmentEntry.setItems(itemMap);
		getModelService().save(consignmentEntry);
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

	public BlAllocationService getBlAllocationService() {
		return blAllocationService;
	}

	public void setBlAllocationService(
			BlAllocationService blAllocationService) {
		this.blAllocationService = blAllocationService;
	}

	public BlOrderDao getBlOrderDao() {
		return blOrderDao;
	}

	public void setBlOrderDao(BlOrderDao blOrderDao) {
		this.blOrderDao = blOrderDao;
	}

}
