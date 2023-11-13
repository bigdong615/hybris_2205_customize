package com.bl.Ordermanagement.reallocation.impl;

import com.bl.Ordermanagement.constants.BlOrdermanagementConstants;
import com.bl.Ordermanagement.reallocation.BlReallocationService;
import com.bl.Ordermanagement.services.BlAllocationService;
import com.bl.Ordermanagement.services.BlAssignSerialService;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.services.consignment.entry.BlConsignmentEntryService;
import com.bl.core.services.customer.impl.DefaultBlUserService;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.logging.BlLogger;
import com.google.common.collect.Sets;

import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.orderprocessing.model.OrderProcessModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.ConsignmentProcessModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.processengine.BusinessProcessService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.warehousing.constants.WarehousingConstants;
import de.hybris.platform.warehousing.data.sourcing.SourcingContext;
import de.hybris.platform.warehousing.data.sourcing.SourcingLocation;
import de.hybris.platform.warehousing.data.sourcing.SourcingResult;
import de.hybris.platform.warehousing.data.sourcing.SourcingResults;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import javax.annotation.Resource;

public class DefaultBlReallocationService implements BlReallocationService {

  private static final Logger LOG = Logger.getLogger(
      DefaultBlReallocationService.class);
  private ModelService modelService;
  private BlAssignSerialService blAssignSerialService;
  private BlAllocationService blAllocationService;
  private BlConsignmentEntryService blConsignmentEntryService;
  private BlStockLevelDao blStockLevelDao;
  private BusinessProcessService businessProcessService;
  @Resource(name = "defaultBlUserService")
  private DefaultBlUserService defaultBlUserService;

  /**
   * {@inheritDoc}
   */
  public SourcingContext createSourcingContext(
      final Collection<AbstractOrderEntryModel> orderEntries) {

    final SourcingContext context = new SourcingContext();
    context.setOrderEntries(orderEntries);
    final SourcingResults result = new SourcingResults();
    result.setResults(Sets.newHashSet());
    result.setComplete(Boolean.FALSE);
    context.setResult(result);

    return context;
  }

  /**
   * {@inheritDoc}
   */
  public SourcingLocation createSourcingLocation(
      final Map<String, List<StockLevelModel>> availabilityMap,
      final WarehouseModel warehouseModel, final SourcingContext context) {

    final SourcingLocation sourcingLocation = new SourcingLocation();
    sourcingLocation.setWarehouse(warehouseModel);
    sourcingLocation.setContext(context);
    sourcingLocation.setAvailabilityMap(availabilityMap);

    final Set<SourcingLocation> sourcingLocations = new HashSet<>();
    sourcingLocations.add(sourcingLocation);
    context.setSourcingLocations(sourcingLocations);

    return sourcingLocation;
  }


  /**
   * {@inheritDoc}
   */
  public void assignSerialFromLocation(final SourcingContext context) {

    final List<AtomicBoolean> allEntrySourceComplete = new ArrayList<>();
    SourcingResult result = new SourcingResult();
    final SourcingLocation finalSourcingLocation = context.getSourcingLocations().iterator().next();
    context.getOrderEntries().forEach(entry ->

        getBlAssignSerialService()
        .fulfillEachEntry(context, result, finalSourcingLocation, entry, allEntrySourceComplete,
            entry.getUnAllocatedQuantity()));
  }

  /**
   * {@inheritDoc}
   */
  public void createConsignment(final AbstractOrderModel order, final SourcingContext context,
      final WarehouseModel warehouseModel) {

    final Set<SourcingResult> sourcingResults = context.getResult().getResults();
    final SourcingResult result =
        CollectionUtils.isNotEmpty(sourcingResults) ? sourcingResults.iterator().next() :
            new SourcingResult();
    final ConsignmentModel consignment = getConsignment(warehouseModel, order);

    if (Objects.isNull(consignment)) {

      final ConsignmentModel newConsignmentModel = getBlAllocationService().createConsignment(order,
          BlCoreConstants.CONSIGNMENT_PROCESS_PREFIX + order.getCode()
              + BlOrdermanagementConstants.UNDER_SCORE
              + order.getConsignments().size(),
          result);

      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
          "A new consignment has been created for consignment reallocation ");

      final List<ConsignmentModel> consignmentModelList = new ArrayList<>();
      consignmentModelList.add(newConsignmentModel);
      startConsignmentSubProcess(consignmentModelList, getOrderProcess((OrderModel)order));

    } else {
      final List<AbstractOrderEntryModel> orderEntries = new ArrayList<>();
      context.getOrderEntries().forEach(orderEntry ->
          consignment.getConsignmentEntries().forEach(consignmentEntryModel -> {
            if (consignmentEntryModel.getOrderEntry().equals(orderEntry)) {
              updateConsignmentEntry(consignmentEntryModel, result, orderEntry);
              orderEntries.add(orderEntry);
              BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
                  "This consignment entry already exists {} of the consignment {}", consignmentEntryModel, consignment);
            }
          }));
      final List<AbstractOrderEntryModel> contextOrderEntries = new ArrayList<>(context.getOrderEntries());
      contextOrderEntries.removeAll(orderEntries);
      context.setOrderEntries(contextOrderEntries);
      final SourcingResult sourcingResult = context.getResult().getResults().iterator().next();
      final Set<ConsignmentEntryModel> entries = context.getOrderEntries().stream()
          .map(entryModel ->
              getBlAllocationService()
                  .createConsignmentEntry(entryModel, Long.valueOf(result.getSerialProductMap()
                          .get(entryModel.getEntryNumber()).size()), consignment,
                      sourcingResult)
          ).collect(Collectors.toSet());
      entries.forEach( consignmentEntryModel -> {
        final Set<BlSerialProductModel> serialProductModels =
            null == result.getSerialProductMap() ? new HashSet<>() : result.getSerialProductMap()
                .get(consignmentEntryModel.getOrderEntry().getEntryNumber());
        reserveStocksForSerialProducts(serialProductModels, consignmentEntryModel);
      });
      entries.addAll(consignment.getConsignmentEntries());
      consignment.setConsignmentEntries(entries);
      consignment.setStatus(ConsignmentStatus.READY);
      getModelService().save(consignment);
    }
  }

  private OrderProcessModel getOrderProcess(final OrderModel order) {

    final Optional<OrderProcessModel> orderProcessModel = order.getOrderProcess().stream().filter(
        orderProcess -> orderProcess.getProcessDefinitionName().equalsIgnoreCase("order-process"))
        .findFirst();

    if (orderProcessModel.isPresent()) {
      return orderProcessModel.get();
    }

    return null;
  }

  private ConsignmentModel getConsignment(final WarehouseModel location,
      final AbstractOrderModel order) {
    final Optional<ConsignmentModel> consignment = order.getConsignments().stream()
        .filter(consignmentModel ->
            consignmentModel.getWarehouse().getCode().equals(location.getCode())).findFirst();
    if (consignment.isPresent()) {
      return consignment.get();
    }
    return null;
  }

  /**
   * {@inheritDoc}
   */
  public void updateConsignmentEntry(final ConsignmentEntryModel entry,
      final SourcingResult result,
      final AbstractOrderEntryModel orderEntry) {

    final List<BlProductModel> consignmentEntrySerialProducts =
        null != entry.getSerialProducts() ? entry.getSerialProducts() : new ArrayList<>();
    final List<BlProductModel> associatedSerialProducts = new ArrayList<>(consignmentEntrySerialProducts);

    final Set<BlSerialProductModel> serialProductModels =
        null != result.getSerialProductMap() ? result.getSerialProductMap()
            .get(orderEntry.getEntryNumber()) : new HashSet<>();

    if (CollectionUtils.isNotEmpty(serialProductModels)) {

      associatedSerialProducts.addAll(serialProductModels);
      entry.setSerialProducts(
          associatedSerialProducts);   //setting serial products from result
      entry.setQuantity(Long.valueOf(associatedSerialProducts.size()));

      final Set<BlSerialProductModel> serialProducts = new HashSet<>();
      associatedSerialProducts.forEach(serial -> {
        if(serial instanceof BlSerialProductModel) {
          serialProducts.add((BlSerialProductModel) serial);
        }
      });
      getBlConsignmentEntryService().setItemsMap(entry, serialProducts);
      getBlAllocationService().setSerialCodesToBillingCharges(entry, serialProducts);
      getModelService().save(entry);
      reserveStocksForSerialProducts(serialProductModels, entry);
    }
  }

  /**
   * {@inheritDoc}
   */
  public void reserveStocksForSerialProducts(final Set<BlSerialProductModel> serialProductModels,
      final ConsignmentEntryModel entry) {
    final Set<String> allocatedProductCodes = new HashSet<>();
    allocatedProductCodes
        .addAll(serialProductModels.stream().map(BlSerialProductModel::getCode).collect(
            Collectors.toSet()));
    final Collection<StockLevelModel> serialStocks = blStockLevelDao
        .findSerialStockLevelsForDateAndCodes(allocatedProductCodes, entry.getConsignment().getOptimizedShippingStartDate(),
            entry.getConsignment().getOptimizedShippingEndDate(), Boolean.FALSE);
    if (CollectionUtils.isNotEmpty(serialStocks) && serialStocks.stream()
        .allMatch(stock -> allocatedProductCodes.contains(stock.getSerialProductCode()))) {
      serialStocks.forEach(stock -> {
        try {
          BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
                  "Reserve stock for serial product {}, for stock date {} while reallocation before change Hard Assign {}, reserve status {}, associated order {}"
                          + ",current date {} current user {}", stock.getSerialProductCode(), stock.getDate(), stock.getHardAssigned(), stock.getReservedStatus(),
                  stock.getOrder(), new Date(), (defaultBlUserService.getCurrentUser() != null ? defaultBlUserService.getCurrentUser().getUid() : "In Automation"));
        } catch (Exception e) {
          BlLogger.logMessage(LOG, Level.ERROR, "Some error occur while reserve stock in reallocation flow", e);
        }
        stock.setReservedStatus(true);
        stock.setOrder(entry.getOrderEntry().getOrder().getCode());

        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
            "Stock status is changed to {} for the serial product {} ", stock.getReservedStatus(),
            stock.getSerialProductCode());
      });
      this.getModelService().saveAll(serialStocks);
    }
  }
  
  @Override
  public void removeReserveStocksForSerialProducts(Set<String> serialProductCodes, Date startDay, Date endDay, Boolean reservedStatus, WarehouseModel warehouse) {
	     final Collection<StockLevelModel> serialStocks = blStockLevelDao
	        .findSerialStockLevelsForDateAndCodesForWarehouse(serialProductCodes, startDay,
	      		  endDay, reservedStatus, warehouse);

    if (CollectionUtils.isNotEmpty(serialStocks) && serialStocks.stream()
	        .allMatch(stock -> serialProductCodes.contains(stock.getSerialProductCode()))) {
	      serialStocks.forEach(stock -> {
            try {
              BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
                      "Release stock for serial product {}, for stock date {} while reallocate or reassign order before change Hard Assign {} , reserve status {}, associated order {} "
                              + ",current date {} current user {}", stock.getSerialProductCode(), stock.getDate(), stock.getHardAssigned(), stock.getReservedStatus(),
                      stock.getOrder(), new Date(), (defaultBlUserService.getCurrentUser() != null ? defaultBlUserService.getCurrentUser().getUid() : "In Automation"));
            } catch (Exception e) {
              BlLogger.logMessage(LOG, Level.ERROR, "Some error occur while release stock in reallocate or reassign flow", e);
            }
            stock.setReservedStatus(false);
            stock.setOrder(StringUtils.EMPTY);

	        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
	            "Stock status is changed to {} for the serial product {} ", stock.getReservedStatus(),
	            stock.getSerialProductCode());
	      });
	      this.getModelService().saveAll(serialStocks);
	    }

	  }

  @Override
  public boolean reAssignSerialReserveStocksForSerialProducts(Set<String> serialProductCodes, Date startDay, Date endDay, Boolean reservedStatus, WarehouseModel warehouse, String orderCode) {
	    final Collection<StockLevelModel> serialStocks = blStockLevelDao
	        .findSerialStockLevelsForDateAndCodesForWarehouse(serialProductCodes, startDay,
	      		  endDay, reservedStatus, warehouse);
	    boolean isSerialHasStock = false;
	    final LocalDateTime rentalStartDate = BlDateTimeUtils.getFormattedDateTime(startDay);
		 final LocalDateTime rentalEndDate = BlDateTimeUtils.getFormattedDateTime(endDay);
		 final long stayDuration = ChronoUnit.DAYS.between(rentalStartDate, rentalEndDate.plusDays(1));
		 if(serialStocks.size() == stayDuration) {
			 isSerialHasStock =true;
			 if (CollectionUtils.isNotEmpty(serialStocks) && serialStocks.stream()
			        .allMatch(stock -> serialProductCodes.contains(stock.getSerialProductCode()))) {
			      serialStocks.forEach(stock -> {
			        stock.setReservedStatus(true);
			        stock.setOrder(orderCode);
			        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
			            "Stock status is changed to {} for the serial product {} ", stock.getReservedStatus(),
			            stock.getSerialProductCode());
			      });
			      this.getModelService().saveAll(serialStocks);
			    }
			 }
		 else {
			 BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "No stock found for serial product : {} and date between : {} and {}", serialProductCodes, startDay, endDay);
		 }
	return isSerialHasStock;
	}

  /**
   * {@inheritDoc}
   */
  public void startConsignmentSubProcess(final Collection<ConsignmentModel> consignments,
      final OrderProcessModel process) {

    for (final ConsignmentModel consignment : consignments) {
      final ConsignmentProcessModel subProcess = getBusinessProcessService()
          .createProcess(
              consignment.getCode() + WarehousingConstants.CONSIGNMENT_PROCESS_CODE_SUFFIX,
              BlOrdermanagementConstants.CONSIGNMENT_SUBPROCESS_NAME);
      subProcess.setParentProcess(process);
      subProcess.setConsignment(consignment);
      this.getModelService().save(subProcess);
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Starting Consignment sub-process: '{}'",
          subProcess.getCode());
      getBusinessProcessService().startProcess(subProcess);
    }
  }


  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(final ModelService modelService) {
    this.modelService = modelService;
  }

  public BlAssignSerialService getBlAssignSerialService() {
    return blAssignSerialService;
  }

  public void setBlAssignSerialService(final BlAssignSerialService blAssignSerialService) {
    this.blAssignSerialService = blAssignSerialService;
  }
  public BlAllocationService getBlAllocationService() {
    return blAllocationService;
  }

  public void setBlAllocationService(final BlAllocationService blAllocationService) {
    this.blAllocationService = blAllocationService;
  }

  public BlConsignmentEntryService getBlConsignmentEntryService() {
    return blConsignmentEntryService;
  }

  public void setBlConsignmentEntryService(
      final BlConsignmentEntryService blConsignmentEntryService) {
    this.blConsignmentEntryService = blConsignmentEntryService;
  }

  public BlStockLevelDao getBlStockLevelDao() {
    return blStockLevelDao;
  }

  public void setBlStockLevelDao(final BlStockLevelDao blStockLevelDao) {
    this.blStockLevelDao = blStockLevelDao;
  }

  public BusinessProcessService getBusinessProcessService() {
    return businessProcessService;
  }

  public void setBusinessProcessService(
      final BusinessProcessService businessProcessService) {
    this.businessProcessService = businessProcessService;
  }

}
