package com.bl.Ordermanagement.actions.order;

import com.bl.Ordermanagement.constants.BlOrdermanagementConstants;
import com.bl.Ordermanagement.exceptions.BlShippingOptimizationException;
import com.bl.Ordermanagement.exceptions.BlSourcingException;
import com.bl.Ordermanagement.filters.BlDeliveryStateSourcingLocationFilter;
import com.bl.Ordermanagement.services.BlSourcingService;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.VerificationStatusEnum;
import com.bl.core.esp.service.impl.DefaultBlESPEventService;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.services.order.BlOrderService;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.jalo.order.Order;
import de.hybris.platform.ordercancel.impl.orderstatechangingstrategies.RestorePreviousOrderStatusStrategy;
import de.hybris.platform.orderprocessing.model.OrderProcessModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.ConsignmentProcessModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.processengine.BusinessProcessService;
import de.hybris.platform.processengine.action.AbstractProceduralAction;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.servicelayer.exceptions.AmbiguousIdentifierException;
import de.hybris.platform.task.RetryLaterException;
import de.hybris.platform.warehousing.allocation.AllocationService;
import de.hybris.platform.warehousing.constants.WarehousingConstants;
import de.hybris.platform.warehousing.data.sourcing.SourcingResult;
import de.hybris.platform.warehousing.data.sourcing.SourcingResults;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * BlSourceOrderAction for sourcing the order.
 *
 * @author Sunil
 */
public class BlSourceOrderAction extends AbstractProceduralAction<OrderProcessModel> {

  private static final Logger LOG = Logger.getLogger(BlSourceOrderAction.class);
  private BusinessProcessService businessProcessService;
  private BlSourcingService blSourcingService;
  private AllocationService allocationService;
  private BlDeliveryStateSourcingLocationFilter blDeliveryStateSourcingLocationFilter;
  private BlOrderService blOrderService;

  private ConfigurationService configurationService;
  private DefaultBlESPEventService defaultBlESPEventService;

  /**
   * {@inheritDoc}
   */
  @Override
  public void executeAction(final OrderProcessModel process) throws RetryLaterException, Exception {

    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Process: {} in step {}",
        process.getCode(), getClass().getSimpleName());
    final OrderModel order = process.getOrder();

    SourcingResults results = null;

    try {
      results = getSourcingResults(order);

    } catch (final IllegalArgumentException e) {

      setOrderSuspendedStatus(order);
      BlLogger.logFormattedMessage(LOG, Level.ERROR, LogErrorCodeEnum.ORDER_SOURCING_ERROR.getCode(), e,
          "Could not create SourcingResults. Changing order status to SUSPENDED for order code {}", order.getCode());

    } catch (final BlSourcingException ex) {

      setOrderToManualReviewStatus(order);
      BlLogger.logFormattedMessage(LOG, Level.WARN, LogErrorCodeEnum.ORDER_SOURCING_ERROR.getCode(), ex,
          " Changing order status to RECEIVED_MANUAL_REVIEW for order code {}", order.getCode());
    } catch (final Exception e) {

      setOrderSuspendedStatus(order);
      BlLogger.logFormattedMessage(LOG, Level.ERROR, LogErrorCodeEnum.CONSIGNMENT_CREATION_ERROR.getCode(), e,
          " Changing order status to SUSPENDED for order code {}", order.getCode());
    }

    if (null != results && CollectionUtils.isNotEmpty(results.getResults())) {  //NOSONAR
      try {
        final Collection<ConsignmentModel> consignments = getAllocationService()
            .createConsignments(process.getOrder(),
                BlCoreConstants.CONSIGNMENT_PROCESS_PREFIX + process.getOrder().getCode(), results);
        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
            "Number of consignments created during allocation: {}", consignments.size());

        
        // If it went to fruad manual check, we changed logic to assign consignment, serials, so no need to update status again, if its fruad
        if(!order.getStatus().equals(OrderStatus.WAIT_FRAUD_MANUAL_CHECK)) {
        order.setStatus(OrderStatus.PENDING);
        }

        // Gear Value Validations
        validateGearValueConditions(process, order, consignments);

        if (order.getEntries().stream()
            .anyMatch(orderEntry -> orderEntry.getUnAllocatedQuantity().longValue() > 0)) {

          setOrderToManualReviewStatus(order);
        } else {
        getModelService().save(order);
        }
        // To call the order confirmation Order ESP event

        try {
          getDefaultBlESPEventService().sendOrderConfirmation(order);
        }  catch (final Exception ex) {
          BlLogger.logMessage(LOG , Level.ERROR , "Error while executing order confimration ESP Event");
        }

      } catch (final AmbiguousIdentifierException ex) {

        setOrderSuspendedStatus(order);
        BlLogger.logFormattedMessage(LOG, Level.ERROR,
            LogErrorCodeEnum.ORDER_ALLOCATION_ERROR.getCode(), ex,
            "Cancelling consignment since only one fulfillment system configuration is allowed per consignment. Order code {}",
            order.getCode());
      } catch (final BlSourcingException ex) {

        setOrderToManualReviewStatus(order);
        BlLogger.logFormattedMessage(LOG, Level.WARN, LogErrorCodeEnum.ORDER_ALLOCATION_ERROR.getCode(), ex,
            " Changing order status to RECEIVED_MANUAL_REVIEW due to allocation error for order code {}", order.getCode());
      } catch (final BlShippingOptimizationException soe) {

        setOrderSuspendedStatus(order);
        BlLogger.logFormattedMessage(LOG, Level.ERROR, LogErrorCodeEnum.ORDER_OPTIMIZATION_ERROR.getCode(), soe,
                " Changing order status to SUSPENDED due to shipping optimization for order code {}", order.getCode());
      }
      catch (final Exception e) {

        setOrderSuspendedStatus(order);
        BlLogger.logFormattedMessage(LOG, Level.ERROR, LogErrorCodeEnum.CONSIGNMENT_CREATION_ERROR.getCode(), e,
            " Changing order status to SUSPENDED due to error for order code {}", order.getCode());
      }
    }

  }

  /**
   * Changed by - Sunil (BLS-57)
   * Method to implement the Gear Value Conditions
   *
   * @param process      Order Process Model
   * @param order        Order Model
   * @param consignments Consignments
   */
  private void validateGearValueConditions(OrderProcessModel process, OrderModel order,
                                           Collection<ConsignmentModel> consignments) {
    if (order.getStatus().equals(OrderStatus.PENDING)) {
      double threshouldGearValue = getConfigurationService().getConfiguration().getDouble(
              "blordermanagement.non.subpart.gear.value.threshould", 3499);  // Threshould for Max Gear Value
      double threshouldGearValueSecond = getConfigurationService().getConfiguration().getDouble(
              "blordermanagement.non.subpart.gear.value.threshould.second=",
              800);  // Threshould Secondary for Max Gear Value with Completed Orders
      double threshouldGearValueThird = getConfigurationService().getConfiguration().getDouble(
              "blordermanagement.non.subpart.gear.value.threshould.third=",
              12000);   // Threshould tertiary for Max Gear Value with Completed Orders


      // Total Gear Value from Order
      Double sumOfGearValue = Double.valueOf(0);
      if (order.getSumOfGearValueOnOrder() != null) {
        sumOfGearValue = order.getSumOfGearValueOnOrder();
      }
      List<OrderModel> availableOrderForCustomer = new ArrayList<>();
      int completedOrderCount = 0;
      Boolean LateOrderFlag = Boolean.FALSE;
      Boolean ApproveOrderFlag = Boolean.FALSE;
      Boolean RecentOrderFlag = Boolean.TRUE;

      if (order.getUser() != null) {
        CustomerModel customerModel = (CustomerModel) order.getUser();
        if (CollectionUtils.isNotEmpty(customerModel.getOrders())) {
          availableOrderForCustomer = (List<OrderModel>) customerModel.getOrders();

          for (OrderModel orderModel : availableOrderForCustomer) {
            if (orderModel.isIsLatestOrder() && orderModel.getStatus() != null
                    && !(orderModel.getStatus() == OrderStatus.INCOMPLETE)) {
              RecentOrderFlag = Boolean.TRUE;
            }
          }
          for (OrderModel orderModel : availableOrderForCustomer) {
            if (orderModel.getVerificationStatus() != null && ((orderModel.getVerificationStatus().equals(VerificationStatusEnum.NA)|| (orderModel.getVerificationStatus().equals(VerificationStatusEnum.DENY)))))
            {
              ApproveOrderFlag = Boolean.TRUE;
              break;
            }

            else
            {
              ApproveOrderFlag = Boolean.FALSE;
            }
          }
          for (OrderModel orderModel : availableOrderForCustomer) {
            if (orderModel.getStatus() != null && orderModel.getStatus().equals(OrderStatus.LATE)) {
              LateOrderFlag = Boolean.TRUE;
              break;
            }
          }

          for (OrderModel orderModel : availableOrderForCustomer) {
            if (orderModel.getStatus().equals(OrderStatus.COMPLETED)) {
              completedOrderCount = completedOrderCount + 1;
            }
          }
        }
      }
      // Condition #1

		if (sumOfGearValue >= threshouldGearValueThird)
		{
			order.setStatus(OrderStatus.VERIFICATION_REQUIRED);
			startConsignmentSubProcess(consignments, process, true);
		}
		else if

		(((sumOfGearValue > threshouldGearValue) && ApproveOrderFlag && RecentOrderFlag)
			//	|| (sumOfGearValue >= threshouldGearValueSecond && completedOrderCount == 0)
        	|| LateOrderFlag)
		{
			order.setStatus(OrderStatus.RECEIVED_IN_VERIFICATION);
			startConsignmentSubProcess(consignments, process, true);
		}
		else
		{
			startConsignmentSubProcess(consignments, process, false);
		}
    } else {
      startConsignmentSubProcess(consignments, process, false);
    }
  }

  /**
   * Create SourcingResults for different types of orders.
   *
   * @param order - the order.
   * @return SourcingResults      - the results
   */
  private SourcingResults getSourcingResults(final OrderModel order) {

    SourcingResults results = null;
    if (order.getIsRentalOrder().booleanValue()) {

      if (blOrderService.isAquatechProductOrder(order)) {
        results =  getResultsForOrderWithOnlyAquatechProducts(order);
      } else {
        results = blSourcingService.sourceOrder(order, null);// newOrderEntry parameter should always be null for placing order via storefront
      }
    } else{
      results = getResultsForUsedGearOrder(order);
    }

    return results;
  }

  /**
   * Create sourcing result if order is only with aquatech products.
   *
   * @param order - the order.
   * @return SourcingResults
   */
  private SourcingResults getResultsForOrderWithOnlyAquatechProducts(final OrderModel order) {

    final SourcingResults results = new SourcingResults();
    final Set<SourcingResult> resultSet = new HashSet<>();
    final SourcingResult sourcingResult = new SourcingResult();

    for (AbstractOrderEntryModel entry : order.getEntries()) {

      final WarehouseModel warehouseModel = blDeliveryStateSourcingLocationFilter.applyFilter(order);

      final List<BlProductModel> aquatechProductsToAssign = new ArrayList<>();
      for (int i = 0; i < entry.getQuantity(); i++){
        aquatechProductsToAssign.add((BlProductModel) entry.getProduct());
      }

      final Map<Integer, List<BlProductModel>> resultAquatechProductMap =
          (null != sourcingResult.getAquatechProductMap()) ? new HashMap<>(sourcingResult.getAquatechProductMap()) : new HashMap<>();
      resultAquatechProductMap.put(entry.getEntryNumber(), aquatechProductsToAssign);

      final Map<AbstractOrderEntryModel, Long> resultAllocationMap =
          (null != sourcingResult.getAllocation()) ? new HashMap<>(sourcingResult.getAllocation())
              : new HashMap<>();
      resultAllocationMap.put(entry, (long) aquatechProductsToAssign.size());

      sourcingResult.setAquatechProductMap(resultAquatechProductMap);
      sourcingResult.setAllocation(resultAllocationMap);
      sourcingResult.setWarehouse(warehouseModel);
      resultSet.add(sourcingResult);
    }

    results.setResults(resultSet);

    return results;
  }

  /**
   * Create SourcingResults for used gear orders.
   *
   * @param order - the order.
   * @return SourcingResults      - the results
   */
  private SourcingResults getResultsForUsedGearOrder(final OrderModel order) {

    final SourcingResults results = new SourcingResults();
    final Set<SourcingResult> resultSet = new HashSet<>();
    final Map<WarehouseModel, SourcingResult> warehouseSourcingResultMap = new HashMap<>();

    for (AbstractOrderEntryModel entry : order.getEntries()) {

      final WarehouseModel warehouseModel = ((BlSerialProductModel) entry.getProduct())
          .getWarehouseLocation();

      if (null == warehouseSourcingResultMap.get(warehouseModel)) {

        final SourcingResult sourcingResult = new SourcingResult();
        updateResultAndAssignSerials(resultSet, entry, warehouseModel, sourcingResult);
        warehouseSourcingResultMap.put(warehouseModel, sourcingResult);
      } else {

        updateResultAndAssignSerials(resultSet, entry, warehouseModel,
            warehouseSourcingResultMap.get(warehouseModel));
      }
    }

    results.setResults(resultSet);

    final Calendar calendar = Calendar.getInstance();
    calendar.setTime(new Date());
    calendar.add(Calendar.DATE, BlOrdermanagementConstants.TWO);
    order.setActualRentalStartDate(calendar.getTime());
    blSourcingService.updateShippingDatesForInternalTransfers(order, results);

    return results;
  }

  /**
   * Update SourcingResults and assign serials.
   *
   * @param resultSet      - resultSet
   * @param entry          - order entry
   * @param warehouseModel - warehouseModel
   * @param sourcingResult - sourcingResult
   */
  public void updateResultAndAssignSerials(final Set<SourcingResult> resultSet,
      final AbstractOrderEntryModel entry, final WarehouseModel warehouseModel,
      final SourcingResult sourcingResult) {

    final Set<BlSerialProductModel> serialProductsToAssign = new HashSet<>();
    serialProductsToAssign.add((BlSerialProductModel) entry.getProduct());

    final Map<Integer, Set<BlSerialProductModel>> resultSerialProductMap =
        (null != sourcingResult.getSerialProductMap()) ? new HashMap<>(
            sourcingResult.getSerialProductMap()) : new HashMap<>();
    resultSerialProductMap.put(entry.getEntryNumber(), serialProductsToAssign);

    final Map<AbstractOrderEntryModel, Long> resultAllocationMap =
        (null != sourcingResult.getAllocation()) ? new HashMap<>(sourcingResult.getAllocation())
            : new HashMap<>();
    resultAllocationMap.put(entry, (long) serialProductsToAssign.size());

    sourcingResult.setSerialProductMap(resultSerialProductMap);
    sourcingResult.setAllocation(resultAllocationMap);
    sourcingResult.setWarehouse(warehouseModel);
    resultSet.add(sourcingResult);
  }

  /**
   * Set order status to SUSPENDED.
   *
   * @param order - order
   */
  private void setOrderSuspendedStatus(final OrderModel order) {

    order.setStatus(OrderStatus.SUSPENDED);
    getModelService().save(order);
  }

  /**
   * Set order status to RECEIVED_MANUAL_REVIEW.
   *
   * @param order - order
   */
  private void setOrderToManualReviewStatus(final OrderModel order) {

    order.setStatus(OrderStatus.RECEIVED_MANUAL_REVIEW);
    getModelService().save(order);
  }

  /**
   * Create and start a consignment process for each consignment in the collection.
   *
   * @param consignments - list of consignments; never <tt>null</tt>
   * @param process      - order process model
   */
  public void startConsignmentSubProcess(final Collection<ConsignmentModel> consignments,
      final OrderProcessModel process, boolean gearValueFlag) {

    for (final ConsignmentModel consignment : consignments) {
      final ConsignmentProcessModel subProcess = getBusinessProcessService()
          .createProcess(
              consignment.getCode() + WarehousingConstants.CONSIGNMENT_PROCESS_CODE_SUFFIX,
              BlOrdermanagementConstants.CONSIGNMENT_SUBPROCESS_NAME);
      subProcess.setParentProcess(process);

      if(gearValueFlag){
        consignment.setStatus(ConsignmentStatus.WAITING);
        modelService.save(consignment);
        modelService.refresh(consignment);
      }
      subProcess.setConsignment(consignment);

      save(subProcess);
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Starting Consignment sub-process: '{}'",
          subProcess.getCode());
      getBusinessProcessService().startProcess(subProcess);
    }
  }

  public BusinessProcessService getBusinessProcessService() {
    return businessProcessService;
  }

  public void setBusinessProcessService(final BusinessProcessService businessProcessService) {
    this.businessProcessService = businessProcessService;
  }

  public BlSourcingService getBlSourcingService() {
    return blSourcingService;
  }

  public void setBlSourcingService(final BlSourcingService blSourcingService) {
    this.blSourcingService = blSourcingService;
  }

  public AllocationService getAllocationService() {
    return allocationService;
  }

  public void setAllocationService(final AllocationService allocationService) {
    this.allocationService = allocationService;
  }

  public BlDeliveryStateSourcingLocationFilter getBlDeliveryStateSourcingLocationFilter() {
    return blDeliveryStateSourcingLocationFilter;
  }

  public void setBlDeliveryStateSourcingLocationFilter(
      final BlDeliveryStateSourcingLocationFilter blDeliveryStateSourcingLocationFilter) {
    this.blDeliveryStateSourcingLocationFilter = blDeliveryStateSourcingLocationFilter;
  }

  public BlOrderService getBlOrderService() {
    return blOrderService;
  }

  public void setBlOrderService(final BlOrderService blOrderService) {
    this.blOrderService = blOrderService;
  }


  public DefaultBlESPEventService getDefaultBlESPEventService() {
    return defaultBlESPEventService;
  }

  public void setDefaultBlESPEventService(
      DefaultBlESPEventService defaultBlESPEventService) {
    this.defaultBlESPEventService = defaultBlESPEventService;
  }

  /**
   * Getter Method for the Configuration Service
   * @return ConfigurationService
   */
  public ConfigurationService getConfigurationService() {
    return configurationService;
  }

  /**
   * Setter Method for the Configuration Service
   * @param configurationService
   */
  public void setConfigurationService(ConfigurationService configurationService) {
    this.configurationService = configurationService;
  }


}
