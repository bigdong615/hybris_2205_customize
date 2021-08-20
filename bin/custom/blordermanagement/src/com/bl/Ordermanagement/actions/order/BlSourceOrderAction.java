package com.bl.Ordermanagement.actions.order;

import com.bl.Ordermanagement.constants.BlOrdermanagementConstants;
import com.bl.Ordermanagement.exceptions.BlShippingOptimizationException;
import com.bl.Ordermanagement.exceptions.BlSourcingException;
import com.bl.Ordermanagement.services.BlSourcingService;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlSerialProductModel;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.orderprocessing.model.OrderProcessModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.ConsignmentProcessModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.processengine.BusinessProcessService;
import de.hybris.platform.processengine.action.AbstractProceduralAction;
import de.hybris.platform.servicelayer.exceptions.AmbiguousIdentifierException;
import de.hybris.platform.task.RetryLaterException;
import de.hybris.platform.warehousing.allocation.AllocationService;
import de.hybris.platform.warehousing.constants.WarehousingConstants;
import de.hybris.platform.warehousing.data.sourcing.SourcingResult;
import de.hybris.platform.warehousing.data.sourcing.SourcingResults;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
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

  /**
   * {@inheritDoc}
   */
  @Override
  public void executeAction(final OrderProcessModel process) throws RetryLaterException, Exception {

    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Process: {} in step {}",
        process.getCode(), getClass().getSimpleName());
    final OrderModel order = process.getOrder();
    boolean isSourcingSuccessful;
    SourcingResults results = null;

    try {
      results = getSourcingResults(order);
      isSourcingSuccessful = true;
    } catch (final IllegalArgumentException e) {

      isSourcingSuccessful = false;
      setOrderSuspendedStatus(order);
      BlLogger.logFormattedMessage(LOG, Level.ERROR, LogErrorCodeEnum.ORDER_SOURCING_ERROR.getCode(), e,
          "Could not create SourcingResults. Changing order status to SUSPENDED for order code {}", order.getCode());

    } catch (final BlSourcingException ex) {

      isSourcingSuccessful = false;
      setOrderToManualReviewStatus(order);
      BlLogger.logFormattedMessage(LOG, Level.WARN, LogErrorCodeEnum.ORDER_SOURCING_ERROR.getCode(), ex,
          " Changing order status to MANUAL_REVIEW for order code {}", order.getCode());
    } catch (final Exception e) {

      isSourcingSuccessful = false;
      setOrderSuspendedStatus(order);
      BlLogger.logFormattedMessage(LOG, Level.ERROR, LogErrorCodeEnum.CONSIGNMENT_CREATION_ERROR.getCode(), e,
          " Changing order status to SUSPENDED for order code {}", order.getCode());
    }

    if (null != results && CollectionUtils.isNotEmpty(results.getResults()) && isSourcingSuccessful) {  //NOSONAR
      try {
        final Collection<ConsignmentModel> consignments = getAllocationService()
            .createConsignments(process.getOrder(),
                BlCoreConstants.CONSIGNMENT_PROCESS_PREFIX + process.getOrder().getCode(), results);
        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
            "Number of consignments created during allocation: {}", consignments.size());
        startConsignmentSubProcess(consignments, process);
        order.setStatus(OrderStatus.READY);
        getModelService().save(order);

      } catch (final AmbiguousIdentifierException ex) {

        setOrderSuspendedStatus(order);
        BlLogger.logFormattedMessage(LOG, Level.ERROR,
            LogErrorCodeEnum.ORDER_ALLOCATION_ERROR.getCode(), ex,
            "Cancelling consignment since only one fulfillment system configuration is allowed per consignment. Order code {}",
            order.getCode());
      } catch (final BlSourcingException ex) {

        setOrderToManualReviewStatus(order);
        BlLogger.logFormattedMessage(LOG, Level.WARN, LogErrorCodeEnum.ORDER_ALLOCATION_ERROR.getCode(), ex,
            " Changing order status to MANUAL_REVIEW due to allocation error for order code {}", order.getCode());
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
   * Create SourcingResults for different types of orders.
   *
   * @param order - the order.
   * @return SourcingResults      - the results
   */
  private SourcingResults getSourcingResults(final OrderModel order) {

    return BooleanUtils.isTrue(order.getIsRentalCart()) ? blSourcingService.sourceOrder(order)
        : getResultsForUsedGearOrder(order);
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
  private void updateResultAndAssignSerials(final Set<SourcingResult> resultSet,
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
   * Set order status to MANUAL_REVIEW.
   *
   * @param order - order
   */
  private void setOrderToManualReviewStatus(final OrderModel order) {

    order.setStatus(OrderStatus.MANUAL_REVIEW);
    getModelService().save(order);
  }

  /**
   * Create and start a consignment process for each consignment in the collection.
   *
   * @param consignments - list of consignments; never <tt>null</tt>
   * @param process      - order process model
   */
  protected void startConsignmentSubProcess(final Collection<ConsignmentModel> consignments,
      final OrderProcessModel process) {

    for (final ConsignmentModel consignment : consignments) {
      final ConsignmentProcessModel subProcess = getBusinessProcessService()
          .createProcess(
              consignment.getCode() + WarehousingConstants.CONSIGNMENT_PROCESS_CODE_SUFFIX,
              BlOrdermanagementConstants.CONSIGNMENT_SUBPROCESS_NAME);
      subProcess.setParentProcess(process);
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
}
