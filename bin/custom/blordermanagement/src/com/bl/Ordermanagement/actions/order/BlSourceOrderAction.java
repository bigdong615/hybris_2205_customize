package com.bl.Ordermanagement.actions.order;

import com.bl.Ordermanagement.constants.BlOrdermanagementConstants;
import com.bl.Ordermanagement.services.BlSourcingService;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.orderprocessing.model.OrderProcessModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.ConsignmentProcessModel;
import de.hybris.platform.processengine.BusinessProcessService;
import de.hybris.platform.processengine.action.AbstractProceduralAction;
import de.hybris.platform.task.RetryLaterException;
import de.hybris.platform.warehousing.allocation.AllocationService;
import de.hybris.platform.warehousing.constants.WarehousingConstants;
import de.hybris.platform.warehousing.data.sourcing.SourcingResults;
import java.util.Collection;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


public class BlSourceOrderAction extends AbstractProceduralAction<OrderProcessModel> {
  private static final Logger LOG = Logger
      .getLogger(BlSourceOrderAction.class);
  private BusinessProcessService businessProcessService;
  private BlSourcingService blSourcingService;
  private AllocationService allocationService;

  @Override
  public void executeAction(OrderProcessModel process)
      throws RetryLaterException, Exception {
    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Process: {} in step {}",
        process.getCode(), getClass().getSimpleName());
    final OrderModel order = process.getOrder();
    boolean failedFulfillment = false;
    SourcingResults results = null;

    try {
      results = blSourcingService.sourceOrder(order);
    } catch (final IllegalArgumentException e) //NOSONAR
    {
      BlLogger.logMessage(LOG, Level.ERROR, LogErrorCodeEnum.ORDER_SOURCING_ERROR.getCode(),
          "Could not create SourcingResults. Changing order status to SUSPENDED",
          e);
    }

    if (null != results && CollectionUtils.isNotEmpty(results.getResults())) {
      final Collection<ConsignmentModel> consignments = getAllocationService()
          .createConsignments(process.getOrder(), "cons" + process.getOrder().getCode(), results);
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
          "Number of consignments created during allocation: {}",
          consignments.size());
      startConsignmentSubProcess(consignments, process);
      order.setStatus(OrderStatus.READY);

      failedFulfillment = order.getEntries().stream()
          .allMatch(
              orderEntry -> ((OrderEntryModel) orderEntry).getQuantityAllocated().longValue() == 0);
    } else {
      failedFulfillment = true;
    }

    if (failedFulfillment) {
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Order failed to be sourced");
      order.setStatus(OrderStatus.SUSPENDED);
    } else {
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Order was successfully sourced");
    }
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

  public void setBusinessProcessService(
      BusinessProcessService businessProcessService) {
    this.businessProcessService = businessProcessService;
  }

  public BlSourcingService getBlSourcingService() {
    return blSourcingService;
  }

  public void setBlSourcingService(BlSourcingService blSourcingService) {
    this.blSourcingService = blSourcingService;
  }

  public AllocationService getAllocationService() {
    return allocationService;
  }

  public void setAllocationService(
      AllocationService allocationService) {
    this.allocationService = allocationService;
  }
}
