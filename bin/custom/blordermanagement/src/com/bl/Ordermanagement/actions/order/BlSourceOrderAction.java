package com.bl.Ordermanagement.actions.order;

import com.bl.Ordermanagement.constants.BlOrdermanagementConstants;
import com.bl.Ordermanagement.exceptions.BlShippingOptimizationException;
import com.bl.Ordermanagement.exceptions.BlSourcingException;
import com.bl.Ordermanagement.services.BlSourcingService;
import com.bl.core.constants.BlCoreConstants;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.orderprocessing.model.OrderProcessModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.ConsignmentProcessModel;
import de.hybris.platform.processengine.BusinessProcessService;
import de.hybris.platform.processengine.action.AbstractProceduralAction;
import de.hybris.platform.servicelayer.exceptions.AmbiguousIdentifierException;
import de.hybris.platform.task.RetryLaterException;
import de.hybris.platform.warehousing.allocation.AllocationService;
import de.hybris.platform.warehousing.constants.WarehousingConstants;
import de.hybris.platform.warehousing.data.sourcing.SourcingResults;
import java.util.Collection;
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

      results = blSourcingService.sourceOrder(order);
      isSourcingSuccessful = true;
    } catch (final IllegalArgumentException e) {

      isSourcingSuccessful = false;
      setOrderSuspendedStatus(order);
      BlLogger.logMessage(LOG, Level.ERROR, LogErrorCodeEnum.ORDER_SOURCING_ERROR.getCode(),
          "Could not create SourcingResults. Changing order status to SUSPENDED", e);
    } catch (final BlSourcingException ex) {

      isSourcingSuccessful = false;
      setOrderSuspendedStatus(order);
      BlLogger.logMessage(LOG, Level.ERROR, LogErrorCodeEnum.ORDER_SOURCING_ERROR.getCode(),
          ex.getMessage() + " Changing order status to SUSPENDED", ex);
    }

    if (null != results && CollectionUtils.isNotEmpty(results.getResults()) && isSourcingSuccessful) {
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
        BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
            LogErrorCodeEnum.ORDER_ALLOCATION_ERROR.getCode(),
            "Cancelling consignment since only one fulfillment system configuration is allowed per consignment.",
            ex);
      } catch (final BlSourcingException ex) {

        setOrderSuspendedStatus(order);
        BlLogger.logMessage(LOG, Level.ERROR, LogErrorCodeEnum.ORDER_ALLOCATION_ERROR.getCode(),
            ex.getMessage() + " Changing order status to SUSPENDED", ex);
      } catch (final BlShippingOptimizationException soe) {

        setOrderSuspendedStatus(order);
        BlLogger.logMessage(LOG, Level.ERROR, LogErrorCodeEnum.ORDER_OPTIMIZATION_ERROR.getCode(), soe.getMessage() +
                " Changing order status to SUSPENDED due to shipping optimization", soe);
      }
    }

  }

  private void setOrderSuspendedStatus(final OrderModel order) {

    order.setStatus(OrderStatus.SUSPENDED);
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
