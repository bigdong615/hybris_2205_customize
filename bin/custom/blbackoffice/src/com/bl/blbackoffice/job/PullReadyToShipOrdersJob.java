package com.bl.blbackoffice.job;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.dao.warehouse.BlConsignmentDao;
import com.bl.core.inventory.order.service.BlReadyToShipOrderItemService;
import com.bl.core.model.PullReadyToShipOrdersCronJobModel;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


/**
 * This cron job will extract the excel for ready to ship consignments
 *
 * @author Sunil
 */

public class PullReadyToShipOrdersJob extends
    AbstractJobPerformable<PullReadyToShipOrdersCronJobModel> {

  private static final Logger LOG = Logger.getLogger(PullReadyToShipOrdersJob.class);

  private BlConsignmentDao blConsignmentDao;
  private BlReadyToShipOrderItemService blReadyToShipOrderItemService;


  /**
   * This cron job runs daily for fetching morning pull orders and populates morning pull view.
   *
   * @param pullReadyToShipOrdersCronJob
   * @return PerformResult
   */
  @Override
  public PerformResult perform(
      final PullReadyToShipOrdersCronJobModel pullReadyToShipOrdersCronJob) {

    BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Start performing PullReadyToShipOrdersJob...");

    final Date currentDate = new Date();
    Date shipDate = null;
    //compare both dates
    if (pullReadyToShipOrdersCronJob.getShipDate().before(currentDate)) {
      shipDate = currentDate;
    } else {
      shipDate = pullReadyToShipOrdersCronJob.getShipDate();
    }

    //remove the existing ReadyToShipOrderItem models for the specified warehouse
    blReadyToShipOrderItemService
        .removeReadyToShipOrderItemsForWarehouse(pullReadyToShipOrdersCronJob.getWarehouse());

    final List<ConsignmentModel> consignmentModels = blConsignmentDao
        .getReadyToShipConsignmentsForDate(shipDate);

    if (CollectionUtils.isNotEmpty(consignmentModels)) {

      final List<ConsignmentModel> filteredConsignmentModels = consignmentModels.stream().filter(
          consignmentModel -> consignmentModel.getWarehouse() == pullReadyToShipOrdersCronJob
              .getWarehouse()).collect(Collectors.toList());

      try {

        if (CollectionUtils.isNotEmpty(filteredConsignmentModels)) {

          blReadyToShipOrderItemService.createReadyToShipOrderItems(filteredConsignmentModels,
              pullReadyToShipOrdersCronJob.getMembersCount());
        } else {
          BlLogger.logFormattedMessage(LOG, Level.INFO,
              "PullReadyToShipOrdersJob :- No consignments found for shipping on {} for warehouse {}",
              shipDate.toString(), pullReadyToShipOrdersCronJob
                  .getWarehouse().getCode());
        }

      } catch (final Exception ex) {

        BlLogger.logFormattedMessage(LOG, Level.ERROR, LogErrorCodeEnum.CRONJOB_ERROR.getCode(), ex.getMessage(),
            "Error occurred while performing PullReadyToShipOrdersJob");
        return resetAndReturnResult(pullReadyToShipOrdersCronJob, CronJobResult.FAILURE);
      }

    } else {

      BlLogger.logFormattedMessage(LOG, Level.INFO,
          "PullReadyToShipOrdersJob :- No consignments found for shipping on {} for warehouse {}",
          shipDate.toString(), pullReadyToShipOrdersCronJob.getWarehouse().getCode());
    }

    BlLogger
        .logFormatMessageInfo(LOG, Level.INFO, "Successfully executed PullReadyToShipOrdersJob...");
    return resetAndReturnResult(pullReadyToShipOrdersCronJob, CronJobResult.SUCCESS);
  }

  /**
   * This method reset the fields to default values and saves the cronjob.
   *
   * @param pullReadyToShipOrdersCronJob
   * @param result
   * @return PerformResult
   */
  private PerformResult resetAndReturnResult(
      final PullReadyToShipOrdersCronJobModel pullReadyToShipOrdersCronJob,
      final CronJobResult result) {

    pullReadyToShipOrdersCronJob.setShipDate(new Date());
    pullReadyToShipOrdersCronJob.setMembersCount(BlCoreConstants.DEFAULT_MEMBERS_COUNT);  // 1
    this.modelService.save(pullReadyToShipOrdersCronJob);
    return new PerformResult(result, CronJobStatus.FINISHED);
  }

  /**
   * @return the blConsignmentDao
   */
  public BlConsignmentDao getBlConsignmentDao() {
    return blConsignmentDao;
  }

  /**
   * @param blConsignmentDao the blConsignmentDao to set
   */
  public void setBlConsignmentDao(final BlConsignmentDao blConsignmentDao) {
    this.blConsignmentDao = blConsignmentDao;
  }

  public BlReadyToShipOrderItemService getBlReadyToShipOrderItemService() {
    return blReadyToShipOrderItemService;
  }

  public void setBlReadyToShipOrderItemService(
      final BlReadyToShipOrderItemService blReadyToShipOrderItemService) {
    this.blReadyToShipOrderItemService = blReadyToShipOrderItemService;
  }

  }

