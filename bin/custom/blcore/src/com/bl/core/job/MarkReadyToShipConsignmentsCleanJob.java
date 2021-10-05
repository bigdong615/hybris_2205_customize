package com.bl.core.job;

import com.bl.core.dao.warehouse.BlConsignmentDao;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.MarkReadyToShipConsignmentsCleanJobModel;
import com.bl.core.utils.BlInventoryScanUtility;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


/**
 * This cron job will fetch consignments and mark clean complete consignment flag.
 *
 * @author Sunil
 */

public class MarkReadyToShipConsignmentsCleanJob extends AbstractJobPerformable<MarkReadyToShipConsignmentsCleanJobModel> {

  private static final Logger LOG = Logger.getLogger(MarkReadyToShipConsignmentsCleanJob.class);

  private BlConsignmentDao blConsignmentDao;
  private ConfigurationService configurationService;

   /**
   * It extracts the ready to ship consignments and mark the flag cleanCompleteConsignment.
   *
   * @param markReadyToShipConsignmentsCleanJob - job
   * @return PerformResult  - result
   */
  @Override
  public PerformResult perform(
      final MarkReadyToShipConsignmentsCleanJobModel markReadyToShipConsignmentsCleanJob) {

    final Date shipDate = (null == markReadyToShipConsignmentsCleanJob.getShipDate() ? new Date()
        : markReadyToShipConsignmentsCleanJob.getShipDate());

    final List<ConsignmentModel> consignmentModels = blConsignmentDao
        .getReadyToShipConsignmentsForDate(shipDate);

    if (CollectionUtils.isEmpty(consignmentModels)) {

      BlLogger.logFormattedMessage(LOG, Level.DEBUG, LogErrorCodeEnum.CRONJOB_ERROR.getCode(),
          "No matching consignments found while performing MarkReadyToShipConsignmentsCleanJob");
    } else {

      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
          "Start performing MarkReadyToShipConsignmentsCleanJob...for shipping date : {} and consignment codes : {}",
          shipDate, consignmentModels.stream().map(ConsignmentModel::getCode).collect(
              Collectors.toList()));

      try {

        updateCleanCompleteFlagInConsignments(consignmentModels);
      } catch (final Exception ex) {

        BlLogger.logFormattedMessage(LOG, Level.ERROR, LogErrorCodeEnum.CRONJOB_ERROR.getCode(), ex,
            "Error occurred while performing MarkReadyToShipConsignmentsCleanJob");

        return resetAndReturnResult(markReadyToShipConsignmentsCleanJob, CronJobResult.FAILURE);
      }

    }

    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
        "Completed performing MarkReadyToShipConsignmentsCleanJob...");

    return resetAndReturnResult(markReadyToShipConsignmentsCleanJob, CronJobResult.SUCCESS);
  }

  /**
   * This method reset the fields to default values and saves the cronjob.
   *
   * @param markReadyToShipConsignmentsCleanJob
   * @param result
   * @return PerformResult
   */
  private PerformResult resetAndReturnResult(
      final MarkReadyToShipConsignmentsCleanJobModel markReadyToShipConsignmentsCleanJob,
      final CronJobResult result) {

    markReadyToShipConsignmentsCleanJob.setShipDate(null);
    this.modelService.save(markReadyToShipConsignmentsCleanJob);
    return new PerformResult(result, CronJobStatus.FINISHED);
  }

  /**
   * It extracts the ready to ship consignments and mark the flag cleanCompleteConsignment.
   *
   * @param consignmentModels  - consignmentModels
   * @return PerformResult  - result
   */
  private void updateCleanCompleteFlagInConsignments(
      final List<ConsignmentModel> consignmentModels) {

    consignmentModels.forEach(consignment -> {

      final List<AtomicBoolean> allCleanConsignmentEntryList = new ArrayList<>();

      for (ConsignmentEntryModel entryModel : consignment.getConsignmentEntries()) {
        final List<AtomicBoolean> cleanSerialLocationList = new ArrayList<>();

        for (BlProductModel productModel : entryModel.getSerialProducts()) {
          if (productModel instanceof BlSerialProductModel){

            cleanSerialLocationList.add(new AtomicBoolean(isSerialLocationClean(productModel)));
          }
        }

        final boolean isAllEntriesClean =  cleanSerialLocationList.stream().allMatch(AtomicBoolean::get);

        allCleanConsignmentEntryList.add(new AtomicBoolean(isAllEntriesClean));
      }
      consignment.setCleanCompleteConsignment(
          allCleanConsignmentEntryList.stream().allMatch(AtomicBoolean::get));

      if (allCleanConsignmentEntryList.stream().allMatch(AtomicBoolean::get)) {

        BlLogger
            .logFormatMessageInfo(LOG, Level.DEBUG,
                "Consignment with code {} is marked as clean complete.", consignment.getCode());
      }

    });

    modelService.saveAll(consignmentModels);
  }

  /**
   * It returns true if the serial product is in clean location category.
   *
   * @param productModel
   * @return true
   */
  private boolean isSerialLocationClean(final BlProductModel productModel) {

    final String locationCode = null != ((BlSerialProductModel) productModel).getOcLocationDetails()
        && null != ((BlSerialProductModel) productModel).getOcLocationDetails()
        .getLocationCategory() ? ((BlSerialProductModel) productModel).getOcLocationDetails()
        .getLocationCategory()
        .getCode() : "";

    final String parentLocationCode = null != ((BlSerialProductModel) productModel).getOcLocationDetails()
        && null != ((BlSerialProductModel) productModel).getOcLocationDetails()
        .getParentInventoryLocation()
        && null != ((BlSerialProductModel) productModel).getOcLocationDetails()
        .getParentInventoryLocation().getLocationCategory()
        ? ((BlSerialProductModel) productModel).getOcLocationDetails().getParentInventoryLocation()
        .getLocationCategory().getCode()
        : "";

    if (StringUtils.isNotBlank(parentLocationCode)) {
      ((BlSerialProductModel) productModel).setLastLocationScanParent(parentLocationCode);
      modelService.save(productModel);
    }

    return getCleanLocationCategoryList()
        .contains(StringUtils.isNotBlank(parentLocationCode) ? parentLocationCode : locationCode);
  }

  /**
   * It fetches the clean cart locations
   *
   * @return clean cart locations
   */
  private List<String> getCleanLocationCategoryList() {

    final List<String> cleanCartLocationCategoryList = new ArrayList<>();
    cleanCartLocationCategoryList.addAll(BlInventoryScanUtility.getTechEngCleanCartLocations());
    cleanCartLocationCategoryList.addAll(BlInventoryScanUtility.getTechEngCleanPriorityCartLocations());

    return cleanCartLocationCategoryList;
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

  /**
   * @return the configurationService
   */
  public ConfigurationService getConfigurationService() {
    return configurationService;
  }

  /**
   * @param configurationService the configurationService to set
   */
  public void setConfigurationService(
      final ConfigurationService configurationService) {
    this.configurationService = configurationService;
  }

}
