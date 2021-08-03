package com.bl.core.job;

import com.bl.core.dao.warehouse.BlConsignmentDao;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.MarkReadyToShipConsignmentsCleanJobModel;
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
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.zookeeper.common.StringUtils;


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

    BlLogger.logFormatMessageInfo(LOG, Level.INFO,
        "Start performing MarkReadyToShipConsignmentsCleanJob...");

    final List<ConsignmentModel> consignmentModels = blConsignmentDao
        .getReadyToShipConsignmentsForDate(new Date());

    if (CollectionUtils.isEmpty(consignmentModels)) {

      BlLogger.logFormattedMessage(LOG, Level.DEBUG, LogErrorCodeEnum.CRONJOB_ERROR.getCode(),
          "No matching consignments found while performing MarkReadyToShipConsignmentsCleanJob");
    } else {

      updateCleanCompleteFlagInConsignments(consignmentModels);
    }

    BlLogger.logFormatMessageInfo(LOG, Level.INFO,
        "Completed performing MarkReadyToShipConsignmentsCleanJob...");
    return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
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

      final List<AtomicBoolean> allEntryCleanCompleteFlagList = new ArrayList<>();

      for (ConsignmentEntryModel entryModel : consignment.getConsignmentEntries()) {
        final List<AtomicBoolean> entryLevelFlagList = new ArrayList<>();

        for (BlProductModel productModel : entryModel.getSerialProducts()) {
          if (productModel instanceof BlSerialProductModel){

            entryLevelFlagList.add(new AtomicBoolean(isSerialLocationClean(productModel)));
          }
        }

        final boolean entryLevelFlag =  entryLevelFlagList.stream().allMatch(AtomicBoolean::get);

        allEntryCleanCompleteFlagList.add(new AtomicBoolean(entryLevelFlag));
      }
      consignment.setCleanCompleteConsignment(
          allEntryCleanCompleteFlagList.stream().allMatch(AtomicBoolean::get));
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

    return null != ((BlSerialProductModel) productModel).getOcLocationDetails()
        && null != ((BlSerialProductModel) productModel).getOcLocationDetails()
        .getLocationCategory() && getCleanLocationCategoryList().contains(
        ((BlSerialProductModel) productModel).getOcLocationDetails().getLocationCategory()
            .getCode());
  }

  /**
   * It fetches the clean cart locations from configuration file.
   *
   * @return clean cart locations
   */
  private List<String> getCleanLocationCategoryList() {

    //CLEAN_FRONT_DESK_CART,CLEAN_GEAR_AISLE_IN_CAGE,CLEAN_GEAR_CAGE,CLEAN_GEAR_MOBILE_CART,
    // CLEAN_GEAR_REQUEST_PICKUP_MOBILE_CART,CLEAN_GEAR_SHIPPING_MOBILE_CART,CLEAN_MOBILE_LAUNDRY_BIN
    final String cleanCartLocations = getConfigurationService().getConfiguration()
        .getString("mark.ready.to.ship.consignments.clean.cart.locations");

    //CLEAN_PRIORITY_GEAR_CART,CLEAN_PRIORITY_MOBILE_CART,VIP_CLEAN_PRIORITY_GEAR
    final String cleanPriorityCartLocations = getConfigurationService().getConfiguration()
        .getString("mark.ready.to.ship.consignments.clean.priority.cart.locations");

    final List<String> cleanCartLocationCategoryList = new ArrayList<>();
    cleanCartLocationCategoryList.addAll(StringUtils.split(cleanCartLocations, ","));
    cleanCartLocationCategoryList.addAll(StringUtils.split(cleanPriorityCartLocations, ","));

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
