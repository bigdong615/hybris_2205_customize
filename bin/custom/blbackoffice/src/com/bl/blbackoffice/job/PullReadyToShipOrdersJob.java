package com.bl.blbackoffice.job;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.dao.warehouse.BlConsignmentDao;
import com.bl.core.dao.warehouse.BlReadyToShipOrderItemDao;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.PullReadyToShipOrdersCronJobModel;
import com.bl.core.model.ReadyToShipOrderItemModel;
import com.bl.logging.BlLogger;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import java.util.ArrayList;
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

public class PullReadyToShipOrdersJob extends AbstractJobPerformable<PullReadyToShipOrdersCronJobModel> {

  private static final Logger LOG = Logger.getLogger(PullReadyToShipOrdersJob.class);

  private BlConsignmentDao blConsignmentDao;
  private BlReadyToShipOrderItemDao blReadyToShipOrderItemDao;

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

    //remove the existing ReadyToShipOrderItem models for the specified shipping date
    blReadyToShipOrderItemDao
        .removeReadyToShipOrderItemsForDate(shipDate, pullReadyToShipOrdersCronJob.getWarehouse());

    final List<ConsignmentModel> consignmentModels = blConsignmentDao
        .getReadyToShipConsignmentsForDate(shipDate);

    if (CollectionUtils.isNotEmpty(consignmentModels)) {

     final List<ConsignmentModel> filteredConsignmentModels = consignmentModels.stream().filter(
          consignmentModel -> consignmentModel.getWarehouse() == pullReadyToShipOrdersCronJob
              .getWarehouse()).collect(Collectors.toList());

      // create fresh order items and store in DB
      createReadyToShipOrderItems(filteredConsignmentModels,
          pullReadyToShipOrdersCronJob.getMembersCount());
      }

    BlLogger
        .logFormatMessageInfo(LOG, Level.INFO, "Successfully executed PullReadyToShipOrdersJob...");
    return resetAndReturnResult(pullReadyToShipOrdersCronJob, CronJobResult.SUCCESS);
  }

  /**
   * It creates and save ReadyToShipOrderItem to DB
   * @param consignmentModels
   * @param membersCount
   */
  private void createReadyToShipOrderItems(final List<ConsignmentModel> consignmentModels,
      final Integer membersCount) {

    final List<String> membersList = getMembersNameList(membersCount);
    int counter = 0;

    final List<ReadyToShipOrderItemModel> orderItems = new ArrayList<>();
    for (ConsignmentModel consignmentModel : consignmentModels) {

      if (membersList.size() <= counter) {
        counter = 0;
      }

      final String memberName = membersList.get(counter);
      counter++;

      for (ConsignmentEntryModel entry : consignmentModel.getConsignmentEntries()) {

        final List<BlProductModel> blProductModels = entry.getSerialProducts().stream()
            .collect(Collectors.toList());

        final List<BlSerialProductModel> serialProductModels = new ArrayList<>();
        blProductModels.stream().forEach(serial -> {
          if (serial instanceof BlSerialProductModel) {
            serialProductModels.add((BlSerialProductModel) serial);
          }
        });

        for (BlSerialProductModel serialProduct : serialProductModels) {

          orderItems
              .add(createReadyToShipOrderItem(consignmentModel, memberName, entry, serialProduct));

        }
      }
    }

    modelService.saveAll(orderItems);
    }

  /**
   * It creates and returns ReadyToShipOrderItem
   * @param consignmentModel
   * @param memberName
   * @param entry
   * @param serialProduct
   * @return ReadyToShipOrderItemModel
   */
  private ReadyToShipOrderItemModel createReadyToShipOrderItem(
      final ConsignmentModel consignmentModel,
      final String memberName, final ConsignmentEntryModel entry,
      final BlSerialProductModel serialProduct) {

    final ReadyToShipOrderItemModel orderItem = modelService
        .create(ReadyToShipOrderItemModel.class);

    orderItem.setShipDate(consignmentModel.getOrder().getActualRentalStartDate());
    orderItem.setEmployeeName(memberName);  // memberName
    orderItem.setWarehouse(consignmentModel.getWarehouse());
    orderItem.setOrderNo(consignmentModel.getOrder().getCode());
    orderItem.setOrderType(
        null != consignmentModel.getOrderType() ? consignmentModel.getOrderType()
            .getCode() : BlCoreConstants.EMPTY_STRING);

    if (null != consignmentModel.getOrder().getVipFlag()) {
      orderItem.setVipFlag(
          Boolean.TRUE.equals(consignmentModel.getOrder().getVipFlag()) ? BlCoreConstants.TRUE
              : BlCoreConstants.FALSE);
    }

    if (null != consignmentModel.getOrder().getIsRentalCart()) {
      orderItem.setUsedGearFlag(
          Boolean.TRUE.equals(consignmentModel.getOrder().getIsRentalCart()) ? BlCoreConstants.TRUE
              : BlCoreConstants.FALSE);
    }

    orderItem.setOrderNotes(
        null != consignmentModel.getOrder().getConsolidatedOrderNote() ? consignmentModel.getOrder()
            .getConsolidatedOrderNote() : BlCoreConstants.EMPTY_STRING);
    orderItem.setProductId(entry.getOrderEntry().getProduct().getCode());
    orderItem.setProductName(entry.getOrderEntry().getProduct().getName());
    orderItem.setProductCount(BlCoreConstants.DEFAULT_PRODUCT_QUANTITY);
    orderItem.setSerialNumber(serialProduct.getCode());

    orderItem.setChildLocation(
        null != serialProduct.getOcLocation() ? (serialProduct).getOcLocation()
            : BlCoreConstants.EMPTY_STRING);
    orderItem.setParentLocation(
        null != (serialProduct).getLastLocationScanParent() ? (serialProduct)
            .getLastLocationScanParent() : BlCoreConstants.EMPTY_STRING);

    orderItem.setShippingMethod(consignmentModel.getDeliveryMode().getName());

    return orderItem;
  }

  private PerformResult resetAndReturnResult(
      final PullReadyToShipOrdersCronJobModel pullReadyToShipOrdersCronJob,
      final CronJobResult result) {

    pullReadyToShipOrdersCronJob.setShipDate(new Date());
    pullReadyToShipOrdersCronJob.setMembersCount(BlCoreConstants.DEFAULT_MEMBERS_COUNT);  // 1
    this.modelService.save(pullReadyToShipOrdersCronJob);
    return new PerformResult(result, CronJobStatus.FINISHED);
  }

  private List<String> getMembersNameList(final Integer membersCount) {

    final List<String> membersList = new ArrayList<>();
    for (int i = 1; i <= membersCount; i++) {
      membersList.add(BlCoreConstants.EMPLOYEE + i);
    }
    return membersList;
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

  public BlReadyToShipOrderItemDao getBlReadyToShipOrderItemDao() {
    return blReadyToShipOrderItemDao;
  }

  public void setBlReadyToShipOrderItemDao(
      final BlReadyToShipOrderItemDao blReadyToShipOrderItemDao) {
    this.blReadyToShipOrderItemDao = blReadyToShipOrderItemDao;
  }

  }

