package com.bl.core.job;

import com.bl.core.esp.service.impl.DefaultBlOrderFeedFTPService;
import com.bl.core.model.BlOrderBillFeedCronJobModel;
import com.bl.core.order.dao.BlOrderDao;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This class created to feed the Newly Created Bill or Modified Bill to ESP Feed
 * @author Manikandan
 */
public class BlOrderBillFeedJob  extends AbstractJobPerformable<BlOrderBillFeedCronJobModel> {

  private static final Logger LOG = Logger.getLogger(BlOrderBillFeedJob.class);
  private BlOrderDao orderDao;
  private DefaultBlOrderFeedFTPService defaultBlOrderFeedFTPService;

  /**
   * This method created to perform the ESP order feed
   * @param blOrderBillFeedCronJobModel cronjob instance
   * @return result
   */
  @Override
  public PerformResult perform(final BlOrderBillFeedCronJobModel blOrderBillFeedCronJobModel) {
    try {
        List<AbstractOrderModel> orderModelList = null;
      if(Objects.isNull(blOrderBillFeedCronJobModel.getOrderBillFeedDate())) {
        orderModelList = getOrderDao().getOrdersForOrderBillFeedToFTP();
      }
      else{
        orderModelList = getOrderDao().getOrdersForOrderBillFeedToFTPBasedOnSpecificDate(blOrderBillFeedCronJobModel.getOrderBillFeedDate());
      }
      if(CollectionUtils.isNotEmpty((orderModelList))) {
        final List<AbstractOrderModel> unExportedOrderList = new ArrayList<>();
        getDefaultBlOrderFeedFTPService().convertOrderBillIntoXML(orderModelList,unExportedOrderList);
        if(CollectionUtils.isNotEmpty(unExportedOrderList)) {
          printLoggerForFailedOrders(unExportedOrderList , blOrderBillFeedCronJobModel.getOrderBillFeedDate());
          resetOrderFeedDate(blOrderBillFeedCronJobModel);
          return new PerformResult(CronJobResult.FAILURE , CronJobStatus.FINISHED);
        }
      }
    }
    catch (final Exception e) {
      resetOrderFeedDate(blOrderBillFeedCronJobModel);
      return new PerformResult(CronJobResult.FAILURE , CronJobStatus.FINISHED);

    }
    resetOrderFeedDate(blOrderBillFeedCronJobModel);
    return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
  }

  /**
   * This method created to reset the OrderBillFeedDate as null after performing cron job
   * @param blOrderBillFeedCronJobModel cronjob
   */
  private void resetOrderFeedDate(final BlOrderBillFeedCronJobModel blOrderBillFeedCronJobModel) {
    blOrderBillFeedCronJobModel.setOrderBillFeedDate(null);
    this.modelService.save(blOrderBillFeedCronJobModel);
    this.modelService.refresh(blOrderBillFeedCronJobModel);
  }

  /**
   * This method created to print the orders which is failed for export to FTP
   * @param unExportedOrderList list of unexported orders
   * @param orderFeedDate order feed date
   */
  private void printLoggerForFailedOrders(final List<AbstractOrderModel> unExportedOrderList,
      final Date orderFeedDate) {
    unExportedOrderList.forEach(abstractOrderModel -> BlLogger.logFormattedMessage(LOG , Level.ERROR ,
        "Error While exporting order {} to FTP for Date {} " , abstractOrderModel.getCode() , Objects.isNull(orderFeedDate) ? new Date() : orderFeedDate));
  }


  public BlOrderDao getOrderDao() {
    return orderDao;
  }

  public void setOrderDao(BlOrderDao orderDao) {
    this.orderDao = orderDao;
  }
  public DefaultBlOrderFeedFTPService getDefaultBlOrderFeedFTPService() {
    return defaultBlOrderFeedFTPService;
  }

  public void setDefaultBlOrderFeedFTPService(
      DefaultBlOrderFeedFTPService defaultBlOrderFeedFTPService) {
    this.defaultBlOrderFeedFTPService = defaultBlOrderFeedFTPService;
  }
}
