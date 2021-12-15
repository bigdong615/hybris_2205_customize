package com.bl.core.job;

import com.bl.core.order.dao.BlOrderDao;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import java.util.List;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This class created to feed the Newly Created order or Modified order to ESP Feed
 * @author Manikandan
 */
public class BlOrderFeedJob extends AbstractJobPerformable<CronJobModel> {

  private static final Logger LOG = Logger.getLogger(BlOrderFeedJob.class);


  private BlOrderDao orderDao;

  /**
   * This method created to perform the ESP order feed
   * @param cronJobModel cronjob instance
   * @return result
   */
  @Override
  public PerformResult perform(final CronJobModel cronJobModel) {
    try {
     final List<AbstractOrderModel> orderModelList = getOrderDao().getOrdersForOrderFeedToFTP();
      BlLogger.logMessage(LOG , Level.INFO , "****"  , String.valueOf(orderModelList.size()));

    }
    catch (final Exception e) {
      return new PerformResult(CronJobResult.FAILURE , CronJobStatus.FINISHED);

    }
    return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
  }

  public BlOrderDao getOrderDao() {
    return orderDao;
  }

  public void setOrderDao(BlOrderDao orderDao) {
    this.orderDao = orderDao;
  }

}
