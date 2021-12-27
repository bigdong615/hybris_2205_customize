package com.bl.core.job;

import com.bl.core.esp.service.impl.DefaultBlESPFTPService;
import com.bl.core.order.dao.BlOrderDao;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import java.util.List;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This class created to feed the Newly Created Bill or Modified Bill to ESP Feed
 * @author Manikandan
 */
public class BlOrderBillFeedJob  extends AbstractJobPerformable<CronJobModel> {

  private static final Logger LOG = Logger.getLogger(BlOrderBillFeedJob.class);

  private BlOrderDao orderDao;
  private DefaultBlESPFTPService defaultBlESPFTPService;

  /**
   * This method created to perform the ESP order feed
   * @param cronJobModel cronjob instance
   * @return result
   */
  @Override
  public PerformResult perform(final CronJobModel cronJobModel) {
    try {
        List<AbstractOrderModel> orderModelList =  getOrderDao().getOrdersForOrderBillFeedToFTP();
      BlLogger.logMessage(LOG , Level.INFO , "****"  , String.valueOf(orderModelList.size()));

      if(CollectionUtils.isNotEmpty((orderModelList))) {
        getDefaultBlESPFTPService().convertOrderBillIntoXML(orderModelList);
      }
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
  public DefaultBlESPFTPService getDefaultBlESPFTPService() {
    return defaultBlESPFTPService;
  }

  public void setDefaultBlESPFTPService(
      DefaultBlESPFTPService defaultBlESPFTPService) {
    this.defaultBlESPFTPService = defaultBlESPFTPService;
  }
}
