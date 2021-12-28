package com.bl.core.job;

import com.bl.core.esp.service.impl.DefaultBlOrderFeedFTPService;
import com.bl.core.model.BlOrderFeedCronJobModel;
import com.bl.core.order.dao.BlOrderDao;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import de.hybris.platform.servicelayer.model.ModelService;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This class created to feed the Newly Created order or Modified order to ESP Feed
 * @author Manikandan
 */
public class BlOrderFeedJob extends AbstractJobPerformable<BlOrderFeedCronJobModel> {

  private static final Logger LOG = Logger.getLogger(BlOrderFeedJob.class);

  private BlOrderDao orderDao;
  private DefaultBlOrderFeedFTPService defaultBlOrderFeedFTPService;
  private ModelService modelService;

  /**
   * This method created to perform the ESP order feed
   * @param blOrderFeedCronJobModel cronjob instance
   * @return result
   */
  @Override
  public PerformResult perform(final BlOrderFeedCronJobModel blOrderFeedCronJobModel) {
    try { List<AbstractOrderModel> orderModelList = null;
      if(Objects.isNull(blOrderFeedCronJobModel.getOrderFeedDate())) {
        orderModelList = getOrderDao().getOrdersForOrderFeedToFTP();
      }
      else {
           orderModelList = getOrderDao().getOrdersForOrderFeedToFTPBasedOnSpecificDate(blOrderFeedCronJobModel.getOrderFeedDate());
      }
      if(CollectionUtils.isNotEmpty((orderModelList))) {
        final List<AbstractOrderModel> unExportedOrderList = new ArrayList<>();
        getDefaultBlOrderFeedFTPService().convertOrderIntoXML(orderModelList , unExportedOrderList);
        if(CollectionUtils.isNotEmpty(unExportedOrderList)) {
          resetOrderFeedDate(blOrderFeedCronJobModel);
          return new PerformResult(CronJobResult.FAILURE , CronJobStatus.FINISHED);
        }
      }
    }
    catch (final Exception e) {
      resetOrderFeedDate(blOrderFeedCronJobModel);
      BlLogger.logMessage(LOG , Level.ERROR , "Error while exporting order feed to FTP location" , e);
      return new PerformResult(CronJobResult.FAILURE , CronJobStatus.FINISHED);

    }
    resetOrderFeedDate(blOrderFeedCronJobModel);
    return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
  }


  /**
   * This method created to reset the OrderFeedDate as null after performing cron job
   * @param blOrderFeedCronJobModel cronjob
   */
  private void resetOrderFeedDate(final BlOrderFeedCronJobModel blOrderFeedCronJobModel) {
    blOrderFeedCronJobModel.setOrderFeedDate(null);
    getModelService().save(blOrderFeedCronJobModel);
    getModelService().refresh(blOrderFeedCronJobModel);
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

  public ModelService getModelService() {
    return modelService;
  }

  @Override
  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }

}
