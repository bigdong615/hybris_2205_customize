package com.bl.core.job;

import com.bl.core.order.dao.BlOrderDao;
import com.bl.core.services.upsscrape.impl.DefaultUPSScrapeService;
import com.bl.logging.BlLogger;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This cronjob created to execute automatically to call UPS Scrape service
 * @author Manikandan
 */
public class BlAutomatedUPSScrapeJob extends AbstractJobPerformable<CronJobModel> {

  private static final Logger LOG = Logger.getLogger(BlAutomatedUPSScrapeJob.class);
  private DefaultUPSScrapeService defaultUPSScrapeService;

  /**
   * This method perform cronjob
   */
  @Override
  public PerformResult perform(final CronJobModel cronJobModel) {
    BlLogger.logMessage(LOG , Level.INFO , "Started Executing BlUPSScrapeJob perform method");
    try {
        getDefaultUPSScrapeService().performUPSScrapeForOrders();
        getDefaultUPSScrapeService().performUPSScrapeForLateOrder();
        getDefaultUPSScrapeService().performUPSScrapeForDelayedOrUpdatedOrder();
    }
    catch (final Exception e){
      BlLogger.logFormattedMessage(LOG , Level.ERROR , "Error while executing BlUPSScrapeJob {} " , e.getMessage());
      BlLogger.logMessage(LOG , Level.ERROR , "Error while executing BlUPSScrapeJob " , e);
      return new PerformResult(CronJobResult.FAILURE , CronJobStatus.FINISHED);
    }
    BlLogger.logMessage(LOG , Level.INFO , "Finished Executing BlUPSScrapeJob perform method");
    return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
  }

  public DefaultUPSScrapeService getDefaultUPSScrapeService() {
    return defaultUPSScrapeService;
  }

  public void setDefaultUPSScrapeService(
      DefaultUPSScrapeService defaultUPSScrapeService) {
    this.defaultUPSScrapeService = defaultUPSScrapeService;
  }

}
