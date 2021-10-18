package com.bl.integration.cronjob;

import com.bl.core.model.UPSScrapeCronJobModel;
import com.bl.integration.services.impl.DefaultUPSScrapeService;
import com.bl.logging.BlLogger;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This cronjob created to execute automatically to call UPS Scrape service
 * @author Manikandan
 */
public class BlAutomatedUPSScrapeJob extends AbstractJobPerformable<UPSScrapeCronJobModel> {

  private static final Logger LOG = Logger.getLogger(BlAutomatedUPSScrapeJob.class);

  private DefaultUPSScrapeService defaultUPSScrapeService;

  /**
   * This method perform cronjob
   */
  @Override
  public PerformResult perform(final UPSScrapeCronJobModel upsScrapeCronJobModel) {
    BlLogger.logMessage(LOG , Level.INFO , "Executing BlUPSScrapeJob perform method");
    try {
      getDefaultUPSScrapeService().performUPSScrapeForOrders();
      getDefaultUPSScrapeService().performUPSScrapeForLateOrder();
    }
    catch (final Exception e){
      BlLogger.logMessage(LOG , Level.ERROR , "Error while executing BlUPSScrapeJob"  , e.getMessage());
      return new PerformResult(CronJobResult.FAILURE , CronJobStatus.FINISHED);
    }
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
