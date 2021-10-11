package com.bl.blbackoffice.job;

import com.bl.core.model.UPSScrapeCronJobModel;
import com.bl.core.services.upsscrape.impl.BlUpdateSerialService;
import com.bl.integration.services.impl.DefaultBlTrackWebServiceImpl;
import com.bl.integration.services.impl.DefaultBlUPSScrapeService;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import java.util.Date;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This cronjob created to execute automatically to call UPS Scrape service
 * @author Manikandan
 */
public class BlAutomatedUPSScrapeJob extends AbstractJobPerformable<UPSScrapeCronJobModel> {

  private static final Logger LOG = Logger.getLogger(BlAutomatedUPSScrapeJob.class);
  private BlUpdateSerialService blUpdateSerialService;
  private DefaultBlTrackWebServiceImpl defaultBlTrackWebServiceimpl;

  /**
   * This method perform cronjob
   */
  @Override
  public PerformResult perform(UPSScrapeCronJobModel upsScrapeCronJobModel) {
    BlLogger.logMessage(LOG , Level.INFO , "Executing BlUPSScrapeJob perform method");
    try {
//
      OrderModel orderModel = null;
      getDefaultBlTrackWebServiceimpl().trackService(orderModel);

      getBlUpdateSerialService().updateSerialProducts(null , "00016003" , new Date() , 2);
    }
    catch (final Exception e){
      BlLogger.logMessage(LOG , Level.ERROR , "Error while executing BlUPSScrapeJob"  , e.getMessage());
      return new PerformResult(CronJobResult.FAILURE , CronJobStatus.FINISHED);
    }
    return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
  }


  public BlUpdateSerialService getBlUpdateSerialService() {
    return blUpdateSerialService;
  }

  public void setBlUpdateSerialService(
      BlUpdateSerialService blUpdateSerialService) {
    this.blUpdateSerialService = blUpdateSerialService;
  }

  public DefaultBlTrackWebServiceImpl getDefaultBlTrackWebServiceimpl() {
    return defaultBlTrackWebServiceimpl;
  }

  public void setDefaultBlTrackWebServiceimpl(
      DefaultBlTrackWebServiceImpl defaultBlTrackWebServiceimpl) {
    this.defaultBlTrackWebServiceimpl = defaultBlTrackWebServiceimpl;
  }



}
