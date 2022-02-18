package com.bl.core.job;

import com.bl.core.payment.service.BlPaymentService;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This is for voiding $1 auth transactions for the orders
 * @author Moumita
 */
public class BlVoidAuthTransactionJob extends AbstractJobPerformable<CronJobModel>
{
  private static final Logger LOG = Logger.getLogger(BlVoidAuthTransactionJob.class);
  private BlPaymentService blPaymentService;

  /**
   * This is for voiding $1 auth transactions for the orders
   * @param cronJob the cron jo instance
   * @return PerformResult
   */
  @Override
  public PerformResult perform(final CronJobModel cronJob)
  {
    BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Start performing BlVoidAuthTransactionJob...");
    try
    {
      getBlPaymentService().voidAuthTransaction();
      BlLogger.logFormatMessageInfo(LOG, Level.INFO, "BlVoidAuthTransactionJob finished successfully");
    }
    catch(final Exception ex)
    {
      BlLogger.logMessage(LOG, Level.ERROR, LogErrorCodeEnum.CRONJOB_ERROR.getCode(),
          "Error occurred while performing BlVoidAuthTransactionJob", ex);
      return new PerformResult(CronJobResult.FAILURE, CronJobStatus.FINISHED);
    }
    return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
  }

  public BlPaymentService getBlPaymentService() {
    return blPaymentService;
  }

  public void setBlPaymentService(BlPaymentService blPaymentService) {
    this.blPaymentService = blPaymentService;
  }
}
