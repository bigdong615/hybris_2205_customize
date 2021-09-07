package com.bl.core.job;

import com.bl.logging.BlLogger;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This is responsible for creating the auth transactions for the orders
 * @author Namrata Lohar
 */
public class BlInventoryCycleCountJob extends AbstractJobPerformable<CronJobModel> {
    private static final Logger LOG = Logger.getLogger(BlInventoryCycleCountJob.class);

    /**
     * This is for creating InventoryCycle or generating
     */
    @Override
    public PerformResult perform(CronJobModel cronJobModel) {
        BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Start performing BlAuthorizePaymentJob...");

        //return new PerformResult(CronJobResult.FAILURE, CronJobStatus.FINISHED);
        return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
    }
}
