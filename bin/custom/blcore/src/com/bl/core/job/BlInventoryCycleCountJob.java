package com.bl.core.job;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.inventory.cycle.count.service.BlInventoryCycleCountService;
import com.bl.logging.BlLogger;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;

/**
 * This is responsible for creating the auth transactions for the orders
 * @author Namrata Lohar
 */
public class BlInventoryCycleCountJob extends AbstractJobPerformable<CronJobModel> {
    private static final Logger LOG = Logger.getLogger(BlInventoryCycleCountJob.class);

    private BlInventoryCycleCountService blInventoryCycleCountService;

    /**
     * This is for creating InventoryCycle or generating
     */
    @Override
    public PerformResult perform(final CronJobModel cronJobModel) {
        BlLogger.logFormatMessageInfo(LOG, Level.INFO, BlInventoryScanLoggingConstants.START_PERFORMING_BL_AUTHORIZE_PAYMENT_JOB);

        final LocalDate localDate = new Date().toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
        if(localDate.getDayOfWeek() == DayOfWeek.SATURDAY || localDate.getDayOfWeek() == DayOfWeek.SUNDAY) {
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.FAILED_TO_PERFORM_BL_AUTHORIZE_PAYMENT_JOB_AS_IT_S_WEEKEND);
        } else {
            try {
                this.getBlInventoryCycleCountService().createNextInventoryCycleCount();
            } catch(final Exception e) {
                BlLogger.logFormatMessageInfo(LOG, Level.ERROR, BlInventoryScanLoggingConstants.EXCEPTION, e.getMessage());
                return new PerformResult(CronJobResult.FAILURE, CronJobStatus.ABORTED);
            }
        }

        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.FINISHED_PERFORMING_BL_AUTHORIZE_PAYMENT_JOB);
        return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
    }

    public BlInventoryCycleCountService getBlInventoryCycleCountService() {
        return blInventoryCycleCountService;
    }

    public void setBlInventoryCycleCountService(BlInventoryCycleCountService blInventoryCycleCountService) {
        this.blInventoryCycleCountService = blInventoryCycleCountService;
    }
}
