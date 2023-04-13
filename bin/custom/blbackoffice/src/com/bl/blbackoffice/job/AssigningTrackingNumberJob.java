package com.bl.blbackoffice.job;

import com.bl.core.assigningTrackingNumber.AssigningTrackingNumberService;
import com.bl.core.model.AssigningTrackingNumberCronJobModel;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This job assigns the Tracking Number to Consignment if the order status is shipped.
 *
 * @author Sunil
 */

public class AssigningTrackingNumberJob extends AbstractJobPerformable<AssigningTrackingNumberCronJobModel> {
    private static final Logger LOG = Logger.getLogger(AssigningTrackingNumberJob.class);
    private AssigningTrackingNumberService assigningTrackingNumberService;

    @Override
    public PerformResult perform(AssigningTrackingNumberCronJobModel assigningTrackingNumberCronJobModel) {
        BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Start performing AssigningTrackingNumberJob...");
        try {
            assigningTrackingNumberService.assigningTracking();
            BlLogger.logFormatMessageInfo(LOG, Level.INFO, "AssigningTrackingNumberJob finished successfully");
        } catch (final Exception ex) {
            BlLogger.logFormattedMessage(LOG, Level.ERROR, LogErrorCodeEnum.CRONJOB_ERROR.getCode(), ex,
                    "Error occurred while performing AssigningTrackingNumberJob");
            return new PerformResult(CronJobResult.FAILURE, CronJobStatus.FINISHED);
        }
        return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
    }

    public AssigningTrackingNumberService getAssigningTrackingNumberService() {
        return assigningTrackingNumberService;
    }

    public void setAssigningTrackingNumberService(AssigningTrackingNumberService assigningTrackingNumberService) {
        this.assigningTrackingNumberService = assigningTrackingNumberService;
    }
}


