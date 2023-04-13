package com.bl.core.job;

import com.bl.core.ghostorderremovaljob.GhostOrderRemovalJobService;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.GhostOrderRemovalCronJobModel;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

public class GhostOrderRemovalJob extends AbstractJobPerformable<GhostOrderRemovalCronJobModel> {

    private static final Logger LOG = Logger.getLogger(GhostOrderRemovalJob.class);

    private GhostOrderRemovalJobService ghostOrderRemovalJobService;

    @Override
    public PerformResult perform(GhostOrderRemovalCronJobModel ghostOrderRemovalCronJobModel) {
        BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Start performing GhostOrderRemovalJob...");
        try {
            ghostOrderRemovalJobService.removeGhostOrders();
            BlLogger.logFormatMessageInfo(LOG, Level.INFO, "GhostOrderRemovalJob finished successfully");
        } catch (final Exception ex) {
            BlLogger.logFormattedMessage(LOG, Level.ERROR, LogErrorCodeEnum.CRONJOB_ERROR.getCode(), ex,
                    "Error occurred while performing GhostOrderRemovalJob");
            return new PerformResult(CronJobResult.FAILURE, CronJobStatus.FINISHED);
        }
        return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
    }

    public GhostOrderRemovalJobService getGhostOrderRemovalJobService() {
        return ghostOrderRemovalJobService;
    }

    public void setGhostOrderRemovalJobService(GhostOrderRemovalJobService ghostOrderRemovalJobService) {
        this.ghostOrderRemovalJobService = ghostOrderRemovalJobService;
    }

}
