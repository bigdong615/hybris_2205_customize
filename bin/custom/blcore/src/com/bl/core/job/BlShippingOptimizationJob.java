package com.bl.core.job;


import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.constants.GeneratedBlCoreConstants;
import com.bl.core.enums.OptimizedShippingMethodEnum;
import com.bl.core.shipping.service.BlDeliveryModeService;
import com.bl.core.shipping.strategy.BlShippingOptimizationStrategy;
import com.bl.logging.BlLogger;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import java.util.Collection;

public class BlShippingOptimizationJob extends AbstractJobPerformable<CronJobModel> {

    private static final Logger LOG = Logger.getLogger(BlShippingOptimizationJob.class);

    private BlDeliveryModeService zoneDeliveryModeService;
    private BlShippingOptimizationStrategy blShippingOptimizationStrategy;

    /**
     * {@inheritDoc}
     */
    @Override
    public PerformResult perform(final CronJobModel cronJobModel) {
        BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Start performing BlShippingOptimizationJob for ThreeDayGround...");
        changeGroundStatus(getZoneDeliveryModeService().getAllGroundedConsignments(OptimizedShippingMethodEnum.THREE_DAY_GROUND.getCode()),
                OptimizedShippingMethodEnum.TWO_DAY_GROUND.getCode());

        BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Start performing BlShippingOptimizationJob for TwoDayGround...");
        changeGroundStatus(getZoneDeliveryModeService().getAllGroundedConsignments(OptimizedShippingMethodEnum.TWO_DAY_GROUND.getCode()),
                OptimizedShippingMethodEnum.ONE_DAY_GROUND.getCode());

        BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Start performing BlShippingOptimizationJob for OvernightGround...");
        changeGroundStatus(getZoneDeliveryModeService().getAllGroundedConsignments(OptimizedShippingMethodEnum.ONE_DAY_GROUND.getCode()),
                null);

        return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
    }

    private void changeGroundStatus(final Collection<ConsignmentModel> groundConsignments, final String newStatus) {
        if(CollectionUtils.isNotEmpty(groundConsignments)) {
            for(ConsignmentModel model : groundConsignments) {
                getBlShippingOptimizationStrategy().getOptimizedShippingMethodForOrder(model, Boolean.TRUE);
            }
        }
    }

    public BlShippingOptimizationStrategy getBlShippingOptimizationStrategy() {
        return blShippingOptimizationStrategy;
    }

    public void setBlShippingOptimizationStrategy(final BlShippingOptimizationStrategy blShippingOptimizationStrategy) {
        this.blShippingOptimizationStrategy = blShippingOptimizationStrategy;
    }

    public BlDeliveryModeService getZoneDeliveryModeService() {
        return zoneDeliveryModeService;
    }

    public void setZoneDeliveryModeService(BlDeliveryModeService zoneDeliveryModeService) {
        this.zoneDeliveryModeService = zoneDeliveryModeService;
    }
}
