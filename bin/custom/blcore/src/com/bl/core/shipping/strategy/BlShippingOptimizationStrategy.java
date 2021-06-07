package com.bl.core.shipping.strategy;

import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.warehousing.data.sourcing.SourcingContext;
import de.hybris.platform.warehousing.data.sourcing.SourcingLocation;

public interface BlShippingOptimizationStrategy {

    /**
     * This method will check availability for three day ground optimized shipping
     *
     * @param context order
     * @param sourcingLocation consignment
     * @return SourcingLocation
     */
    SourcingLocation getProductAvailabilityForThreeDayGround(final SourcingContext context, final SourcingLocation sourcingLocation);

    /**
     * This method will do cross-check with warehouse cutoff at the time of shipment label generation
     *
     * @param consignmentModel order
     * @return true if success
     */
    boolean generateShipmentLabelForConsignment(final ConsignmentModel consignmentModel);

    /**
     * This method will calculate order for optimized shipping method and optimized shipping date
     *
     * @param consignmentModel order
     *
     * @return true if success
     */
    boolean getOptimizedShippingMethodForOrder(final ConsignmentModel consignmentModel);

    /**
     * This method will check current day in black dates
     *
     * @return true if present
     */
    boolean checkCurrentDayInBlackOutDays();
}
