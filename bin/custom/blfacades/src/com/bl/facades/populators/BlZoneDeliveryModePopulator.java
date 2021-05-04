package com.bl.facades.populators;

import de.hybris.platform.commercefacades.order.converters.populator.ZoneDeliveryModePopulator;
import de.hybris.platform.commercefacades.order.data.ZoneDeliveryModeData;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;

public class BlZoneDeliveryModePopulator extends ZoneDeliveryModePopulator {

    @Override
    public void populate(final ZoneDeliveryModeModel source, final ZoneDeliveryModeData target)
    {
        super.populate(source, target);
        target.setCutOffTime(source.getCutOffTime());
        target.setCarrier(String.valueOf(source.getCarrier()));
        target.setNumberOfDaysToSkip(String.valueOf(source.getNumberOfDaysToSkip()));
        target.setShippingMethodId(String.valueOf(source.getShippingMethodId()));
        target.setShippingMethodType(String.valueOf(source.getShippingMethodType()));
    }
}
