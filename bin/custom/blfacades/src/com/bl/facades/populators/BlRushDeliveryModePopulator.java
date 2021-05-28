package com.bl.facades.populators;

import com.bl.core.model.BlRushDeliveryModeModel;
import com.bl.facades.shipping.data.BlRushDeliveryModeData;
import de.hybris.platform.commercefacades.order.data.ZoneDeliveryModeData;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;

public class BlRushDeliveryModePopulator extends BlZoneDeliveryModePopulator {

    @Override
    public void populate(final ZoneDeliveryModeModel source, final ZoneDeliveryModeData target)
    {
        super.populate(source, target);
        if(null != target && source instanceof BlRushDeliveryModeModel ) {
            BlRushDeliveryModeModel blRushDeliveryModeModel = (BlRushDeliveryModeModel) source;
            if(target instanceof BlRushDeliveryModeData) {
                BlRushDeliveryModeData blRushDeliveryModeData = (BlRushDeliveryModeData) target;
                blRushDeliveryModeData.setShippingArea(blRushDeliveryModeModel.getShippingArea());
                if(blRushDeliveryModeModel.getRadius() != null) {
                    blRushDeliveryModeData.setRadius(Double.toString(blRushDeliveryModeModel.getRadius()));
                }
                blRushDeliveryModeData.setDeliveryType(blRushDeliveryModeModel.getDeliveryType().toString());
            }
        }
    }
}
