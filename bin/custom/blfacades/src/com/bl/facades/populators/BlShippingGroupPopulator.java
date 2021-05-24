package com.bl.facades.populators;

import com.bl.core.model.ShippingGroupModel;
import com.bl.facades.shipping.data.BlShippingGroupData;
import de.hybris.platform.converters.Populator;

public class BlShippingGroupPopulator implements Populator<ShippingGroupModel, BlShippingGroupData> {

    @Override
    public void populate(ShippingGroupModel shippingGroupModel, BlShippingGroupData blShippingGroupData) {
        if(null != shippingGroupModel && null != blShippingGroupData) {
            blShippingGroupData.setCode(shippingGroupModel.getCode());
            blShippingGroupData.setName(shippingGroupModel.getName());
            blShippingGroupData.setShippingType(String.valueOf(shippingGroupModel.getShippingType()));
            blShippingGroupData.setDefaultShippingGroup(shippingGroupModel.isDefaultShippingGroup());
        }
    }
}
