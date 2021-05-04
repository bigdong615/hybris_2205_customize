package com.bl.facades.populators;

import com.bl.core.model.BlPickUpZoneDeliveryModeModel;
import com.bl.facades.shipping.data.BlPickUpZoneDeliveryModeData;
import de.hybris.platform.commercefacades.order.data.ZoneDeliveryModeData;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;

public class BlPickUpZoneDeliveryModePopulator extends BlZoneDeliveryModePopulator {

    private Converter<AddressModel, AddressData> addressConverter;

    @Override
    public void populate(final ZoneDeliveryModeModel source, final ZoneDeliveryModeData target)
    {
        super.populate(source, target);
        if(null != target && source instanceof BlPickUpZoneDeliveryModeModel) {
            final BlPickUpZoneDeliveryModeModel blPickUpZoneDeliveryModeModel = (BlPickUpZoneDeliveryModeModel) source;
            final BlPickUpZoneDeliveryModeData blPickUpZoneDeliveryModeData = (BlPickUpZoneDeliveryModeData) target;

            blPickUpZoneDeliveryModeData.setUserName(blPickUpZoneDeliveryModeModel.getUserName());
            blPickUpZoneDeliveryModeData.setPassword(blPickUpZoneDeliveryModeModel.getPassword());
            blPickUpZoneDeliveryModeData.setExternalStores(blPickUpZoneDeliveryModeModel.getExternalStores());
            blPickUpZoneDeliveryModeData.setExternalStoreUrl(blPickUpZoneDeliveryModeModel.getExternalStoreUrl());
            blPickUpZoneDeliveryModeData.setInternalStoreAddress(getAddressConverter().convert(
                    blPickUpZoneDeliveryModeModel.getInternalStoreAddress()));
        }
    }

    public Converter<AddressModel, AddressData> getAddressConverter() {
        return addressConverter;
    }

    public void setAddressConverter(Converter<AddressModel, AddressData> addressConverter) {
        this.addressConverter = addressConverter;
    }
}
