package com.bl.facades.populators;

import de.hybris.platform.commercefacades.user.converters.populator.AddressPopulator;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.storelocator.model.PointOfServiceModel;

public class BlAddressPopulator extends AddressPopulator {

    /**
     * {@inheritDoc}
     */
    @Override
    public void populate(final AddressModel source, final AddressData target) {
        super.populate(source, target);
        if(source.getAddressType() != null) {
            target.setAddressType(source.getAddressType().toString());
        }
        target.setOpeningDaysDetails(source.getOpeningDaysDetails());
        target.setUpsStoreAddress(source.getUpsStoreAddress());
        target.setPickStoreAddress(source.getPickStoreAddress());
        if(source.getOwner() instanceof PointOfServiceModel) {
            final PointOfServiceModel pointOfServiceModel = (PointOfServiceModel) source.getOwner();
            target.setUrl("https://www.google.com/maps/place/BorrowLenses/" + "@" + pointOfServiceModel.getLatitude() + "," +
                    pointOfServiceModel.getLongitude());
        }
    }
}
