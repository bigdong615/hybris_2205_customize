package com.bl.facades.populators;

import de.hybris.platform.commercefacades.user.converters.populator.AddressPopulator;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.core.model.user.AddressModel;

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
    }

}
