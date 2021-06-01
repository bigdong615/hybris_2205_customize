package com.bl.facades.populators;

import com.bl.core.enums.AddressTypeEnum;
import de.hybris.platform.commercefacades.user.converters.populator.AddressReversePopulator;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

public class BlAddressReversePopulator extends AddressReversePopulator {

    @Override
    public void populate(final AddressData addressData, final AddressModel addressModel) throws ConversionException
    {
        super.populate(addressData, addressModel);
        if(addressData.getAddressType() != null) {
            addressModel.setAddressType(AddressTypeEnum.valueOf(addressData.getAddressType()));
        }
        addressModel.setOpeningDaysDetails(addressData.getOpeningDaysDetails());
        addressModel.setPickStoreAddress(addressData.getPickStoreAddress());
        addressModel.setUpsStoreAddress(addressData.getUpsStoreAddress());
    }
}
