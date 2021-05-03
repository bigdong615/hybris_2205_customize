package com.bl.storefront.util;

import com.bl.storefront.forms.BlAddressForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.AddressForm;
import de.hybris.platform.acceleratorstorefrontcommons.util.AddressDataUtil;
import de.hybris.platform.commercefacades.user.data.AddressData;

public class BlAddressDataUtil extends AddressDataUtil {

    @Override
    public AddressData convertToAddressData(final AddressForm addressForm)
    {
        final AddressData addressData = super.convertToAddressData(addressForm);
        fillExtendedAttributes(addressForm, addressData);
        return addressData;
    }

    private void fillExtendedAttributes(AddressForm addressForm, final AddressData addressData) {
        if(addressForm instanceof BlAddressForm) {
            final BlAddressForm blAddressForm = (BlAddressForm) addressForm;
            addressData.setEmail(blAddressForm.getEmail());
            addressData.setAddressType(blAddressForm.getAddressType());
        }
    }

    @Override
    public void convertBasic(final AddressData source, final AddressForm target)
    {
        super.convertBasic(source, target);
        fillExtendedAttributes(target, source);
    }

    @Override
    public void convert(final AddressData source, final AddressForm target)
    {
        super.convert(source, target);
        fillExtendedAttributes(target, source);
    }

    @Override
    public AddressData convertToVisibleAddressData(final AddressForm addressForm)
    {
        return super.convertToVisibleAddressData(addressForm);
    }
}
