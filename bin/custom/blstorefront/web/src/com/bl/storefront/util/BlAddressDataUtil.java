package com.bl.storefront.util;

import com.bl.storefront.forms.BlAddressForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.AddressForm;
import de.hybris.platform.acceleratorstorefrontcommons.util.AddressDataUtil;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import org.apache.commons.lang3.StringUtils;

import javax.annotation.Resource;
import java.util.HashMap;
import java.util.Map;

public class BlAddressDataUtil extends AddressDataUtil {

    @Resource(name="commonI18NService")
    private CommonI18NService commonI18NService;

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
            addressData.setUpsStoreAddress(blAddressForm.isUpsStoreAddress());
            final java.lang.String openingDays = blAddressForm.getOpeningDaysDetails();
            if(StringUtils.isNotEmpty(openingDays)) {
                Map<String, String> openingDaysMap = new HashMap<>();
                openingDaysMap.put(openingDays.split(";")[0].split(": ")[0], openingDays.split(";")[0].split(": ")[1]);
                openingDaysMap.put(openingDays.split(";")[1].split(": ")[0], openingDays.split(";")[1].split(": ")[1]);
                openingDaysMap.put(openingDays.split(";")[2].split(": ")[0], openingDays.split(";")[2].split(": ")[1]);
                addressData.setOpeningDaysDetails(openingDaysMap);
            }
            addressData.setPickStoreAddress(blAddressForm.isPickStoreAddress());
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
