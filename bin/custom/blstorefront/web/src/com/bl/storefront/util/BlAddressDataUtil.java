package com.bl.storefront.util;

import com.bl.storefront.forms.BlAddressForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.AddressForm;
import de.hybris.platform.acceleratorstorefrontcommons.util.AddressDataUtil;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;

import org.apache.commons.lang3.BooleanUtils;
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
        final boolean isBillingAddress = BooleanUtils.toBoolean(addressForm.getBillingAddress());
        addressData.setBillingAddress(isBillingAddress);
        addressData.setShippingAddress(BooleanUtils.negate(isBillingAddress));
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
                final String[] openingDD = openingDays.split(";");
                int i=0;
                for(String day : openingDD) {
                    openingDaysMap.put(openingDays.split(";")[i].split(": ")[0], openingDays.split(";")[i].split(": ")[1]);
                    i++;
                }
                addressData.setOpeningDaysDetails(openingDaysMap);
            }
            addressData.setPickStoreAddress(blAddressForm.isPickStoreAddress());
        }
    }

    private void fillExtendedAttributesToAddressForm( final AddressData addressData,AddressForm addressForm) {
        if(addressForm instanceof BlAddressForm) {
            final BlAddressForm blAddressForm = (BlAddressForm) addressForm;
            blAddressForm.setEmail(addressData.getEmail());
            blAddressForm.setCompanyName(addressData.getCompanyName());
            blAddressForm.setAddressType(addressData.getAddressType());
            blAddressForm.setUpsStoreAddress(addressData.getUpsStoreAddress());

        }
    }

    @Override
    public void convertBasic(final AddressData source, final AddressForm target)
    {
        super.convertBasic(source, target);
       // fillExtendedAttributes(target, source);
    }

    @Override
    public void convert(final AddressData source, final AddressForm target)
    {
        super.convert(source, target);
       fillExtendedAttributesToAddressForm(source,target);
       // fillExtendedAttributes(target, source);
    }

    @Override
    public AddressData convertToVisibleAddressData(final AddressForm addressForm)
    {
        final AddressData addressData= super.convertToVisibleAddressData(addressForm);
        BlAddressForm blAddressForm= (BlAddressForm)addressForm;
        addressData.setEmail(blAddressForm.getEmail());
        addressData.setCompanyName(blAddressForm.getCompanyName());
        return addressData;
    }
}
