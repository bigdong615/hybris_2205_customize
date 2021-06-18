package com.bl.facades.customer.impl;

import com.bl.core.services.customer.BlCustomerAccountService;
import com.bl.facades.customer.BlUserFacade;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.commercefacades.user.impl.DefaultUserFacade;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import org.apache.commons.collections.CollectionUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;

/**
 * @author vijay vishwakarma
 *  This class was created to customize project specific address data.
 */
public class DefaultBlUserFacade extends DefaultUserFacade implements BlUserFacade {

    private BlCustomerAccountService customerAccountService;

    /**
     * This method is responsible for setting default billing address.
     * @param addressData
     */
    @Override
    public void setDefaultBillingAddress(final AddressData addressData)
    {
        validateParameterNotNullStandardMessage("addressData", addressData);
        final CustomerModel currentCustomer = (CustomerModel) getUserService().getCurrentUser();
        final AddressModel addressModel = getCustomerAccountService().getAddressForCode(currentCustomer, addressData.getId());
        if (addressModel != null)
        {
            getCustomerAccountService().setDefaultBillingAddress(currentCustomer, addressModel);
        }
    }

    /**
     * This method is override to set default billing address whiling add address.
     * @param addressData
     */
    @Override
    public void addAddress(final AddressData addressData){
        super.addAddress(addressData);
        if(addressData.isDefaultBillingAddress()){
            setDefaultBillingAddress(addressData);
        }
    }

    /**
     * This method is override to get default billing address whiling fetching address book.
     */
    @Override
    public List<AddressData> getAddressBook()
    {
        // Get the current customer's addresses
        final CustomerModel currentUser = (CustomerModel) getUserService().getCurrentUser();
        final Collection<AddressModel> addresses = getCustomerAccountService().getAddressBookDeliveryEntries(currentUser);

        if (CollectionUtils.isNotEmpty(addresses))
        {
            final List<AddressData> result = new ArrayList<>();
            final AddressData defaultAddress = getDefaultAddress();
            final AddressData defaultBillingAddress = getDefaultBillingAddress();

            for (final AddressModel address : addresses)
            {
                final AddressData addressData = getAddressConverter().convert(address);

                if (defaultBillingAddress!= null && defaultBillingAddress.getId() != null && defaultBillingAddress.getId().equals(addressData.getId())){
                    addressData.setDefaultBillingAddress(Boolean.TRUE);
                }

                if (defaultAddress != null && defaultAddress.getId() != null && defaultAddress.getId().equals(addressData.getId()))
                {
                    addressData.setDefaultAddress(true);
                    result.add(0, addressData);
                }
                else
                {
                    result.add(addressData);
                }
            }
            return result;
        }
        return Collections.emptyList();
    }

    /**
     * This method is responsible for getting default billing address.
     */
    @Override
    public AddressData getDefaultBillingAddress()
    {
        final CustomerModel currentCustomer = (CustomerModel) getUserService().getCurrentUser();
        AddressData defaultBillingAddressData = null;

        final AddressModel defaultBilligAddress =currentCustomer.getDefaultBillingAddress();
        if (defaultBilligAddress != null)
        {
            defaultBillingAddressData = getAddressConverter().convert(defaultBilligAddress);
        }
        return defaultBillingAddressData;
    }

    @Override
    public BlCustomerAccountService getCustomerAccountService() {
        return customerAccountService;
    }

    public void setCustomerAccountService(BlCustomerAccountService customerAccountService) {
        this.customerAccountService = customerAccountService;
    }
}
