package com.bl.core.services.customer.impl;

import com.bl.core.services.customer.BlCustomerAccountService;
import de.hybris.platform.commerceservices.customer.DuplicateUidException;
import de.hybris.platform.commerceservices.customer.impl.DefaultCustomerAccountService;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;

import java.util.ArrayList;
import java.util.List;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

/**
 *This is created to override register method and disable registration email.
 *
 * @author vijay vishwakarma
 */

public class DefaultBlCustomerAccountService extends DefaultCustomerAccountService implements BlCustomerAccountService {

    /**
     *This method is overridden to disable register email.
     */
    @Override
    public void register(final CustomerModel customerModel, final String password) throws DuplicateUidException
    {
        registerCustomer(customerModel, password);
    }

    /**
     * This method is responsible for setting default billing address with current customer.
     */
    @Override
    public void setDefaultBillingAddress(final CustomerModel customerModel, final AddressModel addressModel)
    {
        validateParameterNotNull(customerModel, "Customer model cannot be null");
        validateParameterNotNull(addressModel, "Address model cannot be null");
        if (customerModel.getAddresses().contains(addressModel))
        {
            customerModel.setDefaultBillingAddress(addressModel);
        }
        else
        {
            final AddressModel clone = getModelService().clone(addressModel);
            clone.setOwner(customerModel);
            getModelService().save(clone);
            final List<AddressModel> customerAddresses = new ArrayList<>();
            customerAddresses.addAll(customerModel.getAddresses());
            customerAddresses.add(clone);
            customerModel.setAddresses(customerAddresses);
            customerModel.setDefaultBillingAddress(clone);
        }
        getModelService().save(customerModel);
        getModelService().refresh(customerModel);
    }
}

