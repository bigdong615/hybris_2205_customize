package com.bl.core.services.customer;

import de.hybris.platform.commerceservices.customer.CustomerAccountService;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;

public interface BlCustomerAccountService extends CustomerAccountService {
    public void setDefaultBillingAddress(final CustomerModel customerModel, final AddressModel addressModel);
}
