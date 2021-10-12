package com.bl.core.services.customer;

import de.hybris.platform.commerceservices.customer.CustomerAccountService;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import java.util.List;

/**
 * @author Vijay Vishwakarma
 *  This interface was created to provide bl specific customer account-related functionality.
 */
public interface BlCustomerAccountService extends CustomerAccountService {
    /**
     * This method is responsible for setting default billing addresses with current customers.
     */
    public void setDefaultBillingAddress(final CustomerModel customerModel, final AddressModel addressModel);
    /**
     *  This method is used for getting shipping address entries.
     * @param customerModel customer
     * @return address list
     */
     List<AddressModel> getShippingAddressBookEntries(final CustomerModel customerModel);
}
