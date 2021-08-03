package com.bl.core.services.customer;

import de.hybris.platform.commerceservices.customer.CustomerAccountService;
import de.hybris.platform.commerceservices.customer.DuplicateUidException;
import de.hybris.platform.commerceservices.customer.PasswordMismatchException;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;

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
     * Changes uid for current user
     *
     * @param newUid
     *           given new uid
     * @param currentPassword
     *           password checked for authorization change
     * @throws DuplicateUidException
     *            if the newUid already exists in the system
     * @throws PasswordMismatchException
     *            if given currentPassword does not match the store one for the current user
     */
    void changeUid(final String newUid, final String currentPassword) throws DuplicateUidException, PasswordMismatchException;
}
