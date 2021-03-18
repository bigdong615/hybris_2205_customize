package com.bl.core.services.customer.impl;

import com.bl.core.services.customer.BlCustomerAccountService;
import com.bl.logging.BlLogger;
import de.hybris.platform.commerceservices.customer.DuplicateUidException;
import de.hybris.platform.commerceservices.customer.impl.DefaultCustomerAccountService;
import de.hybris.platform.core.model.user.CustomerModel;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 *This is created to override register method and disable registration email.
 *
 * @author vijay vishwakarma
 */

public class DefaultBlCustomerAccountService extends DefaultCustomerAccountService implements BlCustomerAccountService {

  private static final Logger LOGGER = Logger.getLogger(DefaultBlCustomerAccountService.class);
    /**
     *This method is overridden to disable register email.
     */
    @Override
    public void register(final CustomerModel customerModel, final String password) throws DuplicateUidException
    {
        registerCustomer(customerModel, password);
    }
}

