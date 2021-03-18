package com.bl.facades.customer.impl;

import com.bl.core.services.customer.BlCustomerAccountService;
import com.bl.facades.constants.BlFacadesConstants;
import com.bl.facades.customer.BlCustomerFacade;
import com.bl.logging.BlLogger;
import de.hybris.platform.commercefacades.customer.impl.DefaultCustomerFacade;
import de.hybris.platform.commercefacades.user.data.RegisterData;
import de.hybris.platform.commerceservices.customer.DuplicateUidException;
import de.hybris.platform.core.model.user.CustomerModel;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.util.Assert;

import javax.annotation.Resource;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;

/**
 *This is created to override register method to use blRegistration form.
 *
 * @author vijay vishwakarma
 */

public class DefaultBlCustomerFacade extends DefaultCustomerFacade implements BlCustomerFacade {

    private static final Logger LOGGER = Logger.getLogger(DefaultBlCustomerFacade.class);
    /*
    * This is used when we are writing any own personal method in BlCustomerAccountService and want to used it in.
     */
    @Resource (name="customerAccountService")
    private BlCustomerAccountService blcustomerAccountService;

    /*
    * This method is override to remove validation from first name and lastName in registration process.
     */
    @Override
    public void register(final RegisterData registerData) throws DuplicateUidException
    {
        validateParameterNotNullStandardMessage("registerData", registerData);
        Assert.hasText(registerData.getLogin(), "The field [Login] cannot be empty");

        final CustomerModel newCustomer = getModelService().create(CustomerModel.class);
        setCommonPropertiesForRegister(registerData, newCustomer);
        getCustomerAccountService().register(newCustomer, registerData.getPassword());
    }

    /*
    * This method override due to ignore concat of first name and lastName
     */
    @Override
    protected void setCommonPropertiesForRegister(final RegisterData registerData, final CustomerModel customerModel)
    {
        customerModel.setName(BlFacadesConstants.Default_Customer_Name);
        setUidForRegister(registerData, customerModel);
        customerModel.setSessionLanguage(getCommonI18NService().getCurrentLanguage());
        customerModel.setSessionCurrency(getCommonI18NService().getCurrentCurrency());
    }

    public BlCustomerAccountService getBlcustomerAccountService() {
        return blcustomerAccountService;
    }

}
