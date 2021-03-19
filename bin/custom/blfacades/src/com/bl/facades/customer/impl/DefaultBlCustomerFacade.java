package com.bl.facades.customer.impl;

import com.bl.facades.constants.BlFacadesConstants;
import com.bl.facades.customer.BlCustomerFacade;
import de.hybris.platform.commercefacades.customer.impl.DefaultCustomerFacade;
import de.hybris.platform.commercefacades.user.data.RegisterData;
import de.hybris.platform.commerceservices.customer.DuplicateUidException;
import de.hybris.platform.core.model.user.CustomerModel;
import org.springframework.util.Assert;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;

/**
 *This is created to override register method to use blRegistration form.
 *
 * @author vijay vishwakarma
 */
public class DefaultBlCustomerFacade extends DefaultCustomerFacade implements BlCustomerFacade {

    /*
    * This method is override to remove validation from first name and lastName in registration process.
     */
    @Override
    public void register(final RegisterData registerData) throws DuplicateUidException
    {
        validateParameterNotNullStandardMessage("registerData", registerData); // This is also call from other place
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
        customerModel.setName(BlFacadesConstants.DEFAULT_CUSTOMER_NAME);
        setUidForRegister(registerData, customerModel);
        customerModel.setSessionLanguage(getCommonI18NService().getCurrentLanguage());
        customerModel.setSessionCurrency(getCommonI18NService().getCurrentCurrency());
    }
}