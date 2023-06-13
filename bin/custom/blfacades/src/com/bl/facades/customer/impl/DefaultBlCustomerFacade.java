package com.bl.facades.customer.impl;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;

import de.hybris.platform.commercefacades.customer.impl.DefaultCustomerFacade;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.commercefacades.user.data.CustomerData;
import de.hybris.platform.commercefacades.user.data.RegisterData;
import de.hybris.platform.commerceservices.customer.DuplicateUidException;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.core.model.user.UserModel;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.util.Assert;

import com.bl.facades.customer.BlCustomerFacade;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;

/**
 *This is created to override register method to perform bl specific registration.
 *
 * @author vijay vishwakarma
 */
public class DefaultBlCustomerFacade extends DefaultCustomerFacade implements BlCustomerFacade {

	private static final Logger LOG = Logger.getLogger(DefaultBlCustomerFacade.class);

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
    public void setCommonPropertiesForRegister(final RegisterData registerData, final CustomerModel customerModel)
    {
    	//commented as default name not to set as Customer
        //customerModel.setName(BlFacadesConstants.CUSTOMER);
		  customerModel.setName(registerData.getFirstName() + " " + registerData.getLastName());
        setUidForRegister(registerData, customerModel);
        customerModel.setSessionLanguage(getCommonI18NService().getCurrentLanguage());
        customerModel.setSessionCurrency(getCommonI18NService().getCurrentCurrency());
    }

    @Override
 	public List<AddressData> getAllVisibleBillingAddressesOnUser()
 	{
 		try
 		{
 			final UserModel currentUser = getUserService().getCurrentUser();
 			if (Objects.nonNull(currentUser) && !getUserService().isAnonymousUser(currentUser)
 					&& currentUser instanceof CustomerModel)
 			{
 				final List<AddressData> lBillingAddressessOnUser = new ArrayList<>();
 				getCustomerAccountService().getAddressBookEntries((CustomerModel) currentUser).forEach(addressModel -> {
 					if (BooleanUtils.toBoolean(addressModel.getBillingAddress()))
 					{
 						lBillingAddressessOnUser.add(getAddressConverter().convert(addressModel));
 					}
 				});
 				return lBillingAddressessOnUser;
 			}
 		}
 		catch (final Exception exception)
 		{
 			BlLogger.logMessage(LOG, Level.ERROR, "Error while retriving all visible billing addresses from user", exception);
 		}
 		return Lists.newArrayList();
 	}

    @Override
    public AddressData getAddressForCode(final String code)
    {
   	 try
   	 {
   		 final UserModel currentUser = getUserService().getCurrentUser();
      	 if(StringUtils.isNotBlank(code) && Objects.nonNull(currentUser) && !getUserService().isAnonymousUser(currentUser)
   					&& currentUser instanceof CustomerModel)
      	 {
      		 return getAddressConverter().convert(getCustomerAccountService().getAddressForCode((CustomerModel)currentUser, code));
      	 }
   	 }
   	 catch(final Exception exception)
   	 {
   		 BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception, "Error while getting address for code - {}", code);
   	 }
   	 return null;
    }

    /**
 	 * {@inheritDoc}
 	 */
 	@Override
 	public AddressData getDefaultBillingAddress()
 	{
 		try
 		{
 			final CustomerModel currentUser = getCurrentSessionCustomer();
 			if (Objects.nonNull(currentUser) && !getUserService().isAnonymousUser(currentUser))
 			{
 				final AddressModel defaultPaymentAddress = currentUser.getDefaultBillingAddress();
 				return Objects.nonNull(defaultPaymentAddress) && BooleanUtils.isTrue(defaultPaymentAddress.getBillingAddress())
 						? getAddressConverter().convert(defaultPaymentAddress)
 						: null;
 			}
 		}
 		catch (final Exception exception)
 		{
 			BlLogger.logMessage(LOG, Level.ERROR, "Error while getting default billing address from current session customer",
 					exception);
 		}
 		return null;
 	}

	/**
	 * This method used for sending reset password link to ESP.
	 * @param id
	 * 		the id of the customer to send the forgotten password mail for.
	 */
	@Override
	public void forgottenPassword(final String id)
	{
		Assert.hasText(id, "The field [id] cannot be empty");
		final CustomerModel customerModel = getUserMatchingService().getUserByProperty(id, CustomerModel.class);
		getCustomerAccountService().forgottenPassword(customerModel);
	}


	@Override
	public void updateUserProfile(final CustomerData customerData) throws DuplicateUidException
	{
		//validateDataBeforeUpdate(customerData);

		final CustomerModel customer = getCurrentSessionCustomer();
		//customer.setOriginalUid(customerData.getDisplayUid());
		getCustomerAccountService().updateProfile(customer, customerData.getTitleCode(), customerData.getName(),
				customerData.getUid());
	}
}