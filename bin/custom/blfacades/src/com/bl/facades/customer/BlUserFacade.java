package com.bl.facades.customer;

import de.hybris.platform.commercefacades.user.UserFacade;
import de.hybris.platform.commercefacades.user.data.AddressData;

/**
 * @author Vijay Vishwakarma
 * This class was created to customize project specific address data.
 */
public interface BlUserFacade extends UserFacade {
    public void setDefaultBillingAddress(final AddressData addressData);
    public AddressData getDefaultBillingAddress();
}
