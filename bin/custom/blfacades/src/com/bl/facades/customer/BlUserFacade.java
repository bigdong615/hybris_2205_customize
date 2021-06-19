package com.bl.facades.customer;

import de.hybris.platform.commercefacades.user.UserFacade;
import de.hybris.platform.commercefacades.user.data.AddressData;

/**
 * @author Vijay Vishwakarma
 * This interface was created to customize bl specific address data.
 */
public interface BlUserFacade extends UserFacade {
    /**
     * This method is responsible for setting default billing address.
     */
    public void setDefaultBillingAddress(final AddressData addressData);
    /**
     * This method is responsible for getting default billing address.
     */
    public AddressData getDefaultBillingAddress();
}
