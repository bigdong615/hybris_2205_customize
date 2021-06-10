package com.bl.facades.customer;

import de.hybris.platform.commercefacades.customer.CustomerFacade;
import de.hybris.platform.commercefacades.user.data.AddressData;

import java.util.List;


public interface BlCustomerFacade extends CustomerFacade
{

	/**
	 * Gets the all visible billing addresses on user.
	 *
	 * @return the all visible billing addresses on user
	 */
	List<AddressData> getAllVisibleBillingAddressesOnUser();

	/**
	 * Gets the address for code.
	 *
	 * @param customerModel
	 *           the customer model
	 * @param code
	 *           the code
	 * @return the address for code
	 */
	AddressData getAddressForCode(final String code);

}
