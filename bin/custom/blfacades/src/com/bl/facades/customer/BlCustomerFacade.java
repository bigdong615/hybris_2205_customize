package com.bl.facades.customer;

import de.hybris.platform.commercefacades.customer.CustomerFacade;
import de.hybris.platform.commercefacades.user.data.AddressData;
import java.util.List;

/**
*Custom Customer facade to execute custom logic related to Customer
*
*
*/
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
	
	/**
	 * Gets the default billing address from customer.
	 *
	 * @return the default billing address
	 */
	AddressData getDefaultBillingAddress();

}
