package com.bl.facades.customer;

import de.hybris.platform.commercefacades.customer.CustomerFacade;
import de.hybris.platform.commercefacades.user.data.AddressData;

import de.hybris.platform.commercefacades.user.exceptions.PasswordMismatchException;
import de.hybris.platform.commerceservices.customer.DuplicateUidException;
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

	/**
	 * Change the current customer's UID. The current password is required for 2 reasons, firstly to validate that the
	 * current visitor is actually the customer, secondly the password hash may be salted with the UID and therefore if
	 * the UID is changed then the password needs to be re-hashed.
	 *
	 * @param newUid
	 * 		the new UID for the current customer
	 * @param currentPassword
	 * 		current user password to validate user
	 * @throws PasswordMismatchException
	 * 		thrown if the password is invalid
	 * @throws DuplicateUidException
	 * 		thrown if the newUid is already in use
	 */
	void changeUid(String newUid, String currentPassword) throws DuplicateUidException, PasswordMismatchException;

}
