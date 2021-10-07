/**
 *
 */
package com.bl.core.services.customer;

import de.hybris.platform.servicelayer.user.UserService;


/**
 * This class created for to check Cs user
 * @author Avani Patel
 *
 */
public interface BlUserService extends UserService
{

	/**
	 * method will called to check is logged in user is CS user or not
	 *
	 * @return isCsUser
	 */
	boolean isCsUser();

	/**
	 * It checks, Is logged in user tech engineering user or not
	 *
	 * @return boolean value.
	 */
	boolean isTechEngUser();
}
