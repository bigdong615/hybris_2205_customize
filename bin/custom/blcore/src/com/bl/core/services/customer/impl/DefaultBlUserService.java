/**
 *
 */
package com.bl.core.services.customer.impl;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.services.customer.BlUserService;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.security.PrincipalGroupModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.user.impl.DefaultUserService;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


/**
 *  This class created for to check Cs user
 * @author Avani Patel
 *
 */
public class DefaultBlUserService extends DefaultUserService implements BlUserService
{


	private static final Logger LOG = Logger.getLogger(DefaultBlUserService.class);

	/**
	 * method will called to check is logged in user is CS user or not
	 *
	 * @return
	 */
	@Override
	public boolean isCsUser()
	{
		boolean isCsAgent = false;
		final UserModel currentUser = getCurrentUser();
		for (final PrincipalGroupModel userGroup : currentUser.getGroups())
		{
			if (BlInventoryScanLoggingConstants.CUSTOMER_SUPPORT_AGENT_GROUP.equals(userGroup.getUid()))
			{
				isCsAgent = true;
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Logged in user {} is cs user", currentUser);
				break;
			}
		}
		return isCsAgent;
	}

	/**
	 * It checks, Is logged in user tech engineering user or not
	 *
	 * @return boolean value.
	 */
	@Override
	public boolean isTechEngUser() {
		boolean isTechEng = false;
		final UserModel currentUser = getCurrentUser();
		for (final PrincipalGroupModel userGroup : currentUser.getGroups()) {
			if (userGroup.getUid().equals(BlCoreConstants.TECH_ENG_USER_GROUP)) {
				isTechEng = true;
				BlLogger
						.logFormatMessageInfo(LOG, Level.DEBUG, "Logged in user {} is tech engineering user",
								currentUser);
				break;
			}
		}
		return isTechEng;
	}

	/**
	 * It checks, Is logged in user tech engineering/Repair group user or not
	 *
	 * @return boolean value.
	 */
	@Override
	public boolean isTechEngOrRepairUser() {
		boolean isTechEngOrRepairUser = false;
		final UserModel currentUser = getCurrentUser();
		for (final PrincipalGroupModel userGroup : currentUser.getGroups()) {
			if (userGroup.getUid().equals(BlCoreConstants.TECH_ENG_USER_GROUP) || userGroup.getUid().equals(BlCoreConstants.REPAIR_USER_GROUP)) {
				isTechEngOrRepairUser = true;
				BlLogger
						.logFormatMessageInfo(LOG, Level.DEBUG, "Logged in user {} is",
								currentUser);
				break;
			}
		}
		return isTechEngOrRepairUser;
	}
}
