/**
 *
 */
package com.bl.core.services.customer.impl;

import com.bl.core.model.interceptor.BlAddressPrepareInterceptor;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.security.PrincipalGroupModel;
import de.hybris.platform.core.model.user.UserModel;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.services.customer.BlUserService;
import de.hybris.platform.servicelayer.user.impl.DefaultUserService;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


/**
 * @author Administrator
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
}
