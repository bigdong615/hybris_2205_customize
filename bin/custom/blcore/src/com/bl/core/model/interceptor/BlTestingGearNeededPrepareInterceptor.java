package com.bl.core.model.interceptor;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import de.hybris.platform.servicelayer.user.UserService;

import java.util.Objects;

import javax.annotation.Resource;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.BlTestingGearNeededModel;
import com.bl.logging.BlLogger;


/**
 * The Class BlTestingGearNeededPrepareInterceptor used to intercept the BlTestingGearNeededModel and modify the
 * attributes before saving the data.
 *
 * @author Ravikumar
 *
 */
public class BlTestingGearNeededPrepareInterceptor implements PrepareInterceptor<BlTestingGearNeededModel>
{
	private static final Logger LOG = Logger.getLogger(BlTestingGearNeededPrepareInterceptor.class);

	@Resource(name = "userService")
	private UserService userService;

	@Override
	public void onPrepare(final BlTestingGearNeededModel blTestingGearNeededModel, final InterceptorContext interceptorContext)
			throws InterceptorException
	{
		validateParameterNotNull(blTestingGearNeededModel,
				"ERROR : BlTestingGearNeededPrepareInterceptor : Parameter blTestingGearNeededModel is NULL");
		fetchAndAddRequestedUser(blTestingGearNeededModel, interceptorContext);
	}

	/**
	 * Fetch and add requested user.
	 *
	 * @param blTestingGearNeededModel
	 *           the bl testing gear needed model
	 * @param interceptorContext
	 *           the interceptor context
	 */
	private void fetchAndAddRequestedUser(final BlTestingGearNeededModel blTestingGearNeededModel,
			final InterceptorContext interceptorContext)
	{
		if (interceptorContext.isNew(blTestingGearNeededModel))
		{
			final UserModel currentUser = getUserService().getCurrentUser();
			if (Objects.nonNull(currentUser))
			{
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Current User for BlTestingGearNeeded : {}", currentUser.getUid());
				blTestingGearNeededModel.setRequestedUser(currentUser);
			}
		}
	}

	/**
	 * @return the userService
	 */
	public UserService getUserService()
	{
		return userService;
	}

	/**
	 * @param userService
	 *           the userService to set
	 */
	public void setUserService(final UserService userService)
	{
		this.userService = userService;
	}
}
