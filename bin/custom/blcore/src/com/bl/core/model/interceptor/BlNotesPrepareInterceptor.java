package com.bl.core.model.interceptor;

import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import de.hybris.platform.servicelayer.user.UserService;

import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.NotesModel;
import com.bl.logging.BlLogger;


/**
 * Notes Interceptor to perform custom logic on data before saving
 *
 * @author Ravikumar
 *
 */
public class BlNotesPrepareInterceptor implements PrepareInterceptor<NotesModel>
{
	private static final Logger LOG = Logger.getLogger(BlNotesPrepareInterceptor.class);
	private UserService userService;

	@Override
	public void onPrepare(final NotesModel notesModel, final InterceptorContext interceptorContext) throws InterceptorException
	{
		if (StringUtils.isBlank(notesModel.getUserID()))
		{
			final UserModel currentUser = getUserService().getCurrentUser();
			if (Objects.nonNull(currentUser))
			{
				final String currentUserUid = currentUser.getUid();
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Current user id : {}", currentUserUid);
				notesModel.setUserID(currentUserUid);
			}
			else
			{
				BlLogger.logMessage(LOG, Level.ERROR, "Unable to fetch current user from session");
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
