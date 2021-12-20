package com.bl.blbackoffice.services.handlers;

import de.hybris.platform.platformbackoffice.services.handlers.ModelExceptionTranslationHandler;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;

import java.util.List;
import java.util.Objects;

import org.apache.commons.lang3.StringUtils;

import com.bl.core.constants.BlCoreConstants;
import com.google.common.collect.Lists;
import com.hybris.cockpitng.dataaccess.facades.object.exceptions.ObjectSavingException;


/**
 * This class is use to handle Custom Exception Error Messages to display on backoffice
 *
 * @author Ravikumar
 *
 */
public class BlModelSavingExceptionTranslationHandler extends ModelExceptionTranslationHandler
{
	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean canHandle(final Throwable exception)
	{
		return isEligibleForCustomMessage(exception);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String toString(final Throwable exception)
	{
		if (Objects.nonNull(exception.getCause().getCause()))
		{
			return getCustomMessage(exception.getCause().getCause().getMessage());
		}
		return this.getLabelByKey(BlCoreConstants.COMMON_ERROR_MESSAGE);
	}

	/**
	 * Checks if is eligible for custom message.
	 *
	 * @param exception
	 *           the exception
	 * @return true, if is eligible for custom message
	 */
	private boolean isEligibleForCustomMessage(final Throwable exception)
	{
		if ((exception instanceof ModelSavingException || exception.getCause() instanceof ModelSavingException)
				|| exception instanceof ObjectSavingException)
		{
			return isExceptionFromValidInterceptor(exception);
		}
		return Boolean.FALSE;
	}

	/**
	 * Checks if is exception from valid interceptor.
	 *
	 * @param exception
	 *           the exception
	 * @return true, if is exception from valid interceptor
	 */
	private boolean isExceptionFromValidInterceptor(final Throwable exception)
	{
		if (Objects.nonNull(exception.getCause()) && StringUtils.isNotBlank(exception.getCause().getMessage()))
		{
			final String message = exception.getCause().getMessage();
			for(final String objectName : getCustomExceptionObjectsList())
			{
				if(message.contains(objectName))
				{
					return true;
				}
			}
		}
		return Boolean.FALSE;
	}
	
	/**
	 * Gets the custom exception objects list.
	 *
	 * @return the custom exception objects list
	 */
	private List<String> getCustomExceptionObjectsList()
	{
		final List<String> customExceptionObjectsList = Lists.newArrayList();
		customExceptionObjectsList.add(BlCoreConstants.BL_SERIAL_PRODUCT_VALIDATE_INTERCEPTOR);
		customExceptionObjectsList.add(BlCoreConstants.BL_BLACKOUT_DATE_MODEL);
		customExceptionObjectsList.add(BlCoreConstants.BL_PRODUCT_VALIDATE_INTERCEPTOR);
		customExceptionObjectsList.add(BlCoreConstants.BL_ORDER_VALIDATE_INTERCEPTOR);
		customExceptionObjectsList.add(BlCoreConstants.BL_REPAIR_LOG_PREPARE_INTERCEPTOR);
		return customExceptionObjectsList;
	}

	/**
	 * Gets the custom message.
	 *
	 * @param message
	 *           the message
	 * @return the custom message
	 */
	private String getCustomMessage(final String message)
	{
		if (StringUtils.isNotBlank(message) && message.contains(":"))
		{
			final String[] splitMessgae = message.split(":");
			return splitMessgae.length > 1 ? splitMessgae[1] : this.getLabelByKey(BlCoreConstants.COMMON_ERROR_MESSAGE);
		}
		return this.getLabelByKey(BlCoreConstants.COMMON_ERROR_MESSAGE);
	}
}
