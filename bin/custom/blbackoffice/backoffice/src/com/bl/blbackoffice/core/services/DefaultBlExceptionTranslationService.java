package com.bl.blbackoffice.core.services;

import java.util.Collection;

import org.apache.commons.collections4.CollectionUtils;

import com.hybris.cockpitng.service.ExceptionTranslationHandler;
import com.hybris.cockpitng.service.impl.DefaultExceptionTranslationService;


/**
 *
 * Extended class to handle Custom Exception Error Messages to display on backoffice
 *
 * @author Ravikumar
 *
 */
public class DefaultBlExceptionTranslationService extends DefaultExceptionTranslationService
{
	private Collection<ExceptionTranslationHandler> blExceptionHandlers;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String toString(final Throwable exception)
	{
		if (CollectionUtils.isNotEmpty(getBlExceptionHandlers()))
		{
			for (final ExceptionTranslationHandler exceptionTranslationHandler : getBlExceptionHandlers())
			{
				if (exceptionTranslationHandler.canHandle(exception))
				{
					return exceptionTranslationHandler.toString(exception);
				}
			}
		}
		return super.toString(exception);
	}

	/**
	 * @return the blExceptionHandlers
	 */
	public Collection<ExceptionTranslationHandler> getBlExceptionHandlers()
	{
		return blExceptionHandlers;
	}

	/**
	 * @param blExceptionHandlers
	 *           the blExceptionHandlers to set
	 */
	public void setBlExceptionHandlers(final Collection<ExceptionTranslationHandler> blExceptionHandlers)
	{
		this.blExceptionHandlers = blExceptionHandlers;
	}
}
