package com.bl.facades.populators;

import de.hybris.platform.converters.Populator;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.NotesModel;
import com.bl.facades.customerNotes.data.NotesData;
import com.bl.facades.process.email.impl.DefaultBlDomoFailureNotificationService;
import com.bl.logging.BlLogger;


public class BlDomoNotesPopulator implements Populator<NotesModel, NotesData>
{
	private static final Logger LOG = Logger.getLogger(BlDomoNotesPopulator.class);
	private DefaultBlDomoFailureNotificationService defaultBlDomoFailureNotificationService;

	@Override
	public void populate(final NotesModel source, final NotesData target) throws ConversionException
	{
		try
		{
			target.setCreatedTS(source.getCreationtime());
			target.setModifiedTS(source.getModifiedtime());
			target.setUserId(source.getUserID());
			if (source.getType() != null)
			{
				target.setType(source.getType().getCode());
			}
			if (source.getOrder() != null)
			{
				target.setOrder(source.getOrder().getCode());
			}

			target.setNote(source.getNote());
			target.setPrimaryKey(source.getPk().toString());
		}
		catch (final Exception exception)
		{
			getDefaultBlDomoFailureNotificationService().send(exception.toString(), source.getPk().toString(), "Notes api");
			LOG.error("Error while getting Notes for PK " + source.getPk().toString());
			BlLogger.logMessage(LOG, Level.ERROR, StringUtils.EMPTY, "Error while getting Notes", exception);
			exception.printStackTrace();

		}

	}

	/**
	 * @return the defaultBlDomoFailureNotificationService
	 */
	public DefaultBlDomoFailureNotificationService getDefaultBlDomoFailureNotificationService()
	{
		return defaultBlDomoFailureNotificationService;
	}

	/**
	 * @param defaultBlDomoFailureNotificationService
	 *           the defaultBlDomoFailureNotificationService to set
	 */
	public void setDefaultBlDomoFailureNotificationService(
			final DefaultBlDomoFailureNotificationService defaultBlDomoFailureNotificationService)
	{
		this.defaultBlDomoFailureNotificationService = defaultBlDomoFailureNotificationService;
	}
}
