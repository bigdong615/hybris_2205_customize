/**
 *
 */
package com.bl.facades.populators;

import de.hybris.platform.converters.Populator;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.CustomerNotesModel;
import com.bl.facades.customerNotes.data.CustomerNotesData;
import com.bl.logging.BlLogger;


/**
 * @author
 *
 */
public class BlDomoCustomerNotesPopulator implements Populator<CustomerNotesModel, CustomerNotesData>
{
	private static final Logger LOG = Logger.getLogger(BlDomoCustomerNotesPopulator.class);

	@Override
	public void populate(final CustomerNotesModel source, final CustomerNotesData target) throws ConversionException
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
		if (source.getCustomerNoteType() != null)
		{
			target.setCustomerNoteType(source.getCustomerNoteType().getCode());
		}
		target.setFraud(source.isFraud());
		if (source.getCustomer() != null)
		{
			target.setCustomer(source.getCustomer().getUid());
		}
		target.setNote(source.getNote());
		target.setPrimaryKey(source.getPk().toString());
	}
	catch (final Exception exception)
	{
		LOG.info("Error while getting ConsignmentEntry info for PK " + source.getPk().toString());
		BlLogger.logMessage(LOG, Level.ERROR, StringUtils.EMPTY, "Error while getting ConsignmentEntryModel info", exception);
		exception.printStackTrace();

	}
	}

}
