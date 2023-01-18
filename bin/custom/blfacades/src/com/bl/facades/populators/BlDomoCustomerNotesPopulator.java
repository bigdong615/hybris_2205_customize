/**
 *
 */
package com.bl.facades.populators;

import de.hybris.platform.converters.Populator;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

import com.bl.core.model.CustomerNotesModel;
import com.bl.facades.customerNotes.data.CustomerNotesData;


/**
 * @author
 *
 */
public class BlDomoCustomerNotesPopulator implements Populator<CustomerNotesModel, CustomerNotesData>
{

	@Override
	public void populate(final CustomerNotesModel source, final CustomerNotesData target) throws ConversionException
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
	}

}
