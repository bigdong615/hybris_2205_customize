package com.bl.facades.populators;

import de.hybris.platform.converters.Populator;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

import com.bl.core.model.NotesModel;
import com.bl.facades.customerNotes.data.NotesData;


public class BlDomoNotesPopulator implements Populator<NotesModel, NotesData>
{
	@Override
	public void populate(final NotesModel source, final NotesData target) throws ConversionException
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
	}
}