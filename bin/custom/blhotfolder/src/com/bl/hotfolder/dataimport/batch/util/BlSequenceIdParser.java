package com.bl.hotfolder.dataimport.batch.util;

import de.hybris.platform.acceleratorservices.dataimport.batch.util.SequenceIdParser;

import java.io.File;

import org.springframework.util.Assert;


public class BlSequenceIdParser extends SequenceIdParser
{
	@Override
	public Long getSequenceId(final File file)
	{
		Long result = null;
		Assert.notNull(file);
		final String fileName = file.getName();
		final String part = getParser().parse(fileName, 0);
		if (part != null)
		{
			result = Long.valueOf(part);
		}
		else
		{
			throw new IllegalArgumentException("missing sequenceId in " + fileName);
		}
		return result;
	}
}