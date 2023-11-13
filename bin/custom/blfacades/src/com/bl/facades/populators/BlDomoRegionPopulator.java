package com.bl.facades.populators;

import de.hybris.platform.commercefacades.user.data.RegionData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.c2l.RegionModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.facades.process.email.impl.DefaultBlDomoFailureNotificationService;
import com.bl.logging.BlLogger;


public class BlDomoRegionPopulator implements Populator<RegionModel, RegionData>
{
	private static final Logger LOG = Logger.getLogger(BlDomoRegionPopulator.class);
	private DefaultBlDomoFailureNotificationService defaultBlDomoFailureNotificationService;

	@Override
	public void populate(final RegionModel source, final RegionData target) throws ConversionException
	{
		try
		{
			target.setCreatedTS(source.getCreationtime());
			target.setModifiedTS(source.getModifiedtime());
			target.setName(source.getName());
			target.setIsocode(source.getIsocode());
			target.setIsocodeShort(source.getIsocodeShort());
			target.setActive(source.getActive());
			if (source.getCountry() != null)
			{
				target.setCountryIso(source.getCountry().getIsocode());
			}
			target.setComments(source.getComments().stream().map(String::valueOf).collect(Collectors.joining(",")));
			target.setPrimaryKey(source.getPk().toString());
		}
		catch (final Exception exception)
		{
			getDefaultBlDomoFailureNotificationService().send(exception.toString(), source.getPk().toString(), "Region api");
			LOG.error("Error while getting Region for PK " + source.getPk().toString());
			BlLogger.logMessage(LOG, Level.ERROR, StringUtils.EMPTY, "Error while getting Region", exception);
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
