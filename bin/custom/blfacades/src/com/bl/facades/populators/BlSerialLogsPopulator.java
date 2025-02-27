/**
 *
 */
package com.bl.facades.populators;

import de.hybris.platform.converters.Populator;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

import com.bl.core.model.BlSerialLogModel;
import com.bl.facades.blSerialLog.data.BlSerialLogData;
import com.bl.facades.process.email.impl.DefaultBlDomoFailureNotificationService;


/**
 * @author ravi
 *
 */
public class BlSerialLogsPopulator implements Populator<BlSerialLogModel, BlSerialLogData>
{
	private DefaultBlDomoFailureNotificationService defaultBlDomoFailureNotificationService;

	@Override
	public void populate(final BlSerialLogModel source, final BlSerialLogData target) throws ConversionException
	{
		try
		{
			target.setCreatedTS(source.getCreationtime());
			target.setModifiedTS(source.getModifiedtime());
			target.setSerialsLogID(source.getSerialsLogID());
			target.setLogUserName(source.getLogUserName());
			target.setSerialNumber(source.getSerialNumber());
			if (source.getSerialsId() != null)
			{
				target.setSerialsId(source.getSerialsId().getCode());
			}
			target.setItemBarcode(source.getItemBarcode());
			target.setLogTime(source.getLogTime());
			target.setWillBeActiveOn(source.getWillBeActiveOn());
			target.setLogChanged(source.getLogChanged());
			if (source.getSerialStatus() != null)
			{
				target.setSerialStatus(source.getSerialStatus().getCode());
			}
			target.setNotes(source.getNotes());
			target.setConditions(source.getConditions());
			target.setFirmwareVersion(source.getFirmwareVersion());
			target.setIsAvailableForSale(source.isIsAvailableForSale());
			target.setForSalePrice(source.getForSalePrice());
			target.setForSalePricingValue(source.getForSalePricingValue());
			if (source.getWithOrderId() != null)
			{
				target.setWithOrderId(source.getWithOrderId().getCode());
			}
			target.setPrimaryKey(source.getPk().toString());
		}
		catch (final Exception exception)
		{
			getDefaultBlDomoFailureNotificationService().send(exception.toString(), source.getPk().toString(), "BlSerialLog api");
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
