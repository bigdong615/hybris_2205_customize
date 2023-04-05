/**
 *
 */
package com.bl.facades.populators;

import de.hybris.platform.converters.Populator;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

import com.bl.core.model.BlSerialLogModel;
import com.bl.facades.blSerialLog.data.BlSerialLogData;


/**
 * @author ravi
 *
 */
public class BlSerialLogsPopulator implements Populator<BlSerialLogModel, BlSerialLogData>
{

	@Override
	public void populate(final BlSerialLogModel source, final BlSerialLogData target) throws ConversionException
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

}
