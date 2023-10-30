package com.bl.facades.populators;

import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.warehousingfacades.product.data.StockLevelData;
import de.hybris.platform.warehousingfacades.stocklevel.converters.populator.WarehousingStockLevelPopulator;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.logging.BlLogger;


public class BlStockLevelPopulator extends WarehousingStockLevelPopulator
{
	private static final Logger LOG = Logger.getLogger(BlStockLevelPopulator.class);

	@Override
	public void populate(final StockLevelModel source, final StockLevelData target) throws ConversionException
	{
		try
		{
		//super.populate(source, target);
		target.setCreatedTS(source.getCreationtime());
		target.setModifiedTS(source.getModifiedtime());
		target.setNextDeliveryTime(source.getNextDeliveryTime());
		target.setDate(source.getDate());
		target.setAvailable(source.getAvailable());
		target.setReserved(source.getReserved());
		target.setOverSelling(source.getOverSelling());
		target.setPreOrder(source.getPreOrder());
		target.setMaxPreOrder(source.getMaxPreOrder());
		target.setMaxStockLevelHistoryCount(source.getMaxStockLevelHistoryCount());
		target.setTreatNegativeAsZero(source.isTreatNegativeAsZero());
		if (source.getAsnEntry() != null)
		{
			target.setAsnEntry(source.getAsnEntry().getProductCode());
		}
		if (source.getReservedStatus() != null)
		{
			target.setReservedStatus(source.getReservedStatus());
		}
		if (source.getHardAssigned() != null)
		{
			target.setHardAssigned(source.getHardAssigned());
		}
		if (source.getBufferedInventory() != null)
		{
			target.setBufferedInventory(source.getBufferedInventory());
		}
		if (source.getForSale() != null)
		{
			target.setForSale(source.getForSale());
		}
		target.setSerialProductCode(source.getSerialProductCode());
		target.setProductCode(source.getProductCode());
		target.setBin(source.getBin());
		target.setOrder(source.getOrder());
		if (source.getSerialStatus() != null)
		{
			target.setSerialStatus(source.getSerialStatus().getCode());
		}
		target.setReleaseDate(source.getReleaseDate());
		target.setInStockStatus(source.getInStockStatus());
		target.setWarehouseCode(source.getWarehouse().getCode());
		target.setPrimaryKey(source.getPk().toString());
	}
	catch (final Exception exception)
	{
		LOG.info("Error while getting stocklevel for PK " + source.getPk().toString());
		BlLogger.logMessage(LOG, Level.ERROR, StringUtils.EMPTY, "Error while getting stocklevel", exception);
		exception.printStackTrace();

	}
	}
}
