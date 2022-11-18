package com.bl.core.attributes;

import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.servicelayer.model.attribute.AbstractDynamicAttributeHandler;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.dao.BlProductDao;
import com.bl.logging.BlLogger;

public class BlStockLevelForSaleHandler extends
    AbstractDynamicAttributeHandler<Boolean, StockLevelModel> {

	private static final Logger LOGGER = Logger.getLogger(BlStockLevelForSaleHandler.class);

	private BlProductDao productDao;

	@Override
	public Boolean get(final StockLevelModel stockLevel)
	{
		boolean forSale = false;

		try
		{
			final BlSerialProductModel serial = getProductDao().getSerialBySerialCode(stockLevel.getSerialProductCode());
			if (serial != null && serial.getForSale() != null && serial.getForSale())
			{
				forSale = true;
			}
		}
		catch (final Exception exception)
		{
			BlLogger.logFormatMessageInfo(LOGGER, Level.ERROR, "Error occurred while assigning for sale to stockLevel : {}",
					stockLevel.getSerialProductCode(), exception);
		}
		return forSale;
	}

	public BlProductDao getProductDao()
	{
		return productDao;
	}

	public void setProductDao(final BlProductDao productDao)
	{
		this.productDao = productDao;
	}
}
