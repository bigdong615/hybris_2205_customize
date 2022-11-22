package com.bl.core.attributes;

import de.hybris.platform.servicelayer.model.attribute.AbstractDynamicAttributeHandler;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.BlSerialProductModel;
import com.bl.core.stock.BlStockService;
import com.bl.logging.BlLogger;

public class BlVisibleOnStorefrontHandler extends
    AbstractDynamicAttributeHandler<Boolean, BlSerialProductModel> {

	private static final Logger LOGGER = Logger.getLogger(BlVisibleOnStorefrontHandler.class);

	private BlStockService blStockService;

	@Override
	public Boolean get(final BlSerialProductModel blSerialProduct)
	{
		boolean forSale = false;

		try
		{
			if (getBlStockService().isActiveStatus(blSerialProduct.getSerialStatus()) && !blSerialProduct.getHardAssigned()
					&& !blSerialProduct.getSoftAssigned())
			{
				forSale = true;
			}
		}
		catch (final Exception exception)
		{
			BlLogger.logFormatMessageInfo(LOGGER, Level.ERROR, "Error occurred while checking is visible on storefront : {}",
					blSerialProduct.getCode(), exception);
		}
		return forSale;
	}

	public BlStockService getBlStockService()
	{
		return blStockService;
	}

	public void setBlStockService(final BlStockService blStockService)
	{
		this.blStockService = blStockService;
	}
}
