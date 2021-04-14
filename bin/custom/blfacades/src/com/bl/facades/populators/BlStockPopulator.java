package com.bl.facades.populators;

import de.hybris.platform.basecommerce.enums.StockLevelStatus;
import de.hybris.platform.commercefacades.product.data.StockData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;

import java.time.LocalDate;
import java.util.Date;

import com.bl.core.data.StockResult;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.utils.BlDateTimeUtils;


/**
 * This class is used to populate the stock data
 *
 * @author Moumita
 */
public class BlStockPopulator<SOURCE extends ProductModel, TARGET extends StockData> implements Populator<SOURCE, TARGET>
{
	private BaseStoreService baseStoreService;
	private BlCommerceStockService blCommerceStockService;
	private SessionService sessionService;

	/**
	 * It populates the stock status and available quantity
	 *
	 * @param blProductModel
	 * @param stockData
	 */
	@Override
	public void populate(final SOURCE blProductModel, final TARGET stockData)
	{
		final BaseStoreModel baseStore = getBaseStoreService().getCurrentBaseStore();
		final LocalDate startDate = getSessionService().getAttribute("selectedFromDate");
		final LocalDate endDate = getSessionService().getAttribute("selectedToDate");
		if (null != startDate && null != endDate) {
			final Date startDay = BlDateTimeUtils
					.convertStringDateToDate(startDate.toString(), "yyyy-MM-dd");
			final Date endDay = BlDateTimeUtils
					.convertStringDateToDate(endDate.toString(), "yyyy-MM-dd");
			final StockResult stockResult = getBlCommerceStockService().getStockForEntireDuration(
					blProductModel.getCode(), baseStore.getWarehouses(), startDay, endDay);
			final StockLevelStatus stockLevelStatus = stockResult.getStockLevelStatus();
			stockData.setStockLevelStatus(stockLevelStatus);
			if (StockLevelStatus.LOWSTOCK.equals(stockLevelStatus)) {
				stockData
						.setStockLevel(stockResult.getAvailableCount());
			}
		}
	}

	/**
	 * @return the baseStoreService
	 */
	public BaseStoreService getBaseStoreService()
	{
		return baseStoreService;
	}

	/**
	 * @param baseStoreService
	 *           the baseStoreService to set
	 */
	public void setBaseStoreService(final BaseStoreService baseStoreService)
	{
		this.baseStoreService = baseStoreService;
	}

	/**
	 * @return the blCommerceStockService
	 */
	public BlCommerceStockService getBlCommerceStockService()
	{
		return blCommerceStockService;
	}

	/**
	 * @param blCommerceStockService
	 *           the blCommerceStockService to set
	 */
	public void setBlCommerceStockService(final BlCommerceStockService blCommerceStockService)
	{
		this.blCommerceStockService = blCommerceStockService;
	}

	/**
	 * @return the sessionService
	 */
	public SessionService getSessionService()
	{
		return sessionService;
	}

	/**
	 * @param sessionService
	 *           the sessionService to set
	 */
	public void setSessionService(final SessionService sessionService)
	{
		this.sessionService = sessionService;
	}
}