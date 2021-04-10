package com.bl.facades.populators;

import de.hybris.platform.basecommerce.enums.StockLevelStatus;
import de.hybris.platform.category.CategoryService;
import de.hybris.platform.commercefacades.product.data.StockData;
import de.hybris.platform.commerceservices.stock.CommerceStockService;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.site.BaseSiteService;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;

import java.time.LocalDate;
import java.util.Date;

import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.utils.BookingDateUtils;


/**
 * This class is used to populate the stock data
 *
 * @author Moumita
 */
public class BlStockPopulator<SOURCE extends ProductModel, TARGET extends StockData> implements Populator<SOURCE, TARGET>
{
	private CommerceStockService commerceStockService;
	private BaseStoreService baseStoreService;
	private BaseSiteService baseSiteService;
	private CategoryService categoryService;
	private BlCommerceStockService blCommerceStockService;
	private SessionService sessionService;

	/**
	 * It populates the stock status and available quantity
	 *
	 * @param blproductModel
	 * @param stockData
	 */
	@Override
	public void populate(final SOURCE blproductModel, final TARGET stockData) throws ConversionException
	{
		final BaseStoreModel baseStore = getBaseStoreService().getCurrentBaseStore();
		if (!isStockSystemEnabled(baseStore))
		{
			stockData.setStockLevelStatus(StockLevelStatus.INSTOCK);
			stockData.setStockLevel(Long.valueOf(0));
		}
		else
		{
			final LocalDate startDate = getSessionService().getAttribute("selectedFromDate");
			final LocalDate endDate = getSessionService().getAttribute("selectedToDate");
			if (null != startDate && null != endDate)
			{
				final Date startDay = BookingDateUtils.convertStringDateToDate(startDate.toString(), "yyyy-MM-dd");
				final Date endDay = BookingDateUtils.convertStringDateToDate(endDate.toString(), "yyyy-MM-dd");
				final StockLevelStatus stockLevelStatus = getBlCommerceStockService().getStockLevelStatus(baseStore.getWarehouses(),
						blproductModel.getCode(), startDay, endDay);
				stockData.setStockLevelStatus(stockLevelStatus);
				if (StockLevelStatus.LOWSTOCK.equals(stockLevelStatus))
				{
					stockData.setStockLevel(getBlCommerceStockService().getAvailableCount(blproductModel.getCode(),
							baseStore.getWarehouses(), startDay, endDay));
				}
			}
		}
	}

	protected boolean isStockSystemEnabled()
	{
		return getCommerceStockService().isStockSystemEnabled(getBaseStoreService().getCurrentBaseStore());
	}

	protected boolean isStockSystemEnabled(final BaseStoreModel baseStore)
	{
		return getCommerceStockService().isStockSystemEnabled(baseStore);
	}

	/**
	 * @return the commerceStockService
	 */
	public CommerceStockService getCommerceStockService()
	{
		return commerceStockService;
	}

	/**
	 * @param commerceStockService
	 *           the commerceStockService to set
	 */
	public void setCommerceStockService(final CommerceStockService commerceStockService)
	{
		this.commerceStockService = commerceStockService;
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
	 * @return the baseSiteService
	 */
	public BaseSiteService getBaseSiteService()
	{
		return baseSiteService;
	}

	/**
	 * @param baseSiteService
	 *           the baseSiteService to set
	 */
	public void setBaseSiteService(final BaseSiteService baseSiteService)
	{
		this.baseSiteService = baseSiteService;
	}

	/**
	 * @return the categoryService
	 */
	public CategoryService getCategoryService()
	{
		return categoryService;
	}

	/**
	 * @param categoryService
	 *           the categoryService to set
	 */
	public void setCategoryService(final CategoryService categoryService)
	{
		this.categoryService = categoryService;
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