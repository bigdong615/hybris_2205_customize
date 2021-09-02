package com.bl.facades.populators;

import com.bl.core.model.BlProductModel;
import de.hybris.platform.basecommerce.enums.StockLevelStatus;
import de.hybris.platform.commercefacades.product.data.StockData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;

import java.util.Date;

import com.bl.core.data.StockResult;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.constants.BlFacadesConstants;
import com.bl.facades.product.data.RentalDateDto;


/**
 * This class is used to populate the stock data
 *
 * @author Moumita
 */
public class BlStockPopulator<SOURCE extends BlProductModel, TARGET extends StockData> implements Populator<SOURCE, TARGET>
{
	private BaseStoreService baseStoreService;
	private BlCommerceStockService blCommerceStockService;
	private BlDatePickerService blDatePickerService;

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
		final RentalDateDto rentalDateDto = blDatePickerService.getRentalDatesFromSession();
		if (null != rentalDateDto)
		{
			final String startDate = rentalDateDto.getSelectedFromDate();
			final String endDate = rentalDateDto.getSelectedToDate();
			final Date startDay = BlDateTimeUtils
					.convertStringDateToDate(startDate, BlFacadesConstants.DATE_FORMAT);
			final Date endDay = BlDateTimeUtils
					.convertStringDateToDate(endDate, BlFacadesConstants.DATE_FORMAT);
      final StockResult stockResult;
     if(blProductModel.isBundleProduct()){
       stockResult = getBlCommerceStockService().getStockForBundleProduct(
           blProductModel, baseStore.getWarehouses(), startDay, endDay);
     }else{
       stockResult = getBlCommerceStockService().getStockForEntireDuration(
           blProductModel.getCode(), baseStore.getWarehouses(), startDay, endDay);
     }
			final StockLevelStatus stockLevelStatus = stockResult.getStockLevelStatus();
			stockData.setStockLevelStatus(stockLevelStatus);
			stockData.setStockLevel(stockResult.getAvailableCount());
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
	 * @return the blDatePickerService
	 */
	public BlDatePickerService getBlDatePickerService()
	{
		return blDatePickerService;
	}

	/**
	 * @param blDatePickerService
	 *           the blDatePickerService to set
	 */
	public void setBlDatePickerService(final BlDatePickerService blDatePickerService)
	{
		this.blDatePickerService = blDatePickerService;
	}

}