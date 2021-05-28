package com.bl.core.model.interceptor;

import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import de.hybris.platform.servicelayer.model.ItemModelContextImpl;

import java.math.BigDecimal;
import java.util.Objects;

import org.apache.commons.lang.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.jalo.BlSerialProduct;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.services.calculation.BlPricingService;
import com.bl.core.stock.BlStockService;
import com.bl.logging.BlLogger;


/**
 * The Class BlSerialProductPrepareInterceptor used to intercept the model and modify the attributes before saving the data.
 *
 * @author Ritika
 */
public class BlSerialProductPrepareInterceptor implements PrepareInterceptor<BlSerialProductModel>
{
	/** The bl pricing service. */
	private BlPricingService blPricingService;
	private BlStockService blStockService;

	private static final Logger LOG = Logger.getLogger(BlSerialProductPrepareInterceptor.class);

	/**
	 * On prepare.
	 *
	 * @param blSerialProduct
	 *           the bl serial product
	 * @param ctx
	 *           the ctx
	 * @throws InterceptorException
	 *            the interceptor exception
	 */
	@Override
	public void onPrepare(final BlSerialProductModel blSerialProduct, final InterceptorContext ctx) throws InterceptorException
	{
		//Intercepting forSaleBasePrice and conditionRatingOverallScore attribute to create finalSalePrice for serial
		calculateFinalSalePriceForSerial(blSerialProduct, ctx);
		//Intercepting finalSalePrice and forSaleDiscount attribute to create incentivizedPrice for serial
		calculateIncentivizedPriceForSerial(blSerialProduct, ctx);
		//updateStockRecordsOnSerialStatusUpdate(blSerialProduct, ctx);
		//updateStockRecordsOnForRentFlagUpdate(blSerialProduct, ctx);
		//updateWarehouseInStockRecordsOnWHLocUpdate(blSerialProduct, ctx);
	}

	/**
	 * It updates the warehouse in stock records when warehouse location is changed of a serial product
	 *
	 * @param blSerialProduct
	 *           the bl serial product
	 * @param ctx
	 *           the ctx
	 */
	private void updateWarehouseInStockRecordsOnWHLocUpdate(final BlSerialProductModel blSerialProduct,
			final InterceptorContext ctx)
	{
		try {
			final Object initialValue = getInitialValue(blSerialProduct, BlSerialProduct.WAREHOUSELOCATION);
			if (null != initialValue && ctx.isModified(blSerialProduct, BlSerialProductModel.WAREHOUSELOCATION) &&
					blSerialProduct.getWarehouseLocation() != null &&
					blSerialProduct.getSerialStatus().equals(SerialStatusEnum.ACTIVE)) {
					getBlStockService().findAndUpdateWarehouseInStockRecords(blSerialProduct);
			}
		} catch(final Exception ex) {
			BlLogger.logFormattedMessage(LOG, Level.ERROR, BlCoreConstants.EMPTY_STRING, ex,
					"Exception occurred while updating the warehouse {} in the stock record for serial product {} ",
					blSerialProduct.getWarehouseLocation(), blSerialProduct.getCode());
		}
	}

	/**
	 * It deletes the date specific stock records when 'forRent' flag  of a serial product is marked as false
	 * and creates one stock record where date is null
	 *
	 * @param blSerialProduct
	 *           the bl serial product
	 * @param ctx
	 *           the ctx
	 */
	private void updateStockRecordsOnForRentFlagUpdate(final BlSerialProductModel blSerialProduct, final InterceptorContext ctx) {
		try {
			final Object initialValue = getInitialValue(blSerialProduct, BlSerialProduct.FORRENT);
			if (null != initialValue && ctx.isModified(blSerialProduct, BlSerialProductModel.FORRENT) && //NOSONAR
			SerialStatusEnum.ACTIVE.equals(blSerialProduct.getSerialStatus()) && Boolean.TRUE.equals(blSerialProduct
						.getForSale()) && Boolean.FALSE.equals(blSerialProduct.getForRent())) {
					getBlStockService().findAndDeleteStockRecords(blSerialProduct);
				}
		} catch(final Exception ex) {
			BlLogger.logFormattedMessage(LOG, Level.ERROR, BlCoreConstants.EMPTY_STRING, ex,
					"Exception occurred while updating the stock records on 'For Rent' flag change event of serial product {} ",
					blSerialProduct.getCode());
		}
	}

	/**
	 * It updates the stock records when serial status of a serial product is changed
	 *
	 * @param blSerialProduct
	 *           the bl serial product
	 * @param ctx
	 *           the ctx
	 */
	private void updateStockRecordsOnSerialStatusUpdate(final BlSerialProductModel blSerialProduct, final InterceptorContext ctx)
	{
		try {
			final Object initialValue = getInitialValue(blSerialProduct, BlSerialProduct.SERIALSTATUS);
			if (null != initialValue && ctx.isModified(blSerialProduct, BlSerialProductModel.SERIALSTATUS)) {
				if (SerialStatusEnum.ACTIVE.equals(blSerialProduct.getSerialStatus())) {
					updateStockRecordsAsAvailable(blSerialProduct, initialValue);
				} else if(SerialStatusEnum.ACTIVE.equals(initialValue)){
					getBlStockService().findAndUpdateStockRecords(blSerialProduct, true);
				}
			}
		} catch(final Exception ex) {
			BlLogger.logFormattedMessage(LOG, Level.ERROR, BlCoreConstants.EMPTY_STRING, ex,
					"Exception occurred while updating the stock records on serial status change event of serial product {} ",
					blSerialProduct.getCode());
		}
	}

	/**
	 * It updates the stock records as reserved status is false
	 *
	 * @param blSerialProduct
	 *           the bl serial product
	 * @param initialValue
	 */
	private void updateStockRecordsAsAvailable(final BlSerialProductModel blSerialProduct, final Object initialValue) {
		if (initialValue.equals(SerialStatusEnum.COMING_FROM_PURCHASE) && null != blSerialProduct.getWarehouseLocation()
				&& Boolean.TRUE.equals(blSerialProduct.getForRent()))
		{
			getBlStockService().createStockRecordsForNewSerialProducts(blSerialProduct);
		}
		else
		{
			getBlStockService().findAndUpdateStockRecords(blSerialProduct, false);
		}
	}

	/**
	 * It gets the initial value of the attribute before update
	 *
	 * @param blSerialProduct
	 *           the bl serial product
	 */
	private Object getInitialValue(final BlSerialProductModel blSerialProduct, final String status) {
		final ItemModelContextImpl itemModelCtx = (ItemModelContextImpl) blSerialProduct
				.getItemModelContext();
		return itemModelCtx.isLoaded(status) ? itemModelCtx
				.getOriginalValue(status) : itemModelCtx.loadOriginalValue(status);
	}

	/**
	 * Calculate final sale price for serial.
	 *
	 * @param blSerialProduct
	 *           the bl serial product
	 * @param ctx
	 *           the ctx
	 */
	private void calculateFinalSalePriceForSerial(final BlSerialProductModel blSerialProduct, final InterceptorContext ctx)
	{
		if (BooleanUtils.isTrue(blSerialProduct.getForSale()) && hasForSaleBaseAndConditionalRating(blSerialProduct)
				&& isForSalePriceCalculationRequired(blSerialProduct, ctx))
		{
			blSerialProduct.setFinalSalePrice(getBlPricingService().calculateFinalSalePriceForSerial(
					blSerialProduct.getForSaleBasePrice(), blSerialProduct.getConditionRatingOverallScore()));
		}
	}

	/**
	 * Check if the sale price is not null and greater than zero and conditional rating is also not zero.
	 *
	 * @param blSerialProduct
	 *           the bl serial product
	 * @return true, if successful
	 */
	private boolean hasForSaleBaseAndConditionalRating(final BlSerialProductModel blSerialProduct)
	{
		final Double conditionRatingOverallScore = blSerialProduct.getConditionRatingOverallScore();
		final BigDecimal forSaleBasePrice = blSerialProduct.getForSaleBasePrice();

		return Objects.nonNull(forSaleBasePrice) && Objects.nonNull(conditionRatingOverallScore)
				&& forSaleBasePrice.compareTo(BigDecimal.ZERO) > 0 && conditionRatingOverallScore > 0.0D;
	}

	/**
	 * Checks if For sale price calculation required.
	 *
	 * @param blSerialProduct
	 *           the bl serial product
	 * @param ctx
	 *           the ctx
	 * @return true, if is for sale price calculation required
	 */
	private boolean isForSalePriceCalculationRequired(final BlSerialProductModel blSerialProduct, final InterceptorContext ctx)
	{
		return ctx.isNew(blSerialProduct) || Objects.isNull(blSerialProduct.getFinalSalePrice())
				|| ctx.isModified(blSerialProduct, BlSerialProductModel.FORSALEBASEPRICE)
				|| ctx.isModified(blSerialProduct, BlSerialProductModel.CONDITIONRATINGOVERALLSCORE);
	}

	/**
	 * Calculate incentivized price based on the finalSalePrice.
	 *
	 * @param blSerialProduct
	 *           the bl serial product
	 * @param ctx
	 *           the ctx
	 */
	private void calculateIncentivizedPriceForSerial(final BlSerialProductModel blSerialProduct, final InterceptorContext ctx)
	{
		if (Objects.nonNull(blSerialProduct.getFinalSalePrice()) && Objects.nonNull(blSerialProduct.getForSaleDiscount()))
		{
			final BigDecimal finalSalePrice = blSerialProduct.getFinalSalePrice().setScale(BlCoreConstants.DECIMAL_PRECISION,
					BlCoreConstants.ROUNDING_MODE);
			final Integer forSaleDiscount = blSerialProduct.getForSaleDiscount();
			if (finalSalePrice.compareTo(BigDecimal.ZERO) > 0 && forSaleDiscount > 0
					&& isIncentivizedCalculationRequired(blSerialProduct, ctx))
			{
				final BigDecimal calculatedIncentivizedPrice = finalSalePrice.subtract(
						finalSalePrice.multiply(BigDecimal.valueOf(forSaleDiscount)).divide(BigDecimal.valueOf(BlCoreConstants.DIVIDE_BY_HUNDRED))
								.setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE));
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
						"Calculated Incentivized Price is {} for Serial Product {} with For Sale Discount {} and For Sale Final Price {}",
						calculatedIncentivizedPrice, blSerialProduct.getProductId(), forSaleDiscount.intValue(), finalSalePrice.doubleValue());
				blSerialProduct.setIncentivizedPrice(calculatedIncentivizedPrice);
			}
		}
	}

	/**
	 * Checks if Incentivized price calculation required.
	 *
	 * @param blSerialProduct
	 *           the bl serial product
	 * @param ctx
	 *           the ctx
	 * @return true, if is incentivized calculation required
	 */
	private boolean isIncentivizedCalculationRequired(final BlSerialProductModel blSerialProduct, final InterceptorContext ctx)
	{
		return Objects.isNull(blSerialProduct.getIncentivizedPrice())
				|| blSerialProduct.getIncentivizedPrice().compareTo(BigDecimal.ZERO) == 0
				|| ctx.isModified(blSerialProduct, BlSerialProductModel.FINALSALEPRICE)
				|| ctx.isModified(blSerialProduct, BlSerialProductModel.FORSALEDISCOUNT);
	}

	/**
	 * Gets the bl pricing service.
	 *
	 * @return the bl pricing service
	 */
	public BlPricingService getBlPricingService()
	{
		return blPricingService;
	}

	/**
	 * Sets the bl pricing service.
	 *
	 * @param blPricingService
	 *           the new bl pricing service
	 */
	public void setBlPricingService(final BlPricingService blPricingService)
	{
		this.blPricingService = blPricingService;
	}

	/**
	 * @return the blStockService
	 */
	public BlStockService getBlStockService()
	{
		return blStockService;
	}

	/**
	 * @param blStockService
	 *           the blStockService to set
	 */
	public void setBlStockService(final BlStockService blStockService)
	{
		this.blStockService = blStockService;
	}

}
