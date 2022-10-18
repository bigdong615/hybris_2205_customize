package com.bl.core.model.handler;

import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.servicelayer.model.attribute.DynamicAttributeHandler;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Objects;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.logging.BlLogger;
import com.google.common.util.concurrent.AtomicDouble;


/**
 * @author Ravikumar
 *
 */
public class BLSumOfGearValueOnOrderHandler implements DynamicAttributeHandler<Double, AbstractOrderModel>
{
	private static final Logger LOG = Logger.getLogger(BLSumOfGearValueOnOrderHandler.class);

	@Override
	public Double get(final AbstractOrderModel abstractOrder)
	{
		final AtomicDouble sumOfGearValueOnOrder = new AtomicDouble(0.0d);
		try
		{
			if (Objects.isNull(abstractOrder))
			{
				BlLogger.logMessage(LOG, Level.ERROR,
						"Cannot evaluate the value for AbstractOrderModel.sumOfGearValueOnOrder because AbstractOrderModel is null");
				return sumOfGearValueOnOrder.get();
			}
			if (CollectionUtils.isEmpty(abstractOrder.getEntries()))
			{
				BlLogger.logFormatMessageInfo(LOG, Level.INFO,
						"Cannot evaluate the value for AbstractOrderModel.sumOfGearValueOnOrder because no Entries found on Order : {}",
						abstractOrder.getCode());
				return sumOfGearValueOnOrder.get();
			}
			abstractOrder.getEntries().forEach(entry -> sumOfGearValueOnOrder
					.addAndGet(getTwoDecimalDoubleValue(getRetailPriceOnProduct(entry, entry.getProduct()))));
		}
		catch (final Exception exception)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception,
					"Cannot evaluate the value for AbstractOrderModel.sumOfGearValueOnOrder due to error occured for Order : {}",
					Objects.isNull(abstractOrder) ? "NULL" : abstractOrder.getCode());
		}
		return sumOfGearValueOnOrder.get();
	}

	/**
	 * Gets the retail price on SKU.
	 *
	 * @param entry
	 *           the entry
	 * @param product
	 *           the product
	 * @return the retail price on product
	 */
	private Double getRetailPriceOnProduct(final AbstractOrderEntryModel entry, final ProductModel product)
	{
		if (Objects.isNull(product))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, "No Product found on Order : {} with Entry Number : {} and Entry PK : {}",
					entry.getOrder().getCode(), entry.getEntryNumber().intValue(), entry.getPk().toString());
			return Double.valueOf(0.0d);
		}
		final BlProductModel sku = product instanceof BlSerialProductModel
				? getSkuFromSerial(entry, ((BlSerialProductModel) product))
				: ((BlProductModel) product);
		if (Objects.isNull(sku.getRetailPrice()) || sku.getRetailPrice().compareTo(Double.valueOf(0.0d)) <= 0)
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"No Retail Price found on Product with code: {} on Order : {} with Entry Number : {} and Entry PK : {}",
					sku.getCode(), entry.getOrder().getCode(), entry.getEntryNumber().intValue(), entry.getPk().toString());
			return Double.valueOf(0.0d);
		}
		return sku.getRetailPrice() * entry.getQuantity().intValue();
	}

	/**
	 * Gets the sku from given serial.
	 *
	 * @param entry
	 *           the entry
	 * @param serialProduct
	 *           the serial product
	 * @return the sku from serial
	 */
	private BlProductModel getSkuFromSerial(final AbstractOrderEntryModel entry, final BlSerialProductModel serialProduct)
	{
		if (Objects.isNull(serialProduct.getBlProduct()))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"No SKU found on Serial with code: {} on Order : {} with Entry Number : {} and Entry PK : {}",
					serialProduct.getCode(), entry.getOrder().getCode(), entry.getEntryNumber().intValue(), entry.getPk().toString());
			return serialProduct;
		}
		return (serialProduct.getBlProduct());
	}

	/**
	 * Gets the two decimal double value of the retail price.
	 *
	 * @param amount
	 *           the amount
	 * @return the two decimal double value
	 */
	private Double getTwoDecimalDoubleValue(final Double amount)
	{
		return amount.compareTo(0.0d) <= 0 ? Double.valueOf(0.0d)
				: BigDecimal.valueOf(amount).setScale(2, RoundingMode.HALF_EVEN).doubleValue();
	}

	@Override
	public void set(final AbstractOrderModel abstractOrder, final Double sumOfGearValueOnOrder)
	{
		BlLogger.logMessage(LOG, Level.ERROR, "Setter for attribute AbstractOrderModel.sumOfGearValueOnOrder is not supported");
		throw new UnsupportedOperationException();
	}

}
