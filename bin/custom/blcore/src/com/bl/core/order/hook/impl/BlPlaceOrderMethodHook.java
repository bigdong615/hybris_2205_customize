package com.bl.core.order.hook.impl;

import de.hybris.platform.commerceservices.order.hook.CommercePlaceOrderMethodHook;
import de.hybris.platform.commerceservices.service.data.CommerceCheckoutParameter;
import de.hybris.platform.commerceservices.service.data.CommerceOrderResult;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.order.InvalidCartException;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.solrfacetsearch.enums.IndexerOperationValues;
import de.hybris.platform.solrfacetsearch.indexer.cron.SolrIndexerHotUpdateJob;
import de.hybris.platform.solrfacetsearch.model.config.SolrFacetSearchConfigModel;
import de.hybris.platform.solrfacetsearch.model.indexer.cron.SolrIndexerHotUpdateCronJobModel;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Objects;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.services.order.BlOrderService;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.core.utils.BlDateTimeUtils;


/**
 * It populates the fields on order as required
 *
 * @author Ravikumar
 *
 */
public class BlPlaceOrderMethodHook implements CommercePlaceOrderMethodHook
{
	private static final Logger LOG = LoggerFactory.getLogger(BlPlaceOrderMethodHook.class);
	private ModelService modelService;
	private BlOrderService blOrderService;
	private BlStockLevelDao blStockLevelDao;
	private SolrIndexerHotUpdateJob solrIndexerHotUpdateJob;

	@Override
	public void afterPlaceOrder(final CommerceCheckoutParameter checkoutParameter, final CommerceOrderResult commerceOrderResult)
			throws InvalidCartException
	{
		final OrderModel order = commerceOrderResult.getOrder();
		if (Objects.nonNull(order))
		{
			getModelService().refresh(order);
			setValuesForRunTAttributes(order);
			setDateOfSaleOnSerialForUsedGearOrder(order);
		}
	}

	/**
	 * Sets the date of sale on serial for used gear order.
	 *
	 * @param order the new date of sale on serial for used gear order
	 */
	private void setDateOfSaleOnSerialForUsedGearOrder(final OrderModel order)
	{
		if(getBlOrderService().isUsedOrderOnly(order) && CollectionUtils.isNotEmpty(order.getEntries())
				&& Objects.nonNull(order.getCreationtime()))
		{
			order.getEntries().forEach(entry -> {
				if(entry.getProduct() instanceof BlSerialProductModel)
				{
					final List<BlProductModel> serials = new ArrayList<BlProductModel>();
					final BlSerialProductModel serial = (BlSerialProductModel) entry.getProduct();
					serial.setDateOfSale(order.getCreationtime());
					serials.add(serial);
					entry.setSerialProducts(serials);
					getModelService().save(entry);
					getModelService().save(serial);

					final Date currentDate = order.getCreationtime();
					final Date futureDate = BlDateTimeUtils.getNextYearsSameDay();
					final Collection<StockLevelModel> stockLevels = getBlStockLevelDao().findSerialStockLevelForDate(serial.getCode(),
							currentDate, futureDate);
					stockLevels.forEach(stockLevel -> {
						stockLevel.setSerialStatus(serial.getSerialStatus());
						stockLevel.setOrder(order.getCode());
						getModelService().save(stockLevel);
					});
					updateIndexForProduct(serial);
				}
			});
		}
	}

	/**
	 * @param serial
	 */
	private void updateIndexForProduct(final BlSerialProductModel serial)
	{
		try
		{
			final SolrIndexerHotUpdateCronJobModel cronJob = new SolrIndexerHotUpdateCronJobModel();
			final List<ItemModel> products = new ArrayList<ItemModel>();
			products.add(serial.getBlProduct());
			cronJob.setItems(products);
			final SolrFacetSearchConfigModel facetConfig = getBlStockLevelDao().getFacetConfigModel();
			cronJob.setFacetSearchConfig(facetConfig);
			cronJob.setIndexerOperation(IndexerOperationValues.UPDATE);
			cronJob.setIndexTypeName("BlProduct");
			getSolrIndexerHotUpdateJob().perform(cronJob);
		}
		catch (final Exception ex)
		{
			LOG.info("Error during running hot update index for product {} and exception is {} ", serial.getBlProduct().getCode(),
					ex);
		}
	}

	/**
	 * Sets the values for run T attributes.
	 *
	 * @param order
	 *           the new values for run T attributes
	 */
	private void setValuesForRunTAttributes(final OrderModel order)
	{
		order.setRunTot_grandTotal(getGrandTotalFromOrder(order));
		order.setRunTot_subtotal(getDefaultValueIfNull(order.getSubtotal()));
		order.setRunTot_totalOptionsCost(getDefaultValueIfNull(order.getTotalOptionsCost()));
		order.setRunTot_totalPrice(getDefaultValueIfNull(order.getTotalPrice()));
		order.setRunTot_totalTax(getDefaultValueIfNull(order.getTotalTax()));
		if(ObjectUtils.allNotNull(order.getRentalStartDate(),order.getRentalEndDate()))
		{
			order.setRunTot_daysRented(
					Long.valueOf(BlDateTimeUtils.getDaysBetweenDates(order.getRentalStartDate(), order.getRentalEndDate())).intValue());
		}
		else
		{
			order.setRunTot_daysRented(Integer.valueOf(0));
		}
		getModelService().save(order);
	}

	/**
	 * Gets the grand total from order.
	 *
	 * @param order
	 *           the order
	 * @return the grand total from order
	 */
	private Double getGrandTotalFromOrder(final OrderModel order)
	{
		if (Objects.isNull(order.getGrandTotal()) || order.getGrandTotal().compareTo(Double.valueOf(0.0d)) <= 0)
		{
			return order.getTotalPrice();
		}
		return order.getGrandTotal();
	}

	/**
	 * Gets the default value if null.
	 *
	 * @param value the value
	 * @return the default value if null
	 */
	private Double getDefaultValueIfNull(final Double value)
	{
		return ObjectUtils.defaultIfNull(value, Double.valueOf(0.0d));
	}

	@Override
	public void beforePlaceOrder(final CommerceCheckoutParameter checkoutParameter) throws InvalidCartException
	{
		// Not implemented

	}

	@Override
	public void beforeSubmitOrder(final CommerceCheckoutParameter checkoutParameter, final CommerceOrderResult commerceOrderResult)
			throws InvalidCartException
	{
		// Not implemented

	}

	/**
	 * @return the modelService
	 */
	public ModelService getModelService()
	{
		return modelService;
	}

	/**
	 * @param modelService
	 *           the modelService to set
	 */
	public void setModelService(final ModelService modelService)
	{
		this.modelService = modelService;
	}

	/**
	 * @return the blOrderService
	 */
	public BlOrderService getBlOrderService()
	{
		return blOrderService;
	}

	/**
	 * @param blOrderService the blOrderService to set
	 */
	public void setBlOrderService(final BlOrderService blOrderService)
	{
		this.blOrderService = blOrderService;
	}

	public BlStockLevelDao getBlStockLevelDao()
	{
		return blStockLevelDao;
	}

	/**
	 * @param blStockLevelDao
	 *           the blStockLevelDao to set
	 */
	public void setBlStockLevelDao(final BlStockLevelDao blStockLevelDao)
	{
		this.blStockLevelDao = blStockLevelDao;
	}

	public SolrIndexerHotUpdateJob getSolrIndexerHotUpdateJob()
	{
		return solrIndexerHotUpdateJob;
	}

	public void setSolrIndexerHotUpdateJob(final SolrIndexerHotUpdateJob solrIndexerHotUpdateJob)
	{
		this.solrIndexerHotUpdateJob = solrIndexerHotUpdateJob;
	}

}
