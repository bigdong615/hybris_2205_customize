/*
 * [y] hybris Platform
 *
 * Copyright (c) 2018 SAP SE or an SAP affiliate company.
 * All rights reserved.
 *
 * This software is the confidential and proprietary information of SAP
 * ("Confidential Information"). You shall not disclose such Confidential
 * Information and shall use it only in accordance with the terms of the
 * license agreement you entered into with SAP.
 *
 */

package com.bl.Ordermanagement.actions.returns;

import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.processengine.action.AbstractProceduralAction;
import de.hybris.platform.returns.model.ReturnProcessModel;
import de.hybris.platform.returns.model.ReturnRequestModel;
import de.hybris.platform.servicelayer.time.TimeService;
import de.hybris.platform.solrfacetsearch.enums.IndexerOperationValues;
import de.hybris.platform.solrfacetsearch.indexer.cron.SolrIndexerHotUpdateJob;
import de.hybris.platform.solrfacetsearch.model.config.SolrFacetSearchConfigModel;
import de.hybris.platform.solrfacetsearch.model.indexer.cron.SolrIndexerHotUpdateCronJobModel;
import de.hybris.platform.task.RetryLaterException;
import de.hybris.platform.warehousing.model.RestockConfigModel;
import de.hybris.platform.warehousing.returns.RestockException;
import de.hybris.platform.warehousing.returns.service.RestockConfigService;
import de.hybris.platform.warehousing.returns.strategy.RestockWarehouseSelectionStrategy;
import de.hybris.platform.warehousing.stock.services.WarehouseStockService;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import org.apache.commons.lang.BooleanUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Required;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.stock.BlStockLevelDao;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;
import static org.apache.commons.collections4.CollectionUtils.isEmpty;


/**
 * Update inventory and set the {@link ReturnRequestModel} status to COMPLETED.<br/>
 * A custom update inventory behavior must be implemented. This determines the steps to be executed after a successful
 * return.
 */
public class InventoryUpdateAction extends AbstractProceduralAction<ReturnProcessModel>
{
	private static final Logger LOG = LoggerFactory.getLogger(InventoryUpdateAction.class);
	private RestockConfigService restockConfigService;
	private TimeService timeService;
	private RestockWarehouseSelectionStrategy restockWarehouseSelectionStrategy;
	private WarehouseStockService warehouseStockService;
	private BlStockLevelDao blStockLevelDao;
	private SolrIndexerHotUpdateJob solrIndexerHotUpdateJob;

	@Override
	public void executeAction(final ReturnProcessModel process) throws RetryLaterException, Exception
	{
		LOG.debug("Process: {} in step {}", process.getCode(), getClass().getSimpleName());

		final ReturnRequestModel returnRequest = process.getReturnRequest();
		final RestockConfigModel restockConfig = getRestockConfigService().getRestockConfig();
		if (restockConfig != null && Boolean.TRUE.equals(restockConfig.getIsUpdateStockAfterReturn()))
		{
			validateParameterNotNullStandardMessage("returnedBinCode", restockConfig.getReturnedBinCode());
			final WarehouseModel returnWarehouse = getReturnWarehouse(returnRequest);
			if (returnWarehouse == null)
			{
				LOG.info("Cannot find any warehouse accept returned item(s), please update returned stock(s) manually");
			}
			else
			{
				if (isEmpty(returnRequest.getReturnEntries()))
				{
					throw new IllegalArgumentException(
							String.format("No return entries found for the ReturnRequest: [%s]", returnRequest.getRMA()));
				}
				returnRequest.getReturnEntries().forEach(returnEntry -> {
					try
					{
						getWarehouseStockService().createStockLevel(returnEntry.getOrderEntry().getProduct().getCode(), returnWarehouse,
								returnEntry.getReceivedQuantity().intValue(), null, getCurrentDateWithDelayDaysBeforeRestock(),
								getRestockConfigService().getRestockConfig().getReturnedBinCode());
					}
					catch (final RestockException e) //NOSONAR
					{
						LOG.error("More than one record found for restockConfig");
					}
				});
			}
		}
		updateIndexForProduct(returnRequest);
	}

	/**
	 * @param returnRequest
	 */
	private void updateIndexForProduct(final ReturnRequestModel returnRequest)
	{
		OrderModel order = returnRequest.getOrder();
		if(BooleanUtils.isFalse(order.getIsRentalOrder()) && BooleanUtils.isFalse(order.isGiftCardOrder()) 
		&& BooleanUtils.isFalse(order.getIsRetailGearOrder()) && BooleanUtils.isFalse(order.getIsReplacementOrder())) {
			order.getEntries().forEach(entry -> {
				if(entry.getProduct() instanceof BlSerialProductModel)
				{
					try
					{
						final SolrIndexerHotUpdateCronJobModel cronJob = new SolrIndexerHotUpdateCronJobModel();
						final List<ItemModel> products = new ArrayList<ItemModel>();
						products.add(entry.getProduct());
						cronJob.setItems(products);
						final SolrFacetSearchConfigModel facetConfig = getBlStockLevelDao().getFacetConfigModel();
						cronJob.setFacetSearchConfig(facetConfig);
						cronJob.setIndexerOperation(IndexerOperationValues.UPDATE);
						cronJob.setIndexTypeName("BlProduct");
						getSolrIndexerHotUpdateJob().perform(cronJob);
					}
					catch (final Exception ex)
					{
						LOG.info("Error during running hot update index for product {} and exception is {} ", ((BlSerialProductModel)entry.getProduct()).getBlProduct().getCode(),
								ex);
					}
				}
			});
		}
	}

	/**
	 * Finds the {@link WarehouseModel}, which can accept the returned good(s)
	 *
	 * @param returnRequest
	 * 		the {@link ReturnRequestModel} for which goods need to be put back in stock
	 * @return the {@link WarehouseModel} which can accept the returned good(s) from the given {@link ReturnRequestModel}.
	 */
	protected WarehouseModel getReturnWarehouse(final ReturnRequestModel returnRequest)
	{
		WarehouseModel returnWarehouse = returnRequest.getReturnWarehouse();
		if (returnWarehouse == null)
		{
			LOG.info(
					"No return warehouse set for the Return Request: [{}], applying RestockWarehouseSelectionStrategy to find the warehouse",
					returnRequest.getRMA());
			returnWarehouse = getRestockWarehouseSelectionStrategy().performStrategy(returnRequest);
		}
		return returnWarehouse;
	}

	/**
	 * Calculates the current date -  # of delay days before restock according to a property.
	 */
	protected Date getCurrentDateWithDelayDaysBeforeRestock() throws RestockException
	{
		final Calendar cal = Calendar.getInstance();
		cal.setTime(getTimeService().getCurrentTime());

		if (getRestockConfigService().getRestockConfig() != null)
		{
			final int delayDays = getRestockConfigService().getRestockConfig().getDelayDaysBeforeRestock();

			cal.add(Calendar.DATE, delayDays);
		}
		return cal.getTime();
	}

	protected RestockConfigService getRestockConfigService()
	{
		return restockConfigService;
	}

	@Required
	public void setRestockConfigService(final RestockConfigService restockConfigService)
	{
		this.restockConfigService = restockConfigService;
	}

	protected RestockWarehouseSelectionStrategy getRestockWarehouseSelectionStrategy()
	{
		return restockWarehouseSelectionStrategy;
	}

	@Required
	public void setRestockWarehouseSelectionStrategy(final RestockWarehouseSelectionStrategy restockWarehouseSelectionStrategy)
	{
		this.restockWarehouseSelectionStrategy = restockWarehouseSelectionStrategy;
	}

	protected TimeService getTimeService()
	{
		return timeService;
	}

	@Required
	public void setTimeService(final TimeService timeService)
	{
		this.timeService = timeService;
	}

	protected WarehouseStockService getWarehouseStockService()
	{
		return warehouseStockService;
	}

	@Required
	public void setWarehouseStockService(WarehouseStockService warehouseStockService)
	{
		this.warehouseStockService = warehouseStockService;
	}
	
	public BlStockLevelDao getBlStockLevelDao()
	{
		return blStockLevelDao;
	}

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
