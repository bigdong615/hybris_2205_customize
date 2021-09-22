/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.core.event;

import com.bl.core.services.order.BlOrderService;
import de.hybris.platform.acceleratorservices.site.AbstractAcceleratorSiteEventListener;
import de.hybris.platform.basecommerce.model.site.BaseSiteModel;
import de.hybris.platform.commerceservices.enums.SiteChannel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.order.events.SubmitOrderEvent;
import de.hybris.platform.orderprocessing.model.OrderProcessModel;
import de.hybris.platform.processengine.BusinessProcessService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;
import javax.annotation.Resource;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;


/**
 * Listener for order submits.
 */
public class SubmitOrderEventListener extends AbstractAcceleratorSiteEventListener<SubmitOrderEvent>
{
	private static final Logger LOG = Logger.getLogger(SubmitOrderEventListener.class);
	private static final String UNABLE_TO_START_FULFILLMENT = "Unable to start fulfilment process for order [";


	private BusinessProcessService businessProcessService;
	private BaseStoreService baseStoreService;
	private ModelService modelService;

	@Resource(name = "blOrderService")
	BlOrderService blOrderService;
	/**
	 * @return the businessProcessService
	 */
	protected BusinessProcessService getBusinessProcessService()
	{
		return businessProcessService;
	}

	/**
	 * @param businessProcessService
	 *           the businessProcessService to set
	 */
	public void setBusinessProcessService(final BusinessProcessService businessProcessService)
	{
		this.businessProcessService = businessProcessService;
	}

	/**
	 * @return the baseStoreService
	 */
	protected BaseStoreService getBaseStoreService()
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
	 * @return the modelService
	 */
	protected ModelService getModelService()
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

	@Override
	protected void onSiteEvent(final SubmitOrderEvent event)
	{
		final OrderModel order = event.getOrder();
		ServicesUtil.validateParameterNotNullStandardMessage("event.order", order);

		// Try the store set on the Order first, then fallback to the session
		BaseStoreModel store = order.getStore();
		if (store == null)
		{
			store = getBaseStoreService().getCurrentBaseStore();
		}

		if (store == null)
		{
			LOG.warn(UNABLE_TO_START_FULFILLMENT + order.getCode()
					+ "]. Store not set on Order and no current base store defined in session.");
		}
		else
		{
			final String fulfilmentProcessDefinitionName = store.getSubmitOrderProcessCode();

			startOrderProcess(order, store, fulfilmentProcessDefinitionName);
		}
	}

	/**
	 * Create and start the order process
	 * @param order
	 * @param store
	 * @param fulfilmentProcessDefinitionName
	 *
	 */
	private void startOrderProcess(final OrderModel order, final BaseStoreModel store,
			final String fulfilmentProcessDefinitionName) {

		if (fulfilmentProcessDefinitionName == null || fulfilmentProcessDefinitionName.isEmpty())
		{
			LOG.warn(UNABLE_TO_START_FULFILLMENT + order.getCode() + "]. Store [" + store.getUid()
					+ "] has missing SubmitOrderProcessCode");
		}
		else
		{
			if (BooleanUtils.isTrue(order.getInternalTransferOrder())) {
				final String internalTransferOrderProcessCode = store.getInternalTransferProcess();
				if (StringUtils.isBlank(internalTransferOrderProcessCode)) {
					LOG.warn(
							UNABLE_TO_START_FULFILLMENT + order.getCode() + "]. Store ["
									+ store.getUid()
									+ "] has missing internalTransferOrderProcessCode");
				} else {
					createBusinessProcess(order, internalTransferOrderProcessCode);
				}
			} else {
				createBusinessProcess(order, fulfilmentProcessDefinitionName);
			}
		}
	}

	/**
	 * Create and start the order process for specified process definition
	 * @param order
	 * @param fulfilmentProcessDefinitionName
	 *
	 */
	private void createBusinessProcess(final OrderModel order,
			final String fulfilmentProcessDefinitionName) {

		// Creating entry for bundle product.
		if (order.getEntries().stream().anyMatch(entry -> entry.isBundleMainEntry())) {
			blOrderService.createAndSetBundleOrderEntriesInOrder(order);
		}

		final String processCode =
				fulfilmentProcessDefinitionName + "-" + order.getCode() + "-" + System.currentTimeMillis();

		final OrderProcessModel businessProcessModel = getBusinessProcessService()
				.createProcess(processCode, fulfilmentProcessDefinitionName);

				businessProcessModel.setOrder(order);
				getModelService().save(businessProcessModel);
				getBusinessProcessService().startProcess(businessProcessModel);

		if (LOG.isInfoEnabled()) {
					LOG.info(String.format("Started the process %s", processCode));
				}

	}

	@Override
	protected SiteChannel getSiteChannelForEvent(final SubmitOrderEvent event)
	{
		final OrderModel order = event.getOrder();
		ServicesUtil.validateParameterNotNullStandardMessage("event.order", order);
		final BaseSiteModel site = order.getSite();
		ServicesUtil.validateParameterNotNullStandardMessage("event.order.site", site);
		return site.getChannel();
	}
}
