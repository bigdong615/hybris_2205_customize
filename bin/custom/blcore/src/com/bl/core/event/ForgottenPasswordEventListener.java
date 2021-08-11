/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.core.event;

import de.hybris.platform.acceleratorservices.site.AbstractAcceleratorSiteEventListener;
import de.hybris.platform.basecommerce.model.site.BaseSiteModel;
import de.hybris.platform.commerceservices.enums.SiteChannel;
import de.hybris.platform.commerceservices.event.ForgottenPwdEvent;
import de.hybris.platform.commerceservices.model.process.ForgottenPasswordProcessModel;
import de.hybris.platform.processengine.BusinessProcessService;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.util.ServicesUtil;

import de.hybris.platform.site.BaseSiteService;
import org.springframework.beans.factory.annotation.Required;


/**
 * Listener for "forgotten password" functionality event.
 */
public class ForgottenPasswordEventListener extends AbstractAcceleratorSiteEventListener<ForgottenPwdEvent>
{

	private ModelService modelService;
	private BusinessProcessService businessProcessService;
	private CommonI18NService commonI18NService;
	private BaseSiteService baseSiteService;

	protected BusinessProcessService getBusinessProcessService()
	{
		return businessProcessService;
	}

	@Required  // NOSONAR
	public void setBusinessProcessService(final BusinessProcessService businessProcessService)
	{
		this.businessProcessService = businessProcessService;
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
	@Required  // NOSONAR
	public void setModelService(final ModelService modelService)
	{
		this.modelService = modelService;
	}

	@Override
	protected void onSiteEvent(final ForgottenPwdEvent event)
	{

		final ForgottenPasswordProcessModel forgottenPasswordProcessModel =  getBusinessProcessService()
				.createProcess("forgottenPassword-" + event.getCustomer().getUid() + "-" + System.currentTimeMillis(),
						"forgottenPasswordEmailProcess");
		forgottenPasswordProcessModel.setSite(event.getSite() != null ? event.getSite() : getBaseSiteService().getCurrentBaseSite());
		forgottenPasswordProcessModel.setCustomer(event.getCustomer());
		forgottenPasswordProcessModel.setToken(event.getToken());
		forgottenPasswordProcessModel.setLanguage(event.getLanguage() != null ? event.getLanguage() : getCommonI18NService().getCurrentLanguage());
		forgottenPasswordProcessModel.setCurrency(event.getCurrency());
		forgottenPasswordProcessModel.setStore(event.getBaseStore());
		getModelService().save(forgottenPasswordProcessModel);
		getBusinessProcessService().startProcess(forgottenPasswordProcessModel);
	}

	@Override
	protected SiteChannel getSiteChannelForEvent(final ForgottenPwdEvent event)
	{
		final BaseSiteModel site = event.getSite();
		ServicesUtil.validateParameterNotNullStandardMessage("event.site", site);
		return site.getChannel();
	}
	public CommonI18NService getCommonI18NService() {
		return commonI18NService;
	}

	public void setCommonI18NService(CommonI18NService commonI18NService) {
		this.commonI18NService = commonI18NService;
	}

	public BaseSiteService getBaseSiteService() {
		return baseSiteService;
	}

	public void setBaseSiteService(BaseSiteService baseSiteService) {
		this.baseSiteService = baseSiteService;
	}
}
