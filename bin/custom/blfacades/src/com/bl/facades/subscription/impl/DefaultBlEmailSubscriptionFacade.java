/**
 *
 */
package com.bl.facades.subscription.impl;

import com.bl.core.subscription.models.ContactRequest;
import com.bl.core.subscription.service.BlEmailSubscriptionService;
import com.bl.facades.populators.BlEmailSubscriptionPopulator;
import com.bl.facades.subscription.BlEmailSubscriptionFacade;


/**
 * Default implementation of {@link BlEmailSubscriptionFacade}. Subscribe the emails.
 *
 * @author Sunil Sahu
 */
public class DefaultBlEmailSubscriptionFacade implements BlEmailSubscriptionFacade
{
	private BlEmailSubscriptionService blEmailSubscriptionService;
	private BlEmailSubscriptionPopulator blEmailSubscriptionPopulator;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void subscribe(final String emailId)
	{
		final ContactRequest contactRequest = new ContactRequest();
		blEmailSubscriptionPopulator.poupulateContactRequest(emailId, contactRequest);
		// call API to create contact
		blEmailSubscriptionService.subscribe(contactRequest);
	}


	/**
	 * @return the blEmailSubscriptionService
	 */
	public BlEmailSubscriptionService getBlEmailSubscriptionService()
	{
		return blEmailSubscriptionService;
	}

	/**
	 * @param blEmailSubscriptionService
	 *           the blEmailSubscriptionService to set
	 */
	public void setBlEmailSubscriptionService(final BlEmailSubscriptionService blEmailSubscriptionService)
	{
		this.blEmailSubscriptionService = blEmailSubscriptionService;
	}

	/**
	 * @return the blEmailSubscriptionPopulator
	 */
	public BlEmailSubscriptionPopulator getBlEmailSubscriptionPopulator()
	{
		return blEmailSubscriptionPopulator;
	}

	/**
	 * @param blEmailSubscriptionPopulator
	 *           the blEmailSubscriptionPopulator to set
	 */
	public void setBlEmailSubscriptionPopulator(final BlEmailSubscriptionPopulator blEmailSubscriptionPopulator)
	{
		this.blEmailSubscriptionPopulator = blEmailSubscriptionPopulator;
	}

}
