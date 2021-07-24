/**
 *
 */
package com.bl.facades.subscription.impl;

import com.bl.core.subscription.models.ContactRequest;
import com.bl.core.subscription.service.BlEmailSubscriptionService;
import com.bl.facades.populators.BlEmailSubscriptionRequestPopulator;
import com.bl.facades.subscription.BlEmailSubscriptionFacade;


/**
 * Default implementation of {@link BlEmailSubscriptionFacade}. Subscribe the emails.
 *
 * @author Sunil Sahu
 */
public class DefaultBlEmailSubscriptionFacade implements BlEmailSubscriptionFacade {

	private BlEmailSubscriptionService blEmailSubscriptionService;
	private BlEmailSubscriptionRequestPopulator blEmailSubscriptionRequestPopulator;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void subscribe(final String emailId) {

		final ContactRequest contactRequest = new ContactRequest();
		blEmailSubscriptionRequestPopulator.populate(emailId, contactRequest);
		// call API to create contact
		blEmailSubscriptionService.subscribe(contactRequest);
	}


	/**
	 * @return the blEmailSubscriptionService
	 */
	public BlEmailSubscriptionService getBlEmailSubscriptionService() {
		return blEmailSubscriptionService;
	}

	/**
	 * @param blEmailSubscriptionService the blEmailSubscriptionService to set
	 */
	public void setBlEmailSubscriptionService(
			final BlEmailSubscriptionService blEmailSubscriptionService) {
		this.blEmailSubscriptionService = blEmailSubscriptionService;
	}

	/**
	 * @return the blEmailSubscriptionPopulator
	 */
	public BlEmailSubscriptionRequestPopulator getBlEmailSubscriptionRequestPopulator() {
		return blEmailSubscriptionRequestPopulator;
	}

	/**
	 * @param blEmailSubscriptionRequestPopulator the blEmailSubscriptionPopulator to set
	 */
	public void setBlEmailSubscriptionRequestPopulator(
			final BlEmailSubscriptionRequestPopulator blEmailSubscriptionRequestPopulator) {
		this.blEmailSubscriptionRequestPopulator = blEmailSubscriptionRequestPopulator;
	}
}
