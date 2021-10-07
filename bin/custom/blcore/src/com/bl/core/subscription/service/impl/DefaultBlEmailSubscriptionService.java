/**
 *
 */
package com.bl.core.subscription.service.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.subscription.models.ContactResponseWrapper;
import de.hybris.platform.servicelayer.model.ModelService;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.BlStoredEmailSubscriptionModel;
import com.bl.core.subscription.models.ContactRequest;
import com.bl.core.subscription.service.BlEmailSubscriptionRestService;
import com.bl.core.subscription.service.BlEmailSubscriptionService;
import com.bl.logging.BlLogger;


/**
 * Service to subscribe email.
 *
 * @author Sunil Sahu
 */
public class DefaultBlEmailSubscriptionService implements BlEmailSubscriptionService {

	private static final Logger LOG = Logger.getLogger(DefaultBlEmailSubscriptionService.class);
	private ModelService modelService;
	private BlEmailSubscriptionRestService blEmailSubscriptionRestService;

	/**
	 * Subscribe and save the contact.
	 * {@inheritDoc}
	 */
	@Override
	public void subscribe(final ContactRequest contactRequest) {

			//call the actual rest api and collect response
		final ContactResponseWrapper contactResponseWrapper = blEmailSubscriptionRestService
				.subscribeEmail(contactRequest);

				//save the contact in hybris db
				persistContact(contactResponseWrapper);

	}

	/**
	 * Save the contact key to database.
	 * @param contactResponseWrapper
	 */
	private void persistContact(final ContactResponseWrapper contactResponseWrapper) {

		if (null != contactResponseWrapper) {

			final BlStoredEmailSubscriptionModel emailSubscriptionModel = getModelService()
					.create(BlStoredEmailSubscriptionModel.class);
			emailSubscriptionModel.setContactId(null != contactResponseWrapper.getContactID() ? String
					.valueOf(contactResponseWrapper.getContactID()) : BlCoreConstants.EMPTY_STRING);
			emailSubscriptionModel.setContactKey(contactResponseWrapper.getContactKey());
			emailSubscriptionModel.setStatusCode(contactResponseWrapper.getOperationStatus());
			emailSubscriptionModel.setRequestString(contactResponseWrapper.getRequestString());
			emailSubscriptionModel.setResponseString(contactResponseWrapper.getResponseString());

			getModelService().save(emailSubscriptionModel);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
					"Email contact saved for subscription with id {} and with key {}",
					contactResponseWrapper.getContactID(), contactResponseWrapper.getContactKey());
		} else {
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
					"Email contact can not be saved for subscription");
		}
	}

	public ModelService getModelService()
	{
		return modelService;
	}

	public void setModelService(final ModelService modelService)
	{
		this.modelService = modelService;
	}

	/**
	 * @return the blEmailSubscriptionRestService
	 */
	public BlEmailSubscriptionRestService getBlEmailSubscriptionRestService() {
		return blEmailSubscriptionRestService;
	}

	/**
	 * @param blEmailSubscriptionRestService the blEmailSubscriptionRestService to set
	 */
	public void setBlEmailSubscriptionRestService(
			final BlEmailSubscriptionRestService blEmailSubscriptionRestService) {
		this.blEmailSubscriptionRestService = blEmailSubscriptionRestService;
	}

}
