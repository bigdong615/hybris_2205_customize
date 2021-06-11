/**
 *
 */
package com.bl.core.subscription.service.impl;

import de.hybris.platform.servicelayer.model.ModelService;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlStoredEmailSubscriptionModel;
import com.bl.core.subscription.models.ContactRequest;
import com.bl.core.subscription.models.ContactResponse;
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
		final ContactResponse contactResponse = blEmailSubscriptionRestService
				.subscribeEmail(contactRequest);

		if (null != contactResponse && StringUtils
				.equalsIgnoreCase(BlCoreConstants.SUBSCRIPTION_API_OPERATION_STATUS,
						contactResponse.getOperationStatus())) {
			//save the contact in hybris db
			saveContactInHybris(contactResponse);
		}
	}

	/**
	 * Save the contact key to database.
	 * @param contactResponse
	 */
	private void saveContactInHybris(final ContactResponse contactResponse) {

		if (contactResponse.isNewContactKey()) {

			final BlStoredEmailSubscriptionModel emailSubscriptionModel = getModelService()
					.create(BlStoredEmailSubscriptionModel.class);
			emailSubscriptionModel.setContactId(String.valueOf(contactResponse.getContactId()));
			emailSubscriptionModel.setContactKey(contactResponse.getContactKey());

			getModelService().save(emailSubscriptionModel);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
					"Email contact saved for subscription with id [{%s}] and with key [{%s}]",
					contactResponse.getContactId(), contactResponse.getContactKey());
		} else {
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
					"Email contact already exist with key [{%s}]", contactResponse.getContactKey());
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
