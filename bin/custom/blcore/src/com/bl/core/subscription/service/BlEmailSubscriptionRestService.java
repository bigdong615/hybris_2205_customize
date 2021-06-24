/**
 *
 */
package com.bl.core.subscription.service;

import com.bl.core.subscription.models.ContactRequest;
import com.bl.core.subscription.models.ContactResponse;


/**
 * Service to call REST APIs for email subscription.
 *
 * @author Sunil Sahu
 */
public interface BlEmailSubscriptionRestService {

	/**
	 * Subscribe the emailId by calling REST APIs.
	 *
	 * @param contactRequest - Request containing email id to be subscribed.
	 * @return contactResponse - ContactResponse from API call.
	 */
	ContactResponse subscribeEmail(final ContactRequest contactRequest);
}
