/**
 *
 */
package com.bl.core.subscription.service;

import com.bl.core.subscription.models.ContactRequest;


/**
 * Service to subscribe email.
 *
 * @author Sunil Sahu
 */
public interface BlEmailSubscriptionService {

	/**
	 * Subscribe the email contact.
	 *
	 * @param contactRequest - contactRequest containing the email id to be subscribed.
	 */
	public void subscribe(final ContactRequest contactRequest);

}
