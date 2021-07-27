/**
 *
 */
package com.bl.facades.subscription;

/**
 * It is responsible for subscribing the emails.
 *
 * @author Sunil Sahu
 */
public interface BlEmailSubscriptionFacade {

	/**
	 * Subscribe the email id.
	 *
	 * @param emailId - email id to be subscribed.
	 */
	public void subscribe(final String emailId);

}
