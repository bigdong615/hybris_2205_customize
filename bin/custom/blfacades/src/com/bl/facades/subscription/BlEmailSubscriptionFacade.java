/**
 *
 */
package com.bl.facades.subscription;

import com.bl.esp.dto.email.marketing.data.CustomerMarketingData;

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
	public void subscribe(final String orderCode,final CustomerMarketingData customerMarketingData);

}
