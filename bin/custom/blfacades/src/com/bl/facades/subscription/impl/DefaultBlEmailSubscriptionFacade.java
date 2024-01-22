/**
 *
 */
package com.bl.facades.subscription.impl;

import com.bl.core.esp.service.BlESPEventService;
import com.bl.core.subscription.models.ContactRequest;
import com.bl.core.subscription.service.BlEmailSubscriptionService;
import com.bl.esp.dto.email.marketing.data.CustomerMarketingData;
import com.bl.facades.populators.BlEmailSubscriptionRequestPopulator;
import com.bl.facades.subscription.BlEmailSubscriptionFacade;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.user.UserService;
import org.apache.commons.lang.StringUtils;

import javax.annotation.Resource;


/**
 * Default implementation of {@link BlEmailSubscriptionFacade}. Subscribe the emails.
 *
 * @author Sunil Sahu
 */
public class DefaultBlEmailSubscriptionFacade implements BlEmailSubscriptionFacade {

	private BlEmailSubscriptionService blEmailSubscriptionService;
	private BlEmailSubscriptionRequestPopulator blEmailSubscriptionRequestPopulator;
	@Resource(name="blEspEventService")
	private BlESPEventService blESPEventService;
	@Resource(name = "userService")
	private UserService userService;
	/**
	 * {@inheritDoc}
	 */
	@Override
	public void subscribe(final String orderCode,final CustomerMarketingData customerMarketingData) {
		if (StringUtils.isNotEmpty(customerMarketingData.getEmailId())) {
		final ContactRequest contactRequest = new ContactRequest();
		blEmailSubscriptionRequestPopulator.populate(customerMarketingData.getEmailId(), contactRequest);
		if(orderCode==null) {
			final UserModel currentUser = userService.getCurrentUser();
			if (userService.isAnonymousUser(currentUser)) {
				customerMarketingData.setSubscriberID(customerMarketingData.getEmailId());
				customerMarketingData.setFirstName(StringUtils.EMPTY);
                customerMarketingData.setCustomer(Boolean.FALSE);
			} else {
				customerMarketingData.setSubscriberID(currentUser.getUid());
				customerMarketingData.setFirstName(currentUser.getName());
				customerMarketingData.setCustomer(Boolean.TRUE);
			}
			customerMarketingData.setZipcode(StringUtils.EMPTY);
			customerMarketingData.setState(StringUtils.EMPTY);
			blESPEventService.sendCustomerMarketingEvent(customerMarketingData.getEmailId(),customerMarketingData);
		}else {
            blESPEventService.sendCustomerMarketingEvent(orderCode,customerMarketingData);
			}

		// call API to create contact
		blEmailSubscriptionService.subscribe(contactRequest);

		}
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
