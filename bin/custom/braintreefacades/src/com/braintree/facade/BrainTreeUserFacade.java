package com.braintree.facade;

import de.hybris.platform.commercefacades.order.data.CCPaymentInfoData;
import de.hybris.platform.commercefacades.user.UserFacade;
import de.hybris.platform.commercefacades.user.data.AddressData;

import java.util.List;

import com.braintree.hybris.data.BrainTreeSubscriptionInfoData;
import com.braintree.model.BrainTreePaymentInfoModel;


/**
 * BrainTree User facade interface. Deals with methods related to user operations - registering, logging in and other.
 */
public interface BrainTreeUserFacade extends UserFacade
{
	/**
	 * remove the credit card info from the BrainTree by paymentMethodToken
	 *
	 * @param paymentMethodToken
	 *           the paymentMethodToken
	 */
	void removeBTCCPaymentInfo(String paymentMethodToken);

	/**
	 *
	 * @param saved
	 * @return
	 */
	List<CCPaymentInfoData> getBrainTreeCCPaymentInfos(boolean saved);

	/**
	 * add payment info
	 * 
	 * @param subscriptionInfoData
	 */
	BrainTreePaymentInfoModel addPaymentMethod(BrainTreeSubscriptionInfoData subscriptionInfoData);

	boolean editPaymentMethod(final CCPaymentInfoData paymentInfo, final String expirationDate, String cvv,
			final AddressData addressData, final String defaultCard);

	/**
	 * This method is responsible for setting default billing address.
	 */
	public void setDefaultBillingAddress(final AddressData addressData);
	/**
	 * This method is responsible for getting default billing address.
	 */
	public AddressData getDefaultBillingAddress();

	/**
	 * This method is responsible for getting all shipping address.
	 */
	 List<AddressData> getShippingAddressBook();
}
