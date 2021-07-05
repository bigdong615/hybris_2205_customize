/**
 *
 */
package com.braintree.constants;

import com.braintree.model.PayPalCreditMessageComponentModel;

public interface ControllerConstants
{
	/**
	 * Class with view name constants
	 */

	interface Actions
	{
		interface Cms
		{
			String _Prefix = "/view/";
			String _Suffix = "Controller";

			String PayPalCreditMessageComponent = _Prefix + PayPalCreditMessageComponentModel._TYPECODE + _Suffix;
		}
	}
	interface Views
	{

		interface Pages
		{

			interface MultiStepCheckout
			{
				String AddEditDeliveryAddressPage = "pages/checkout/multi/addEditDeliveryAddressPage";
				String ChooseDeliveryMethodPage = "pages/checkout/multi/chooseDeliveryMethodPage";
				String ChoosePickupLocationPage = "pages/checkout/multi/choosePickupLocationPage";
				String AddPaymentMethodPage = "pages/checkout/multi/addPaymentMethodPage";
				String CheckoutSummaryPage = "pages/checkout/multi/checkoutSummaryPage";
				String HostedOrderPageErrorPage = "pages/checkout/multi/hostedOrderPageErrorPage";
				String HostedOrderPostPage = "pages/checkout/multi/hostedOrderPostPage";
				String SilentOrderPostPage = "pages/checkout/multi/silentOrderPostPage";
				String AccountLayoutPage = "pages/account/accountLayoutPage";
				String FallbackPage = "pages/checkout/multi/fallbackPage";
				String ReviewPrint = "pages/checkout/multi/reviewPrint";
			}
		}

	}
}
