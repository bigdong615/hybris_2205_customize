/*
 * Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.braintree.controllers;

public interface BraintreeaddonControllerConstants
{
    final String ADDON_PREFIX = "addon:/braintreeaddon/";
    final String CLIENT_TOKEN = "client_token";
    final String PAY_PAL_CHECKOUT_DATA = "payPalCheckoutData";
    final String IS_ADDRESS_OPEN = "is_credit_card_select";
    final String PAY_PAL_CONFIGURATION_DATA = "payPalConfigurationData";
    final String ADD_PAYMENT_METHOD_SHOW = "addPaymentMethodShow";

    //localized error messages
    public static final String GENERAL_HEAD_ERROR = "braintree.checkout.general.error";
    public static final String GENERAL_HEAD_ERROR_MESSAGE = "braintree.checkout.general.error.message";

    public static final String PAY_PAL_GUEST_REGISTER_ERROR = "braintree.checkout.guest.error";
    public static final String PAY_PAL_ADDRESS_ERROR = "braintree.paypal.shipping.error";
    public static final String PAY_PAL_HAED_ERROR = "braintree.paypal.head.error";

    interface Views
    {
        interface Pages
        {
            interface MultiStepCheckout
            {
                String SilentOrderPostPage = ADDON_PREFIX + "pages/checkout/multi/silentOrderPostPage";
                String CheckoutOrderPageErrorPage = ADDON_PREFIX + "pages/checkout/multi/checkoutErrorPage";
                String CheckoutSummaryPage = ADDON_PREFIX + "pages/checkout/multi/checkoutSummaryPage";
                String FallbackPage = ADDON_PREFIX + "pages/checkout/multi/fallbackPage";
            }

            interface Checkout
            {
                String CheckoutConfirmationPage = ADDON_PREFIX + "pages/checkout/checkoutConfirmationPage";
                String CheckoutLoginPage = "pages/checkout/checkoutLoginPage";
            }
        }

        interface Fragments
        {
            interface Cart
            {
                String CartPopup = ADDON_PREFIX + "fragments/cart/cartPopup";
                String AddToCartPopup = ADDON_PREFIX + "fragments/cart/addToCartPopup";
            }
        }
    }
}
