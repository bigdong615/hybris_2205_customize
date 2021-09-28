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
    
    public static final String FROM_PAGE_STATUS = "fromPage";
    public static final String GIFTCARDPURCHASENAME = "name";
    public static final String GIFTCARDPURCHASEEMAIL = "email";
    public static final String GIFTCARDPURCHASEMESSAGE = "message";
    public static final String REVIEW_PAGE = "Review";
    public static final String RENTAL_DATE = "rentalDate";
    public static final String CURRENT_PAGE = "currentPage";
    public static final String DATE_FORMAT_PATTERN = "dd-MM-yyyy";
    public static final String FORMATTED_RENTAL_START_DATE = "formattedRentalStartDate";
    public static final String FORMATTED_RENTAL_END_DATE = "formattedRentalEndDate";
    public static final String REVIEW_PAGE_DATE_FORMAT = "EEEE, MMM d";
    public static final String GIFT_CARD_REMOVE = "giftCardCodeRemove";
    public static final String PAYMENT_METHOD_CHECKOUT_URL = "/checkout/multi/payment-method/add";
    
    public static final String CART_DATA = "cartData";
    public static final String ALL_ITEMS = "allItems";
    public static final String DELIVERY_ADDRESS = "deliveryAddress";
    public static final String DELIVERY_MODE = "deliveryMode";
    public static final String PAYMENT_INFO = "paymentInfo";
    public static final String PAY_BILL_SUCCESS_CMS_PAGE = "pay-bill-success";
    public static final String DEPOSIT_SUCCESS_CMS_PAGE = "deposit-payment-success";
    
    public static final String ORDER_DATA = "orderData";
    public static final String DEPOSIT_AMOUNT = "depositAmount";
    public static final String PAYMENT_TYPE = "paymentType";
    public static final String CREDIT_CARD = "Credit Card";
    public static final String PAY_PAL = "PayPal";
    public static final String PAYMENT_ID = "paymentId";
    public static final String PAYMENT_NONCE = "paymentNonce";
    public static final String DEPOSIT_ORDER_TOTAL = "depositOrderTotal";

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
