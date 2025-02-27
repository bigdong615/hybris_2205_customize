package com.braintree.controllers.form;

import de.hybris.platform.acceleratorstorefrontcommons.forms.PlaceOrderForm;

import java.util.HashMap;
import java.util.Map;


public class BraintreePlaceOrderForm extends PlaceOrderForm {

    private Map<String, String> customFields = new HashMap<>();
    private String shipsFromPostalCode;
    private boolean newsLetterSubscriptionOpted;

    public Map<String, String> getCustomFields() {
        return customFields;
    }

    public void setCustomFields(Map<String, String> customFields) {
        this.customFields = customFields;
    }

    public String getShipsFromPostalCode() {
        return shipsFromPostalCode;
    }

    public void setShipsFromPostalCode(String shipsFromPostalCode) {
        this.shipsFromPostalCode = shipsFromPostalCode;
    }

    public boolean isNewsLetterSubscriptionOpted() {
        return newsLetterSubscriptionOpted;
    }

    public void setNewsLetterSubscriptionOpted(final boolean newsLetterSubscriptionOpted) {
        this.newsLetterSubscriptionOpted = newsLetterSubscriptionOpted;
    }

}
