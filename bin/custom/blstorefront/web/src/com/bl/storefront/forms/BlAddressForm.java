package com.bl.storefront.forms;

import de.hybris.platform.acceleratorstorefrontcommons.forms.AddressForm;

import javax.validation.constraints.Email;
import javax.validation.constraints.NotNull;

public class BlAddressForm extends AddressForm {

    private String addressType;
    private String email;

    public String getAddressType() {
        return addressType;
    }

    public void setAddressType(String addressType) {
        this.addressType = addressType;
    }

    @Email
    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }
}
