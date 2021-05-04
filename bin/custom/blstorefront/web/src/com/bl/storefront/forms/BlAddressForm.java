package com.bl.storefront.forms;

import de.hybris.platform.acceleratorstorefrontcommons.forms.AddressForm;
import javax.validation.constraints.Email;
/*
 * This form is created to contain email data.
 *  @author  Vijay Vishwakarma
 */
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
