package com.bl.storefront.forms;

import de.hybris.platform.acceleratorstorefrontcommons.forms.AddressForm;
import javax.validation.constraints.Email;
import java.util.Map;

/*
 * This form is created to contain email data.
 *  @author  Vijay Vishwakarma
 */
public class BlAddressForm extends AddressForm {


    private String addressType;
    private String email;
    private boolean upsStoreAddress;
    private boolean pickStoreAddress;
    private String openingDaysDetails;
    private String companyName;
    private Boolean defaultBillingAddress;

    public boolean isPickStoreAddress() {
        return pickStoreAddress;
    }

    public void setPickStoreAddress(boolean pickStoreAddress) {
        this.pickStoreAddress = pickStoreAddress;
    }

    public String getOpeningDaysDetails() {
        return openingDaysDetails;
    }

    public void setOpeningDaysDetails(String openingDaysDetails) {
        this.openingDaysDetails = openingDaysDetails;
    }

    public boolean isUpsStoreAddress() {
        return upsStoreAddress;
    }

    public void setUpsStoreAddress(boolean upsStoreAddress) {
        this.upsStoreAddress = upsStoreAddress;
    }

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

    public String getCompanyName() {
        return companyName;
    }

    public void setCompanyName(String companyName) {
        this.companyName = companyName;
    }
    public Boolean getDefaultBillingAddress() {
        return defaultBillingAddress;
    }

    public void setDefaultBillingAddress(Boolean defaultBillingAddress) {
        this.defaultBillingAddress = defaultBillingAddress;
    }

}
