package com.bl.storefront.forms;

import de.hybris.platform.acceleratorstorefrontcommons.forms.AddressForm;
/*
 * This form is created to contain email data.
 *  @author  Vijay Vishwakarma
 */
public class BlAddressForm extends AddressForm {

  private String email;
  public String getEmail() {
    return email;
  }

  public void setEmail(String email) {
    this.email = email;
  }

}
