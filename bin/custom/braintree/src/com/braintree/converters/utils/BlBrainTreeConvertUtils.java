package com.braintree.converters.utils;

import com.braintree.command.request.BrainTreeAddressRequest;

import de.hybris.platform.commercefacades.user.data.AddressData;

/**
 * The Class BlBrainTreeConvertUtils used to convert basic things as per the requirement.
 */
public class BlBrainTreeConvertUtils
{

  /**
   * Convert brain tree address.
   *
   * @param customerID the customer ID
   * @param address the address
   * @return the brain tree address request
   */
  public static BrainTreeAddressRequest convertBrainTreeAddress(final String customerID, final AddressData address)
  {
    final BrainTreeAddressRequest addressRequest = new BrainTreeAddressRequest(customerID);
    addressRequest.setCompany(address.getCompanyName());
    addressRequest.setStreetAddress(address.getLine1());
    addressRequest.setExtendedAddress(address.getLine2());
    addressRequest.setFirstName(address.getFirstName());
    addressRequest.setLastName(address.getLastName());
    addressRequest.setLocality(address.getTown());
    addressRequest.setPostalCode(address.getPostalCode());

    if (address.getCountry() != null)
    {
      addressRequest.setCountryCodeAlpha2(address.getCountry().getIsocode());
    }
    if (address.getRegion() != null)
    {
      // The state or province. For PayPal addresses, the region must be a 2-letter abbreviation; for all other payment methods, it must be less than or equal to 255 characters.
      // because of hybris specific use the isocodeShort - 2 character isocode - and its right for braintree
      // the isocode return 2 character isocode US-CA - wrong for braintree
      addressRequest.setRegion(address.getRegion().getIsocodeShort());
    }
    return addressRequest;
  }
}
