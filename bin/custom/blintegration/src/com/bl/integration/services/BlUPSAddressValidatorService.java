package com.bl.integration.services;

import com.bl.facades.ups.address.data.AVSResposeData;
import de.hybris.platform.commercefacades.user.data.AddressData;

public interface BlUPSAddressValidatorService {
  public AVSResposeData getVerifiedAddress(AddressData addressData);
}
