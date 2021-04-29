package com.bl.facades.populators;


import de.hybris.platform.commercefacades.user.converters.populator.AddressPopulator;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.user.AddressModel;

/*
 * This populator is used for populating Email data.
 *  @author  Vijay Vishwakarma
 */
public class BlAddressPopulator extends AddressPopulator {

  @Override
  public void populate(final AddressModel source, final AddressData target){
    target.setEmail(source.getEmail());
    super.populate(source,target);
  }

}
