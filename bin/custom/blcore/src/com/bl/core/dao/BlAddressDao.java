/**
 *
 */
package com.bl.core.dao;

import de.hybris.platform.core.model.user.AddressModel;

import java.util.List;

import com.bl.core.model.BlItemsBillingChargeModel;

/**
 * @author srinivas
 *
 */
public interface BlAddressDao
{
	AddressModel getAddressById(String addressId);

	List<BlItemsBillingChargeModel> getBillChargeList(final String[] chargeCodes);
}
