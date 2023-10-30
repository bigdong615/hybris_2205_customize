package com.bl.facades.populators;

import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.core.model.user.AddressModel;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.logging.BlLogger;


public class DomoAddressPopulator extends BlAddressPopulator
{
	private static final Logger LOG = Logger.getLogger(DomoAddressPopulator.class);

	@Override
	public void populate(final AddressModel source, final AddressData target)
	{
		try
		{
		super.populate(source, target);
		target.setCreatedTS(source.getCreationtime());
		target.setModifiedTS(source.getModifiedtime());
		if (source.getOriginal() != null)
		{
			target.setOriginal(source.getOriginal().getAddressID());
		}
		if (source.getDuplicate() != null)
		{
			target.setDuplicate(source.getDuplicate());
		}
		target.setAppartment(source.getAppartment());
		target.setBuilding(source.getBuilding());
		target.setDepartment(source.getDepartment());
		target.setFax(source.getFax());
		target.setMiddleName(source.getMiddlename());
		target.setMiddleName2(source.getMiddlename2());
		target.setPoBox(source.getPobox());
		target.setStreetName(source.getStreetname());
		target.setStreetNumber(source.getStreetnumber());
		if (source.getGender() != null)
		{
			target.setGender(source.getGender().getCode());
		}
		if (source.getDateOfBirth() != null)
		{
			target.setDateOfBirth(source.getDateOfBirth());
		}
		target.setRemarks(source.getRemarks());
		if (source.getUnloadingAddress() != null)
		{
			target.setUnloadingAddress(source.getUnloadingAddress());
		}
		if (source.getContactAddress() != null)
		{
			target.setContactaAddress(source.getContactAddress());
		}
		target.setZone(source.getZone());
		if (source.getLatitude() != null)
		{
			target.setLatitude(source.getLatitude());
		}
		if (source.getLongitude() != null)
		{
			target.setLongitude(source.getLongitude());
		}
		target.setPhone2(source.getPhone2());
		target.setPrimaryKey(source.getPk().toString());
		target.setBillingAddress(source.getBillingAddress());
		target.setShippingAddress(source.getShippingAddress());
		target.setUnloadingAddress(source.getUnloadingAddress());
		target.setContactaAddress(source.getContactAddress());
		if (source.getOwner() != null)
		{
			target.setOwnerPk(source.getOwner().getPk().toString());
		}
	}
	catch (final Exception exception)
	{
		LOG.info("Error while getting Address for PK " + source.getPk().toString());
		BlLogger.logMessage(LOG, Level.ERROR, StringUtils.EMPTY, "Error while getting Address", exception);
		exception.printStackTrace();

	}
	}
}
