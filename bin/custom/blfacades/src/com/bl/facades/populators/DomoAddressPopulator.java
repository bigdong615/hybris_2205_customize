package com.bl.facades.populators;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.core.model.user.AddressModel;
public class DomoAddressPopulator extends BlAddressPopulator
{
    @Override
    public void populate(final AddressModel source, final AddressData target) {
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
    }
}