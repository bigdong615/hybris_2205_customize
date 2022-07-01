/**
 *
 */
package com.bl.integration.populators;

import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.warehousing.model.PackagingInfoModel;
import de.hybris.platform.warehousingfacades.order.data.PackagingInfoData;

import com.bl.integration.data.UserData;

/**
 * @author Aditi
 *
 */
public class BlPrintShippingLabelPopulator implements Populator<PackagingInfoModel, PackagingInfoData>
{

	@Override
	public void populate(PackagingInfoModel source, PackagingInfoData target) throws ConversionException
	{
		if(source !=null && target !=null)
		{
   		target.setOutBoundShippingLabel(source.getOutBoundShippingLabel());	
   		target.setOutBoundGraphicImage(source.getOutBoundGraphicImage());
   		target.setInBoundShippingLabel(source.getInBoundShippingLabel());	
   		target.setInBoundGraphicImage(source.getInBoundGraphicImage());
   	}
	}

}
