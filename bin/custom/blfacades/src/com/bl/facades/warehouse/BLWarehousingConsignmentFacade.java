/**
 *
 */
package com.bl.facades.warehouse;

import de.hybris.platform.warehousing.model.PackagingInfoModel;
import de.hybris.platform.warehousingfacades.order.data.PackagingInfoData;

import java.util.List;


/**
 * @author Keyur
 *
 */
public interface BLWarehousingConsignmentFacade
{
	public PackagingInfoModel createPackagingInformationOnConsignment(String code, PackagingInfoData packagingInfoData);

	public List<PackagingInfoData> getAllPackagingDimensions();
}
