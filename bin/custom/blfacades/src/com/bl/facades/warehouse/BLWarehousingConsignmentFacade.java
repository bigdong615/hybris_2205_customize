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
	/**
	 * Create Package using packagingInfoData and save it on Consignment
	 *
	 * @param consignmentCode
	 * @param packagingInfoData
	 * @return PackagingInfoModel
	 */
	public PackagingInfoModel createPackagingInformationOnConsignment(String code, PackagingInfoData packagingInfoData);


	/**
	 * Get all the Packaging Dimensions
	 * 
	 * @return List of all PackagingInfoData
	 */
	public List<PackagingInfoData> getAllPackagingDimensions();
}
