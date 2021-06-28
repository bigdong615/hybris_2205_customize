/**
 *
 */
package com.bl.integration.facades;

import de.hybris.platform.warehousing.model.PackagingInfoModel;


/**
 * @author Aditi Sharma
 *
 */
public interface BlCreateShipmentFacade
{
	/**
	 * @param packagingInfo
	 */
	void createBlShipmentPackages(PackagingInfoModel packagingInfo);
}
