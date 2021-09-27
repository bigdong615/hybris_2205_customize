/**
 *
 */
package com.bl.integration.facades;

import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.text.ParseException;


/**
 * use to generate shipment labels
 *
 * @author Aditi Sharma
 *
 */
public interface BlCreateShipmentFacade
{
	/**
	 * method will generate label for shipment
	 *
	 * @param packagingInfo
	 *           as Package Info
	 * @throws ParseException
	 */
	void createBlShipmentPackages(PackagingInfoModel packagingInfo) throws ParseException;

	/**
	 * method will generate return label for shipment
	 *
	 * @param packagingInfo
	 *           as Package Info
	 */
	public void createBlReturnShipmentPackages(final PackagingInfoModel packagingInfo, final WarehouseModel warehouseModel);
}
