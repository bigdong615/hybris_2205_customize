/**
 *
 */
package com.bl.integration.facades;

import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.io.IOException;
import java.text.ParseException;
import java.util.Map;


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
	 * @param packagingInfo as PackageInfo
	 * @param packageCount as PackageCount
	 * @param sequenceMap as SequenceMap
	 * @throws ParseException as Exception
	 * @throws IOException 
	 */
	void createBlShipmentPackages(PackagingInfoModel packagingInfo,final int packageCount, final Map<String, Integer> sequenceMap) throws ParseException, IOException;


	/**
	 * method will generate return label for shipment
	 * @param packagingInfo as PackageInfo
	 * @param warehouseModel as WarehouseModel
	 * @param packageCount as PackageCount
	 * @param sequenceMap as SequenceMap
	 * @throws IOException 
	 */
	public void createBlReturnShipmentPackages(final PackagingInfoModel packagingInfo, final WarehouseModel warehouseModel,final int packageCount,
			final Map<String, Integer> sequenceMap) throws IOException;
}
