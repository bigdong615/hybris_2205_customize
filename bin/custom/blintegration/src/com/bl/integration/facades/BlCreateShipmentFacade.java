/**
 *
 */
package com.bl.integration.facades;

import com.bl.core.enums.CarrierEnum;
import com.bl.core.model.OptimizedShippingMethodModel;
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
	boolean createBlShipmentPackages(PackagingInfoModel packagingInfo,final int packageCount, final Map<String, Integer> sequenceMap) throws ParseException, IOException;

	boolean createBlShipmentPackages(PackagingInfoModel packagingInfo, final int packageCount, final Map<String, Integer> sequenceMap,
			final CarrierEnum shippingType, final OptimizedShippingMethodModel optimizedShippingMethod)
			throws ParseException, IOException;


	/**
	 * method will generate return label for shipment
	 * @param packagingInfo as PackageInfo
	 * @param warehouseModel as WarehouseModel
	 * @param packageCount as PackageCount
	 * @param sequenceMap as SequenceMap
	 * @throws IOException 
	 */
	public boolean createBlReturnShipmentPackages(final PackagingInfoModel packagingInfo, final WarehouseModel warehouseModel,final int packageCount,
			final Map<String, Integer> sequenceMap) throws IOException;
}
