/**
 *
 */
package com.bl.integration.services;

import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.util.List;
import java.util.Map;

import com.bl.facades.shipment.data.UpsShippingRequestData;
import com.bl.shipment.data.UPSShipmentCreateResponse;
import com.fedex.ship.stub.ProcessShipmentReply;


/**
 * interface will use for shipment creation
 * @author Aditi Sharma
 *
 */
public interface BLShipmentCreationService
{
	/**
	 * method will used to create shipment for UPS
	 * @param upsShipmentRequest for ups shipment
	 * @return UPSShipmentCreateResponse
	 */
	UPSShipmentCreateResponse createUPSShipment(final UpsShippingRequestData upsShipmentRequest);


	/**
	 * method will be used to create FedEx Shipment
	 * @param packagingInfo as PackageInfo
	 * @param packageCount as PackageCount
	 * @param sequenceMap as SequenceMap
	 * @param warehouseModel as WarehouseModel
	 * @return ProcessShipmentReply
	 */
	ProcessShipmentReply createFedExShipment(final PackagingInfoModel packagingInfo, final int packageCount, final Map<String, Integer> sequenceMap,final WarehouseModel warehouseModel);

	/**
	 * method will used to check the order status for shipment
	 * @param consignment for ConsignmentModel
	 * @return boolean
	 */
	public boolean checkOrderStatus(final ConsignmentModel consignment);
	
	/**
	 * Gets the package for serial.
	 *
	 * @param consignment
	 *           the consignment
	 * @param serialCode
	 *           the serial code
	 * @return the package for serial
	 */
	PackagingInfoModel getPackageForSerial(final ConsignmentModel consignment, final String serialCode);
	
	/**
	 * This method is used to get the sequence number for shipment
	 * @param sequenceMap as SequenceMap
	 * @param packages as Packages
	 * @param packageCount as PackageCount
	 * @return sequenceMap
	 */
	Map<String, Integer> getSequenceNumber(final Map<String, Integer> sequenceMap, final List<PackagingInfoModel> packages,
			final int packageCount);
}
