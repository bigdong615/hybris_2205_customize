/**
 *
 */
package com.bl.integration.services;

import de.hybris.platform.ordersplitting.model.ConsignmentModel;

import com.bl.facades.shipment.data.FedExShippingRequestData;
import com.bl.facades.shipment.data.UpsShippingRequestData;
import com.bl.shipment.data.UPSShipmentCreateResponse;


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
	 * method will used to create shipment for FedEx
	 * @param upsShipmentRequest for fedEx shipment
	 * @return String
	 */
	String createFedExShipment(final FedExShippingRequestData upsShipmentRequest);

	/**
	 * method will used to check the order status for shipment
	 * @param consignment for ConsignmentModel
	 * @return boolean
	 */
	public boolean checkOrderStatus(final ConsignmentModel consignment);
}
