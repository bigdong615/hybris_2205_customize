/**
 *
 */
package com.bl.integration.services;

import de.hybris.platform.ordersplitting.model.ConsignmentModel;

import com.bl.facades.shipment.data.FedExShippingRequestData;
import com.bl.facades.shipment.data.UpsShippingRequestData;
import com.bl.shipment.data.UPSShipmentCreateResponse;


/**
 * @author Aditi Sharma
 *
 */
public interface BLShipmentCreationService
{
	/**
	 * @param upsShipmentRequest
	 * @return
	 */
	UPSShipmentCreateResponse createUPSShipment(final UpsShippingRequestData upsShipmentRequest);

	/**
	 * @param upsShipmentRequest
	 * @return
	 */
	String createFedExShipment(final FedExShippingRequestData upsShipmentRequest);

	/**
	 * @param consignment
	 * @return
	 */
	public boolean checkOrderStatus(final ConsignmentModel consignment);
}
