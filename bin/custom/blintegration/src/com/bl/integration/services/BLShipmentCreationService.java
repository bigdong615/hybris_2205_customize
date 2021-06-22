/**
 *
 */
package com.bl.integration.services;

import com.bl.facades.shipment.data.FedExShippingRequestData;
import com.bl.facades.shipment.data.UpsShippingRequestData;
import com.bl.shipment.data.UPSShipmentCreateResponse;


/**
 * @author Aditi Sharma
 *
 */
public interface BLShipmentCreationService
{
	UPSShipmentCreateResponse createUPSShipment(final UpsShippingRequestData upsShipmentRequest);

	String createFedExShipment(final FedExShippingRequestData upsShipmentRequest);
}
