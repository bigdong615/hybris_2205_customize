/**
 *
 */
package com.bl.integration.shipping.ups.converters.populator;

import java.util.ArrayList;
import java.util.List;

import com.bl.integration.ups.ship.v1.pojo.PackageResultsType;
import com.bl.integration.ups.ship.v1.pojo.ShipmentResponse;
import com.bl.integration.ups.ship.v1.pojo.ShipmentResultsType;
import com.bl.shipment.data.UPSShipmentCreateResponse;
import com.bl.shipment.data.UPSShipmentPackageResult;


/**
 * @author Aditi Sharma
 *
 */
public class BLUPSShipmentCreateResponsePopulator
{
	/**
	 * @param upsResponse
	 * @return
	 */
	public UPSShipmentCreateResponse convertUPSResponse(final ShipmentResponse upsResponse)
	{
		final UPSShipmentCreateResponse shipmentResponse = new UPSShipmentCreateResponse();

		final ShipmentResultsType shipmentResult = upsResponse.getShipmentResults();

		shipmentResponse.setShipmentIdentificationNumber(shipmentResult.getShipmentIdentificationNumber());
		shipmentResponse.setLabelURL(shipmentResult.getLabelURL());

		if (shipmentResult.getShipmentCharges() != null && shipmentResult.getShipmentCharges().getTotalCharges() != null
				&& shipmentResult.getShipmentCharges().getTotalCharges().getMonetaryValue() != null)
		{
			final Double totalCharges = Double.valueOf(shipmentResult.getShipmentCharges().getTotalCharges().getMonetaryValue());
			shipmentResponse.setTotalCharges(totalCharges);
		}

		final List<UPSShipmentPackageResult> packageResultData = new ArrayList<>();
		for (final PackageResultsType packageResult : shipmentResult.getPackageResults())
		{
			final UPSShipmentPackageResult shipmentPackage = new UPSShipmentPackageResult();
			shipmentPackage.setGraphicImage(packageResult.getShippingLabel().getGraphicImage());
			shipmentPackage.setHTMLImage(packageResult.getShippingLabel().getHTMLImage());
			shipmentPackage.setTrackingNumber(packageResult.getTrackingNumber());
			packageResultData.add(shipmentPackage);
		}
		shipmentResponse.setPackages(packageResultData);

		return shipmentResponse;

	}
}
