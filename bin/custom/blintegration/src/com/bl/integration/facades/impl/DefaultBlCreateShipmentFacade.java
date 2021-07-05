/**
 *
 */
package com.bl.integration.facades.impl;


import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import javax.annotation.Resource;

import org.apache.log4j.Logger;

import com.bl.core.enums.CarrierEnum;
import com.bl.integration.facades.BlCreateShipmentFacade;
import com.bl.integration.populators.BLFedExShippingDataPopulator;
import com.bl.integration.populators.BLUpsShippingDataPopulator;
import com.bl.integration.services.impl.DefaultBLShipmentCreationService;
import com.bl.shipment.data.UPSShipmentCreateResponse;
import com.bl.shipment.data.UPSShipmentPackageResult;


/**
 * @author Aditi Sharma
 *
 */
public class DefaultBlCreateShipmentFacade implements BlCreateShipmentFacade
{
	private static final Logger LOG = Logger.getLogger(DefaultBlCreateShipmentFacade.class);

	private ModelService modelService;

	@Resource(name = "blUpsShippingDataPopulator")
	private BLUpsShippingDataPopulator blUpsShippingDataPopulator;

	@Resource(name = "blFedExShippingDataPopulator")
	private BLFedExShippingDataPopulator blFedExShippingDataPopulator;

	@Resource(name = "blShipmentCreationService")
	private DefaultBLShipmentCreationService blShipmentCreationService;

	/**
	 * @param packagingInfo
	 */
	@Override
	public void createBlShipmentPackages(final PackagingInfoModel packagingInfo)
	{
		final ConsignmentModel consignment = packagingInfo.getConsignment();
		final ZoneDeliveryModeModel deliveryMode = (ZoneDeliveryModeModel) consignment.getDeliveryMode();
		final CarrierEnum carrier = deliveryMode.getCarrier();
		if (carrier.equals(CarrierEnum.UPS))
		{
			final UPSShipmentCreateResponse upsResponse = getBlShipmentCreationService()
					.createUPSShipment(getBlUpsShippingDataPopulator().populateUPSShipmentRequest(packagingInfo));
			saveResponseOnPackage(upsResponse, packagingInfo);
		}
		else
		{
			getBlShipmentCreationService().createFedExShipment(
					getBlFedExShippingDataPopulator().populateFedExShipmentRequest(packagingInfo.getConsignment()));
		}
	}

	/**
	 * @param upsResponse
	 * @param packagingInfo
	 */
	private void saveResponseOnPackage(final UPSShipmentCreateResponse upsResponse, final PackagingInfoModel packagingInfo)
	{
		packagingInfo.setLabelURL(upsResponse.getLabelURL());
		packagingInfo.setShipmentIdentificationNumber(upsResponse.getShipmentIdentificationNumber());
		packagingInfo.setTotalShippingPrice(upsResponse.getTotalCharges());
		final UPSShipmentPackageResult shipmentPackage = upsResponse.getPackages().get(0);
		packagingInfo.setTrackingNumber(shipmentPackage.getTrackingNumber());
		packagingInfo.setHTMLImage(shipmentPackage.getHTMLImage());
		packagingInfo.setGraphicImage(shipmentPackage.getGraphicImage());

		getModelService().save(packagingInfo);
		getModelService().refresh(packagingInfo);
	}

	/**
	 * @return the blUpsShippingDataPopulator
	 */
	public BLUpsShippingDataPopulator getBlUpsShippingDataPopulator()
	{
		return blUpsShippingDataPopulator;
	}

	/**
	 * @param blUpsShippingDataPopulator
	 *           the blUpsShippingDataPopulator to set
	 */
	public void setBlUpsShippingDataPopulator(final BLUpsShippingDataPopulator blUpsShippingDataPopulator)
	{
		this.blUpsShippingDataPopulator = blUpsShippingDataPopulator;
	}

	/**
	 * @return the modelService
	 */
	public ModelService getModelService()
	{
		return modelService;
	}

	/**
	 * @param modelService
	 *           the modelService to set
	 */
	public void setModelService(final ModelService modelService)
	{
		this.modelService = modelService;
	}

	/**
	 * @return the blShipmentCreationService
	 */
	public DefaultBLShipmentCreationService getBlShipmentCreationService()
	{
		return blShipmentCreationService;
	}

	/**
	 * @param blShipmentCreationService
	 *           the blShipmentCreationService to set
	 */
	public void setBlShipmentCreationService(final DefaultBLShipmentCreationService blShipmentCreationService)
	{
		this.blShipmentCreationService = blShipmentCreationService;
	}

	/**
	 * @return the blFedExShippingDataPopulator
	 */
	public BLFedExShippingDataPopulator getBlFedExShippingDataPopulator()
	{
		return blFedExShippingDataPopulator;
	}

	/**
	 * @param blFedExShippingDataPopulator
	 *           the blFedExShippingDataPopulator to set
	 */
	public void setBlFedExShippingDataPopulator(final BLFedExShippingDataPopulator blFedExShippingDataPopulator)
	{
		this.blFedExShippingDataPopulator = blFedExShippingDataPopulator;
	}

}
