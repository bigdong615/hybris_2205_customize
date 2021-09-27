package com.bl.integration.facades.impl;

import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.text.ParseException;

import javax.annotation.Resource;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Value;

import com.bl.core.enums.CarrierEnum;
import com.bl.integration.constants.BlintegrationConstants;
import com.bl.integration.facades.BlCreateShipmentFacade;
import com.bl.integration.populators.BLFedExShippingDataPopulator;
import com.bl.integration.populators.BLUpsShippingDataPopulator;
import com.bl.integration.services.impl.DefaultBLShipmentCreationService;
import com.bl.logging.BlLogger;
import com.bl.shipment.data.UPSShipmentCreateResponse;
import com.bl.shipment.data.UPSShipmentPackageResult;


/**
 * ##################### BL-756, BL-849 ################### This class is used to create shipment package
 *
 * @author Aditi Sharma
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

	@Value("${blintegration.ups.shipment.label.url}")
	private String upsShipmentURL;

	/**
	 * This method is used to create shipment package
	 *
	 * @param packagingInfo
	 *           as Package Info
	 * @throws ParseException
	 */
	@Override
	public void createBlShipmentPackages(final PackagingInfoModel packagingInfo) throws ParseException
	{
		BlLogger.logMessage(LOG, Level.INFO, BlintegrationConstants.UPS_SHIPMENT_MSG);

		final ZoneDeliveryModeModel zoneDeliveryMode = (ZoneDeliveryModeModel) packagingInfo.getConsignment().getDeliveryMode();
		final CarrierEnum delivertCarrier = zoneDeliveryMode.getCarrier();

		if (delivertCarrier.getCode().equalsIgnoreCase(CarrierEnum.UPS.getCode()))
		{
			final UPSShipmentCreateResponse upsResponse = getBlShipmentCreationService()
					.createUPSShipment(getBlUpsShippingDataPopulator().populateUPSShipmentRequest(packagingInfo));
			if (upsResponse != null)
			{
				saveResponseOnPackage(upsResponse, packagingInfo);
			}
		}
		else
		{
			getBlShipmentCreationService()
					.createFedExShipment(getBlFedExShippingDataPopulator().populateFedExShipmentRequest(packagingInfo));
		}
	}

	/**
	 * This method is used to create return shipment package
	 *
	 * @param packagingInfo
	 *           as Package Info
	 * @throws ParseException
	 */
	@Override
	public void createBlReturnShipmentPackages(final PackagingInfoModel packagingInfo, final WarehouseModel warehouseModel)
	{
		BlLogger.logMessage(LOG, Level.INFO, BlintegrationConstants.RETURN_SHIPMENT_MSG);

		final ZoneDeliveryModeModel zoneDeliveryMode = (ZoneDeliveryModeModel) packagingInfo.getConsignment().getDeliveryMode();
		final CarrierEnum delivertCarrier = zoneDeliveryMode.getCarrier();

		if (delivertCarrier.getCode().equalsIgnoreCase(CarrierEnum.UPS.getCode()))
		{
			final UPSShipmentCreateResponse upsResponse = getBlShipmentCreationService().createUPSShipment(
					getBlUpsShippingDataPopulator().populateUPSReturnShipmentRequest(packagingInfo, warehouseModel));
			if (upsResponse != null)
			{
				saveResponseOnPackage(upsResponse, packagingInfo);
			}
		}
		else
		{
			getBlShipmentCreationService()
					.createFedExShipment(getBlFedExShippingDataPopulator().populateFedExShipmentRequest(packagingInfo));
		}
	}

	/**
	 * method will be used to save the UPS shipment response on package
	 *
	 * @param upsResponse
	 * @param packagingInfo
	 */
	private void saveResponseOnPackage(final UPSShipmentCreateResponse upsResponse, final PackagingInfoModel packagingInfo)
	{
		final UPSShipmentPackageResult shipmentPackage = upsResponse.getPackages().get(0);
		
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Shipment generated for package {} with tracking id {}", packagingInfo,shipmentPackage.getTrackingNumber());
		packagingInfo.setLabelURL(upsResponse.getLabelURL());
		packagingInfo.setShipmentIdentificationNumber(upsResponse.getShipmentIdentificationNumber());
		packagingInfo.setTotalShippingPrice(upsResponse.getTotalCharges());
		
		packagingInfo.setTrackingNumber(shipmentPackage.getTrackingNumber());
		packagingInfo.setHTMLImage(shipmentPackage.getHTMLImage());
		packagingInfo.setGraphicImage(shipmentPackage.getGraphicImage());
		packagingInfo.setLabelURL(upsShipmentURL + shipmentPackage.getTrackingNumber());

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
