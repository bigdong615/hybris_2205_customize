package com.bl.integration.facades.impl;

import com.bl.core.enums.ShippingLabelTypeEnum;
import com.bl.core.model.OptimizedShippingMethodModel;
import com.bl.core.model.ShippingLabelHistoryModel;
import com.google.common.collect.Lists;
import de.hybris.platform.catalog.model.CatalogUnawareMediaModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.text.ParseException;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import javax.annotation.Resource;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
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
import com.fedex.ship.stub.CompletedShipmentDetail;
import com.fedex.ship.stub.NotificationSeverityType;
import com.fedex.ship.stub.ProcessShipmentReply;
import com.fedex.ship.stub.ShipmentRateDetail;
import com.fedex.ship.stub.ShipmentRating;


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

	@Value("${blintegration.fedex.shipment.label.url}")
	private String fedExShipmentURL;

	/**
	 * This method is used to create shipment package
	 *
	 * @param packagingInfo
	 *           as Package Info
	 * @throws IOException 
	 * @throws ParseException
	 */
	@Override
	public boolean createBlShipmentPackages(final PackagingInfoModel packagingInfo, final int packageCount,
			final Map<String, Integer> sequenceMap, boolean isSignatureRequired) throws IOException
	{
		BlLogger.logMessage(LOG, Level.INFO, BlintegrationConstants.UPS_SHIPMENT_MSG);

		final ZoneDeliveryModeModel zoneDeliveryMode = (ZoneDeliveryModeModel) packagingInfo.getConsignment().getDeliveryMode();
		final CarrierEnum delivertCarrier = zoneDeliveryMode.getCarrier();
		return processLabelGeneration(packagingInfo, packageCount, delivertCarrier, sequenceMap, null, isSignatureRequired);
	}

	@Override
	public boolean createBlShipmentPackages(final PackagingInfoModel packagingInfo, final int packageCount,
			final Map<String, Integer> sequenceMap, final CarrierEnum shippingType,
			final OptimizedShippingMethodModel optimizedShippingMethod, boolean isSignatureRequired) throws IOException
	{
		return processLabelGeneration(packagingInfo, packageCount, shippingType, sequenceMap, optimizedShippingMethod, isSignatureRequired);
	}

	private boolean processLabelGeneration(final PackagingInfoModel packagingInfo, final int packageCount,
			final CarrierEnum delivertCarrier, final Map<String, Integer> sequenceMap,
			final OptimizedShippingMethodModel optimizedShippingMethod, boolean isSignatureRequired) throws IOException
	{
		if (StringUtils.isNotBlank(delivertCarrier.getCode())
				&& CarrierEnum.UPS.getCode().equalsIgnoreCase(delivertCarrier.getCode()))
		{
			final UPSShipmentCreateResponse upsResponse = getBlShipmentCreationService().createUPSShipment(
					getBlUpsShippingDataPopulator().populateUPSShipmentRequest(packagingInfo, optimizedShippingMethod, isSignatureRequired), packagingInfo);
			if (upsResponse != null)
			{
				saveResponseOnOutboundPackage(upsResponse, packagingInfo, delivertCarrier, optimizedShippingMethod);
				return true;
			}
			else
			{
				return false;
			}
		}
		else
		{
			return createFedExShipment(packagingInfo, packageCount, sequenceMap, null);
		}
	}

	/**
	 * This method is used to create return shipment package
	 *
	 * @param packagingInfo
	 *           as Package Info
	 * @throws IOException 
	 * @throws ParseException
	 */
	@Override
	public boolean createBlReturnShipmentPackages(final PackagingInfoModel packagingInfo, final WarehouseModel warehouseModel,
			final int packageCount, final Map<String, Integer> sequenceMap) throws IOException
	{
		BlLogger.logMessage(LOG, Level.INFO, BlintegrationConstants.RETURN_SHIPMENT_MSG);

		final ZoneDeliveryModeModel zoneDeliveryMode = (ZoneDeliveryModeModel) packagingInfo.getConsignment().getDeliveryMode();
		final CarrierEnum delivertCarrier = zoneDeliveryMode.getCarrier();

		if (CarrierEnum.UPS.getCode().equalsIgnoreCase(delivertCarrier.getCode()))
		{
			final UPSShipmentCreateResponse upsResponse = getBlShipmentCreationService().createUPSShipment(
					getBlUpsShippingDataPopulator().populateUPSReturnShipmentRequest(packagingInfo, warehouseModel), packagingInfo);
			if (upsResponse != null)
			{
				saveResponseOnInBoundPackage(upsResponse, packagingInfo);
				return true;
			}
			else
			{
				return false;
			}
		}
		else
		{
			return createFedExShipment(packagingInfo, packageCount, sequenceMap, warehouseModel);
		}
	}

	/**
	 * method will be used to create fedExShipment
	 *
	 * @param packagingInfo
	 * @param sequenceMap
	 */
	private boolean createFedExShipment(final PackagingInfoModel packagingInfo, final int packageCount,
			final Map<String, Integer> sequenceMap, final WarehouseModel warehouseModel)
	{

		final ProcessShipmentReply masterReply = getBlShipmentCreationService().createFedExShipment(packagingInfo, packageCount,
				sequenceMap, warehouseModel);

		if (masterReply!=null && isResponseOk(masterReply.getHighestSeverity())) // check if the call was successful
		{
			try
			{
				processResponse(masterReply, packagingInfo, warehouseModel);
				return true;
			}
			catch (final Exception exception)
			{
				BlLogger.logFormatMessageInfo(LOG, Level.ERROR, "Exception occurred while creating fedEx shipment {}",
						exception);
				return false;
			}
		}
		return false;
	}

	/**
	 * This method will check is response returned from FedEx has error or not
	 *
	 * @param notificationSeverityType
	 * @return
	 */
	private static boolean isResponseOk(final NotificationSeverityType notificationSeverityType)
	{
		if (notificationSeverityType == null)
		{
			return false;
		}
		else if (notificationSeverityType.equals(NotificationSeverityType.WARNING))
		{
			BlLogger.logMessage(LOG, Level.DEBUG, "Warning Received from FedEx");
			return true;
		}
		else if (notificationSeverityType.equals(NotificationSeverityType.NOTE))
		{
			BlLogger.logMessage(LOG, Level.DEBUG, "Note Received from FedEx");
			return true;
		}
		else
		{
			BlLogger.logMessage(LOG, Level.DEBUG, "Success Response Received from FedEx");
			return true;
		}
	}

	/**
	 * This method will process the response returned from FedEx shipment service and update it on packagingInfoModel
	 *
	 * @param reply
	 * @param packagingInfo
	 */
	private void processResponse(final ProcessShipmentReply reply, final PackagingInfoModel packagingInfo,
			final WarehouseModel warehouseModel) throws Exception
	{
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Shipment created for Transaction Id {} : ",
				reply.getTransactionDetail().getCustomerTransactionId());
		final CompletedShipmentDetail completedShipmentDetails = reply.getCompletedShipmentDetail();
		ShippingLabelTypeEnum labelTypeEnum = null;
		String trackingNumber = StringUtils.EMPTY;
		if (warehouseModel == null)
		{
			trackingNumber = completedShipmentDetails.getMasterTrackingId().getTrackingNumber();
			packagingInfo.setOutBoundTrackingNumber(trackingNumber);
			labelTypeEnum = ShippingLabelTypeEnum.OUTBOUND;
			packagingInfo.setLabelURL(fedExShipmentURL + completedShipmentDetails.getMasterTrackingId().getTrackingNumber());
		}
		else
		{
			trackingNumber = completedShipmentDetails.getMasterTrackingId().getTrackingNumber();
			packagingInfo.setInBoundTrackingNumber(trackingNumber);
			labelTypeEnum = ShippingLabelTypeEnum.INBOUND;
		}
		setTotalChargesOnPackage(completedShipmentDetails.getShipmentRating(), packagingInfo);
		getModelService().save(packagingInfo);
		getModelService().refresh(packagingInfo);
		createShippingLabelHistory(labelTypeEnum, packagingInfo, trackingNumber, StringUtils.EMPTY, StringUtils.EMPTY, null, null, null);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Tracking Id {} generated for package {}",
				completedShipmentDetails.getMasterTrackingId().getTrackingNumber(), packagingInfo.getPackageId());
	}

	/**
	 * This method will be used to set total charges on package
	 *
	 * @param shipmentRating
	 * @param packagingInfo
	 */
	private void setTotalChargesOnPackage(final ShipmentRating shipmentRating, final PackagingInfoModel packagingInfo)
	{
		if (shipmentRating != null)
		{
			final ShipmentRateDetail[] shipmentRateDetail = shipmentRating.getShipmentRateDetails();
			for (int j = 0; j < shipmentRateDetail.length; j++)
			{
				packagingInfo
						.setTotalShippingPrice(Double.valueOf(shipmentRateDetail[j].getTotalBaseCharge().getAmount().toString()));
			}
		}
	}

	/**
	 * method will be used to save the UPS Inbound shipment response on package
	 *
	 * @param upsResponse
	 * @param packagingInfo
	 * @throws IOException 
	 */
	private void saveResponseOnInBoundPackage(final UPSShipmentCreateResponse upsResponse, final PackagingInfoModel packagingInfo) throws IOException
	{
		final StringBuilder buffer = new StringBuilder();
		final UPSShipmentPackageResult shipmentPackage = saveResponseOnPackage(upsResponse, packagingInfo, buffer);
		packagingInfo.setInBoundShippingLabel(buffer.toString());
		packagingInfo.setInBoundTrackingNumber(shipmentPackage.getTrackingNumber());
		packagingInfo.setInBoundGraphicImage(shipmentPackage.getGraphicImage());
		final CatalogUnawareMediaModel createCatalogUnawareMediaModel = getBlShipmentCreationService().createCatalogUnawareMediaModel(buffer.toString(),
				shipmentPackage.getTrackingNumber(), BlintegrationConstants.INBOUND_PACKAGE);
		if (packagingInfo.getInBoundShippingMedia() != null)
		{
			getModelService().remove(packagingInfo.getInBoundShippingMedia());
		}
		packagingInfo.setInBoundShippingMedia(createCatalogUnawareMediaModel);
		getModelService().save(packagingInfo);
		getModelService().refresh(packagingInfo);
		createShippingLabelHistory(ShippingLabelTypeEnum.INBOUND, packagingInfo, shipmentPackage.getTrackingNumber(),
				shipmentPackage.getGraphicImage(), buffer.toString(),createCatalogUnawareMediaModel, null, null);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Inbound Shipment generated for Package with tracking number : {}",
				packagingInfo.getInBoundTrackingNumber());
	}

	/**
	 * method will be used to save the UPS Outbound shipment response on package
	 *
	 * @param upsResponse
	 * @param packagingInfo
	 * @throws IOException 
	 */
	private void saveResponseOnOutboundPackage(final UPSShipmentCreateResponse upsResponse, final PackagingInfoModel packagingInfo, final CarrierEnum shippingType,
			final OptimizedShippingMethodModel optimizedShippingMethod) throws IOException
	{
		final StringBuilder buffer = new StringBuilder();
		final UPSShipmentPackageResult shipmentPackage = saveResponseOnPackage(upsResponse, packagingInfo, buffer);
		packagingInfo.setLabelURL(upsShipmentURL + shipmentPackage.getTrackingNumber());
		packagingInfo.setOutBoundShippingLabel(buffer.toString());
		packagingInfo.setOutBoundTrackingNumber(shipmentPackage.getTrackingNumber());
		packagingInfo.setOutBoundGraphicImage(shipmentPackage.getGraphicImage());
		final CatalogUnawareMediaModel createCatalogUnawareMediaModel = getBlShipmentCreationService().createCatalogUnawareMediaModel(buffer.toString(),
				shipmentPackage.getTrackingNumber(), BlintegrationConstants.OUTBOUND_PACKAGE);
		if (packagingInfo.getOutBoundShippingMedia() != null)
		{
			getModelService().remove(packagingInfo.getOutBoundShippingMedia());
		}
		packagingInfo.setOutBoundShippingMedia(createCatalogUnawareMediaModel);
		getModelService().save(packagingInfo);
		getModelService().refresh(packagingInfo);
		createShippingLabelHistory(ShippingLabelTypeEnum.OUTBOUND, packagingInfo, shipmentPackage.getTrackingNumber(),
				shipmentPackage.getGraphicImage(), buffer.toString(),createCatalogUnawareMediaModel, shippingType, optimizedShippingMethod);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Outbound Shipment generated for Package with tracking number : {}",
				packagingInfo.getOutBoundTrackingNumber());
	}

	/**
	 * method will be used to save shipment response on package
	 *
	 * @param upsResponse
	 * @param packagingInfo
	 * @return
	 */
	private UPSShipmentPackageResult saveResponseOnPackage(final UPSShipmentCreateResponse upsResponse,
			final PackagingInfoModel packagingInfo, final StringBuilder buffer)
	{
		final UPSShipmentPackageResult shipmentPackage = upsResponse.getPackages().get(0);

		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Shipment generated for package {} with tracking id {}", packagingInfo,
				shipmentPackage.getTrackingNumber());
		packagingInfo.setShipmentIdentificationNumber(upsResponse.getShipmentIdentificationNumber());
		packagingInfo.setTotalShippingPrice(upsResponse.getTotalCharges());
		packagingInfo.setHTMLImage(shipmentPackage.getHTMLImage());
		try
		{
			convertImage(shipmentPackage.getGraphicImage(), packagingInfo, buffer);
		}
		catch (final Exception exception)
		{
			BlLogger.logMessage(LOG, Level.ERROR, "Exception occurred when converting the graphic image for the package {} ",
					packagingInfo.getPk().toString(), exception);
		}
		return shipmentPackage;
	}

	/**
	 * It converts the graphic image to ZPL data
	 *
	 * @param graphicImage
	 *           the graphic image
	 * @param packagingInfo
	 *           the packaging info
	 */
	private void convertImage(final String graphicImage, final PackagingInfoModel packagingInfo, final StringBuilder buffer)
	{
		final byte[] image = Base64.decodeBase64(graphicImage);
		final InputStream is = new ByteArrayInputStream(image);
		String st = null;
		try
		{
			final BufferedReader br = new BufferedReader(new InputStreamReader(is));
			while ((st = br.readLine()) != null)
			{
				buffer.append(st);
			}
		}
		catch (final IOException e)
		{
			BlLogger.logMessage(LOG, Level.ERROR, "Exception occurred when converting the graphic image for the package {} ",
					packagingInfo.getPk().toString(), e);
		}
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

	public ModelService getModelService()
	{
		return modelService;
	}

	public void setModelService(final ModelService modelService)
	{
		this.modelService = modelService;
	}

	private void createShippingLabelHistory(final ShippingLabelTypeEnum labelTypeEnum, final PackagingInfoModel packaging,
			final String trackingNumber, final String graphicLabel, final String shippingLabel,
			final CatalogUnawareMediaModel shippingLabelMedia, final CarrierEnum shippingType,
			final OptimizedShippingMethodModel optimizedShippingMethodModel)
	{
		final ShippingLabelHistoryModel shippingLabelHistory = getModelService().create(ShippingLabelHistoryModel.class);
		shippingLabelHistory.setShippingLabelType(labelTypeEnum);
		shippingLabelHistory.setTrackingNumber(trackingNumber);
		shippingLabelHistory.setGraphicImage(graphicLabel);
		shippingLabelHistory.setShippingLabel(shippingLabel);
		shippingLabelHistory.setShippingLabelMedia(shippingLabelMedia);
		shippingLabelHistory
				.setOrderCode(Objects.nonNull(packaging.getConsignment()) && Objects.nonNull(packaging.getConsignment().getOrder())
						? packaging.getConsignment().getOrder().getCode()
						: StringUtils.EMPTY);
		shippingLabelHistory.setConsignmentCode(
				Objects.nonNull(packaging.getConsignment()) ? packaging.getConsignment().getCode() : StringUtils.EMPTY);
		shippingLabelHistory.setPackage(packaging);
		final boolean isOptimizedShippingMethodModified = Objects.nonNull(shippingType)
				&& Objects.nonNull(optimizedShippingMethodModel);
		shippingLabelHistory.setCarrier(isOptimizedShippingMethodModified ? shippingType
				: ((ZoneDeliveryModeModel) packaging.getConsignment().getDeliveryMode()).getCarrier());
		shippingLabelHistory.setOptimizedShippingMethod(isOptimizedShippingMethodModified ? optimizedShippingMethodModel
				: packaging.getConsignment().getOptimizedShippingType());
		shippingLabelHistory.setIsOptimizedShippingMethodModified(isOptimizedShippingMethodModified);
		getModelService().save(shippingLabelHistory);
		final List<ShippingLabelHistoryModel> shippingLabelHistoryModelList = Lists
				.newArrayList(CollectionUtils.emptyIfNull(packaging.getShippingLabelHistoryLogs()));
		shippingLabelHistoryModelList.add(shippingLabelHistory);
		packaging.setShippingLabelHistoryLogs(shippingLabelHistoryModelList);
		getModelService().save(packaging);
		getModelService().refresh(packaging);
	}

}
