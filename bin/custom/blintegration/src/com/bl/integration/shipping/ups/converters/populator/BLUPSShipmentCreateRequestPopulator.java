/**
 *
 */
package com.bl.integration.shipping.ups.converters.populator;

import com.ups.xmlschema.xoltws.ship.v1.*;
import com.ups.xmlschema.xoltws.ship.v1.ShipmentType.ShipmentServiceOptions;

import de.hybris.platform.commercefacades.user.data.AddressData;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Value;

import com.bl.facades.shipment.data.PackageTypeData;
import com.bl.facades.shipment.data.ReturnServiceData;
import com.bl.facades.shipment.data.ShipFromData;
import com.bl.facades.shipment.data.ShipToData;
import com.bl.facades.shipment.data.ShipmentData;
import com.bl.facades.shipment.data.ShipperData;
import com.bl.facades.shipment.data.UpsPaymentInformation;
import com.bl.facades.shipment.data.UpsShipmentServiceData;
import com.bl.facades.shipment.data.UpsShippingRequestData;
import com.bl.integration.constants.BlintegrationConstants;
import com.ups.xmlschema.xoltws.common.v1.RequestType;
import com.ups.xmlschema.xoltws.common.v1.TransactionReferenceType;


/**
 * this class is responsible to populate request data for UPS
 * @author Aditi Sharma
 *
 */
public class BLUPSShipmentCreateRequestPopulator
{
	@Value("${blintegration.ups.shipment.labelStockSize.type.height}")
	private String labelStockHeight;

	@Value("${blintegration.ups.shipment.labelStockSize.type.width}")
	private String labelStockWidth;

	@Value("${blintegration.ups.shipment.labelSpec.code}")
	private String labelSpecCode;

	/**
	 * method will be used to convert UPS shipment request
	 * @param upsShipmentRequest
	 * @return
	 */
	public ShipmentRequest convertToUPSShipmentRequest(final UpsShippingRequestData upsShipmentRequest, final String referenceNumber)
	{
		final ShipmentRequest shipmentRequest = new ShipmentRequest();
		final RequestType requestType = new RequestType();

		final TransactionReferenceType transactionReference = new TransactionReferenceType();
		transactionReference.setCustomerContext(BlintegrationConstants.CUSTOMER_CONTEXT);

		requestType.setTransactionReference(transactionReference);

		final List<String> requestOption = requestType.getRequestOption();
		requestOption.add(BlintegrationConstants.REQUEST_OPTION);

		shipmentRequest.setRequest(requestType);

		final ShipmentType shipmentType = new ShipmentType();

		/** Creating Shipper **/

		final ShipmentData shipmentData = upsShipmentRequest.getShipment();
		final ShipperType shipperType = new ShipperType();

		populateShipperData(shipperType, shipmentData.getShipper());
		shipmentType.setShipper(shipperType);

		/** Creating ShipTo **/

		final ShipToType shipToType = new ShipToType();
		populateShipToData(shipToType, shipmentData.getShipTo());
		shipmentType.setShipTo(shipToType);

		/** Creating ShipFrom **/

		final ShipFromType shipFromType = new ShipFromType();
		populateShipFromData(shipFromType, shipmentData.getShipFrom());
		shipmentType.setShipFrom(shipFromType);

		/** Creating PaymentInformation **/

		final PaymentInfoType paymentInfoType = new PaymentInfoType();
		populatePaymentInfo(paymentInfoType, shipmentData.getPaymentInformation());
		shipmentType.setPaymentInformation(paymentInfoType);

		/** Creating Service **/

		final ServiceType serviceType = new ServiceType();
		populateServiceType(serviceType, shipmentData.getService());
		shipmentType.setService(serviceType);

		/** Creating Retrun Service **/
		final ReturnServiceType returnServiceType = new ReturnServiceType();
		final ReturnServiceData returnServiceData = shipmentData.getReturnService();
		if (returnServiceData != null)
		{
			returnServiceType.setCode(returnServiceData.getCode());
			returnServiceType.setDescription(returnServiceData.getDescription());
			shipmentType.setReturnService(returnServiceType);
		}

		/** Creating Package **/

		final List<PackageType> packageList = shipmentType.getPackage();
		final PackageTypeData packageType = shipmentData.getShipmentPackage().get(0);

		final PackagingType codeDescriptionForPackagingType = new PackagingType();
		codeDescriptionForPackagingType.setCode(packageType.getPackagingType().getCode());
		codeDescriptionForPackagingType.setDescription(packageType.getPackagingType().getDescription());

		final PackageType pkg1 = new PackageType();
		if(shipmentData.isSignatureRequired()) {
   		final DeliveryConfirmationType deliveryConfirmation = new DeliveryConfirmationType();
   		deliveryConfirmation.setDCISType(BlintegrationConstants.DELIVERY_CONFIRMATION_SIGNATURE);
			final PackageServiceOptionsType packageServiceOptions = new PackageServiceOptionsType();
			packageServiceOptions.setDeliveryConfirmation(deliveryConfirmation);
			pkg1.setPackageServiceOptions(packageServiceOptions);
		}
		if(shipmentData.isHoldAtUpsStore()) {
			IndicationType indication = new IndicationType();
			indication.setCode("01");
			List<IndicationType> indications = new ArrayList<IndicationType>();
			indications.add(indication);
			shipmentType.getShipmentIndicationType().add(indication);
			AlternateDeliveryAddressType alternateDeliveryAddress =  new AlternateDeliveryAddressType();
			alternateDeliveryAddress.setAttentionName(shipperType.getAttentionName());
			alternateDeliveryAddress.setName(shipperType.getName());
			AddressData alternateAddress = shipmentData.getShipper().getPaymentAddress();
			//alternateDeliveryAddress.setUPSAccessPointID("GB00088");
			ADLAddressType address = new ADLAddressType();
			address.getAddressLine().add(alternateAddress.getLine1());
			address.setCity(alternateAddress.getTown());
			address.setStateProvinceCode(alternateAddress.getRegion().getIsocodeShort());
			address.setPostalCode(alternateAddress.getPostalCode());
			address.setCountryCode(alternateAddress.getCountry().getIsocode());
			alternateDeliveryAddress.setAddress(address);
			shipmentType.setAlternateDeliveryAddress(alternateDeliveryAddress);
		}

		final ShipUnitOfMeasurementType shipUnitOfMeasurementType = new ShipUnitOfMeasurementType();
		shipUnitOfMeasurementType.setCode(packageType.getDimensions().getUnitOfMeasurement().getCode());
		shipUnitOfMeasurementType.setDescription(packageType.getDimensions().getUnitOfMeasurement().getDescription());

		final DimensionsType dimensionType = new DimensionsType();
		dimensionType.setHeight(packageType.getDimensions().getHeight());
		dimensionType.setLength(packageType.getDimensions().getLength());
		dimensionType.setWidth(packageType.getDimensions().getWidth());
		dimensionType.setUnitOfMeasurement(shipUnitOfMeasurementType);

		pkg1.setPackaging(codeDescriptionForPackagingType);
		pkg1.setDimensions(dimensionType);

		final PackageWeightType pkgWeight = new PackageWeightType();

		final ShipUnitOfMeasurementType codeDescriptionForUnitOfMeasurement = new ShipUnitOfMeasurementType();
		codeDescriptionForUnitOfMeasurement.setCode(packageType.getPackageWeight().getUnitOfMeasurement().getCode());
		codeDescriptionForUnitOfMeasurement.setDescription(packageType.getPackageWeight().getUnitOfMeasurement().getDescription());

		pkgWeight.setUnitOfMeasurement(codeDescriptionForUnitOfMeasurement);
		pkgWeight.setWeight(packageType.getPackageWeight().getWeight());
		pkg1.setPackageWeight(pkgWeight);
		pkg1.setDescription(BlintegrationConstants.PACKAGE_DESCRIPTION);
		final List<ReferenceNumberType> lRefNum = new ArrayList<>();
		final ReferenceNumberType refNumType = new ReferenceNumberType();
		refNumType.setValue(referenceNumber);
		lRefNum.add(refNumType);
		pkg1.getReferenceNumber().addAll(lRefNum);
		packageList.add(pkg1);

		/** Creating LabelSpecification Data **/
		final LabelSpecificationType labelSpecType = new LabelSpecificationType();
		final LabelImageFormatType labelImageFormat = new LabelImageFormatType();
		final LabelStockSizeType labelStockSizeType = new LabelStockSizeType();
		labelStockSizeType.setHeight(labelStockHeight);
		labelStockSizeType.setWidth(labelStockWidth);
		labelImageFormat.setCode(labelSpecCode);
		labelImageFormat.setDescription(labelSpecCode);
		labelSpecType.setLabelImageFormat(labelImageFormat);
		labelSpecType.setHTTPUserAgent(BlintegrationConstants.LABEL_SPECIFICATION_HTTPUSERAGENT);
		labelSpecType.setLabelStockSize(labelStockSizeType);
		shipmentRequest.setLabelSpecification(labelSpecType);

		/** ***********Shipment Description********************* */

		shipmentType.setDescription(BlintegrationConstants.SHIPMENT_DESCRIPTION);

		shipmentRequest.setShipment(shipmentType);

		return shipmentRequest;

	}

	/**
	 * method will be used to populate shipper data
	 * @param shipper
	 * @param shipperData
	 */
	private void populateShipperData(final ShipperType shipper, final ShipperData shipperData)
	{
		shipper.setName(shipperData.getName());
		shipper.setShipperNumber(shipperData.getShipperNumber());

		shipper.setAttentionName(shipperData.getAttentionName());
		final AddressData shipperAddressData = shipperData.getAddress();
		final ShipAddressType shipperAddress = new ShipAddressType();
		final List<String> addressLineList = shipperAddress.getAddressLine();
		addressLineList.add(shipperAddressData.getLine1());
		addressLineList.add(shipperAddressData.getLine2());

		shipperAddress.setCity(shipperAddressData.getTown());
		shipperAddress.setPostalCode(shipperAddressData.getPostalCode());

		if (shipperAddressData.getRegion() != null && shipperAddressData.getRegion().getIsocodeShort() != null)
		{
			shipperAddress.setStateProvinceCode(shipperAddressData.getRegion().getIsocodeShort());
		}

		if (shipperAddressData.getCountry() != null && shipperAddressData.getCountry().getIsocode() != null)
		{
			shipperAddress.setCountryCode(shipperAddressData.getCountry().getIsocode());
		}
		shipper.setAddress(shipperAddress);

		final ShipPhoneType shipperPhone = new ShipPhoneType();
		shipperPhone.setNumber(shipperData.getPhone().getNumber());
		shipper.setPhone(shipperPhone);
	}

	/**
	 * method will be used to populate shipTo data
	 * @param shipTo
	 * @param shipToData
	 */
	private void populateShipToData(final ShipToType shipTo, final ShipToData shipToData)
	{
		shipTo.setName(shipToData.getName());
		shipTo.setAttentionName(shipToData.getAttentionName());
		final AddressData shipToAddressData = shipToData.getAddress();
		final ShipToAddressType shipToAddress = new ShipToAddressType();
		final List<String> addressLineList = shipToAddress.getAddressLine();
		addressLineList.add(shipToAddressData.getLine1());
		addressLineList.add(shipToAddressData.getLine2());

		shipToAddress.setCity(shipToAddressData.getTown());
		shipToAddress.setPostalCode(shipToAddressData.getPostalCode());

		if (shipToAddressData.getCountry() != null && shipToAddressData.getCountry().getIsocode() != null)
		{
			shipToAddress.setCountryCode(shipToAddressData.getCountry().getIsocode());
		}
		if (shipToAddressData.getRegion() != null && shipToAddressData.getRegion().getIsocodeShort() != null)
		{
			shipToAddress.setStateProvinceCode(shipToAddressData.getRegion().getIsocodeShort());
		}
		shipTo.setAddress(shipToAddress);

		final ShipPhoneType shipperToPhone = new ShipPhoneType();
		shipperToPhone.setNumber(shipToData.getPhone().getNumber());
		shipTo.setPhone(shipperToPhone);
	}

	/**
	 * method will be used to populate shipFrom data
	 * @param shipFrom
	 * @param shipFromData
	 */
	private void populateShipFromData(final ShipFromType shipFrom, final ShipFromData shipFromData)
	{
		shipFrom.setName(shipFromData.getName());
		shipFrom.setAttentionName(shipFromData.getAttentionName());
		final AddressData address = shipFromData.getAddress();
		final ShipAddressType shipFromAddress = new ShipAddressType();
		final List<String> addressLineListShipFrom = shipFromAddress.getAddressLine();
		addressLineListShipFrom.add(address.getLine1());
		addressLineListShipFrom.add(address.getLine2());

		shipFromAddress.setCity(address.getTown());
		shipFromAddress.setPostalCode(address.getPostalCode());

		if (address.getRegion() != null && address.getRegion().getIsocodeShort() != null)
		{
			shipFromAddress.setStateProvinceCode(address.getRegion().getIsocodeShort());
		}

		if (address.getCountry() != null && address.getCountry().getIsocode() != null)
		{
			shipFromAddress.setCountryCode(address.getCountry().getIsocode());
		}
		shipFrom.setAddress(shipFromAddress);

		final ShipPhoneType shipFromPhone = new ShipPhoneType();
		shipFromPhone.setNumber(shipFromData.getPhone().getNumber());
		shipFrom.setPhone(shipFromPhone);
	}

	/**
	 * method will be used to populate paymentInfo data
	 * @param upsPaymentInformation
	 *
	 */
	private void populatePaymentInfo(final PaymentInfoType paymentInfoType, final UpsPaymentInformation upsPaymentInformation)
	{
		final ShipmentChargeType shpmntCharge = new ShipmentChargeType();
		shpmntCharge.setType(upsPaymentInformation.getType());
		final BillShipperType billShipper = new BillShipperType();
		billShipper.setAccountNumber(upsPaymentInformation.getAccountNumber());
		shpmntCharge.setBillShipper(billShipper);
		final ShipmentChargeType[] shpmntChargeArray =
		{ shpmntCharge };
		final List<ShipmentChargeType> shipmentChargesList = paymentInfoType.getShipmentCharge();
		shipmentChargesList.add(shpmntChargeArray[0]);
	}

	/**
	 * method will be used to populate serviceType data
	 * @param serviceType
	 * @param serviceData
	 */
	private void populateServiceType(final ServiceType serviceType, final UpsShipmentServiceData serviceData)
	{
		serviceType.setCode(serviceData.getCode());
		serviceType.setDescription(serviceData.getDescription());
	}

}
