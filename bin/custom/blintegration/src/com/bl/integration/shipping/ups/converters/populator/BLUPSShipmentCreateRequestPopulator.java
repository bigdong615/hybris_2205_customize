/**
 *
 */
package com.bl.integration.shipping.ups.converters.populator;

import de.hybris.platform.commercefacades.user.data.AddressData;

import java.util.List;

import org.springframework.beans.factory.annotation.Value;

import com.bl.facades.shipment.data.PackageTypeData;
import com.bl.facades.shipment.data.ShipFromData;
import com.bl.facades.shipment.data.ShipToData;
import com.bl.facades.shipment.data.ShipmentData;
import com.bl.facades.shipment.data.ShipperData;
import com.bl.facades.shipment.data.UpsPaymentInformation;
import com.bl.facades.shipment.data.UpsShipmentServiceData;
import com.bl.facades.shipment.data.UpsShippingRequestData;
import com.bl.integration.constants.BlintegrationConstants;
import com.bl.integration.ups.ship.v1.pojo.BillShipperType;
import com.bl.integration.ups.ship.v1.pojo.DimensionsType;
import com.bl.integration.ups.ship.v1.pojo.LabelDeliveryType;
import com.bl.integration.ups.ship.v1.pojo.LabelImageFormatType;
import com.bl.integration.ups.ship.v1.pojo.LabelSpecificationType;
import com.bl.integration.ups.ship.v1.pojo.LabelStockSizeType;
import com.bl.integration.ups.ship.v1.pojo.PackageType;
import com.bl.integration.ups.ship.v1.pojo.PackageWeightType;
import com.bl.integration.ups.ship.v1.pojo.PackagingType;
import com.bl.integration.ups.ship.v1.pojo.PaymentInfoType;
import com.bl.integration.ups.ship.v1.pojo.ReturnServiceType;
import com.bl.integration.ups.ship.v1.pojo.ServiceType;
import com.bl.integration.ups.ship.v1.pojo.ShipAddressType;
import com.bl.integration.ups.ship.v1.pojo.ShipFromType;
import com.bl.integration.ups.ship.v1.pojo.ShipPhoneType;
import com.bl.integration.ups.ship.v1.pojo.ShipToAddressType;
import com.bl.integration.ups.ship.v1.pojo.ShipToType;
import com.bl.integration.ups.ship.v1.pojo.ShipUnitOfMeasurementType;
import com.bl.integration.ups.ship.v1.pojo.ShipmentChargeType;
import com.bl.integration.ups.ship.v1.pojo.ShipmentRequest;
import com.bl.integration.ups.ship.v1.pojo.ShipmentType;
import com.bl.integration.ups.ship.v1.pojo.ShipmentType.ShipmentServiceOptions;
import com.bl.integration.ups.ship.v1.pojo.ShipperType;
import com.bl.ups.common.v1.pojo.RequestType;
import com.bl.ups.common.v1.pojo.TransactionReferenceType;


/**
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
	 * @param upsShipmentRequest
	 * @return
	 */
	public ShipmentRequest convertToUPSShipmentRequest(final UpsShippingRequestData upsShipmentRequest)
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

		/** Creating Shipper Data **/

		final ShipmentData shipmentData = upsShipmentRequest.getShipment();
		final ShipperType shipperType = new ShipperType();

		populateShipperData(shipperType, shipmentData.getShipper());
		shipmentType.setShipper(shipperType);

		/** Creating ShipTo Data **/

		final ShipToType shipToType = new ShipToType();
		populateShipToData(shipToType, shipmentData.getShipTo());
		shipmentType.setShipTo(shipToType);

		/** Creating ShipFrom Data **/

		final ShipFromType shipFromType = new ShipFromType();
		populateShipFromData(shipFromType, shipmentData.getShipFrom());
		shipmentType.setShipFrom(shipFromType);

		/** Creating PaymentInformation Data **/

		final PaymentInfoType paymentInfoType = new PaymentInfoType();
		populatePaymentInfo(paymentInfoType, shipmentData.getPaymentInformation());
		shipmentType.setPaymentInformation(paymentInfoType);

		/** Creating Service Data **/

		final ServiceType serviceType = new ServiceType();
		populateServiceType(serviceType, shipmentData.getService());
		shipmentType.setService(serviceType);

		/** Creating Service Options Data **/
		final ShipmentServiceOptions shpServiceOptions = new ShipmentServiceOptions();

		final LabelDeliveryType labelDelivery = new LabelDeliveryType();
		labelDelivery.setLabelLinksIndicator(BlintegrationConstants.LABEL_INDICATOR);
		shpServiceOptions.setLabelDelivery(labelDelivery);
		shipmentType.setShipmentServiceOptions(shpServiceOptions);

		/** Creating Package Data **/

		final List<PackageType> packageList = shipmentType.getPackage();
		final PackageTypeData packageType = shipmentData.getShipmentPackage().get(0);

		final PackagingType codeDescriptionForPackagingType = new PackagingType();
		codeDescriptionForPackagingType.setCode(packageType.getPackagingType().getCode());
		codeDescriptionForPackagingType.setDescription(packageType.getPackagingType().getDescription());

		final PackageType pkg1 = new PackageType();

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
		packageList.add(pkg1);

		/** Creating Return Service Data **/

		final ReturnServiceType returnService = new ReturnServiceType();
		if (shipmentData.getReturnService() != null && shipmentData.getReturnService().getCode() != null)
		{
			returnService.setCode(shipmentData.getReturnService().getCode());
			returnService.setDescription(shipmentData.getReturnService().getDescription());
		}

		shipmentType.setReturnService(returnService);

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

		if (shipperAddressData.getRegion() != null && shipperAddressData.getRegion().getIsocode() != null)
		{
			shipperAddress.setStateProvinceCode(shipperAddressData.getRegion().getIsocode());
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
		if (shipToAddressData.getRegion() != null && shipToAddressData.getRegion().getIsocode() != null)
		{
			shipToAddress.setStateProvinceCode(shipToAddressData.getRegion().getIsocode());
		}
		shipTo.setAddress(shipToAddress);

		final ShipPhoneType shipperToPhone = new ShipPhoneType();
		shipperToPhone.setNumber(shipToData.getPhone().getNumber());
		shipTo.setPhone(shipperToPhone);
	}

	/**
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

		if (address.getRegion() != null && address.getRegion().getIsocode() != null)
		{
			shipFromAddress.setStateProvinceCode(address.getRegion().getIsocode());
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
	 * @param serviceType
	 * @param serviceData
	 */
	private void populateServiceType(final ServiceType serviceType, final UpsShipmentServiceData serviceData)
	{
		serviceType.setCode(serviceData.getCode());
		serviceType.setDescription(serviceData.getDescription());
	}

}
