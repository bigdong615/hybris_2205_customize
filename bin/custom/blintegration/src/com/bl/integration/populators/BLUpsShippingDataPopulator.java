/**
 *
 */
package com.bl.integration.populators;

import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.commercefacades.user.data.CountryData;
import de.hybris.platform.commercefacades.user.data.RegionData;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.util.Config;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import javax.annotation.Resource;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;

import com.bl.core.model.OptimizedShippingMethodModel;
import com.bl.facades.shipment.data.DimensionsTypeData;
import com.bl.facades.shipment.data.PackageTypeData;
import com.bl.facades.shipment.data.PackageWeightTypeData;
import com.bl.facades.shipment.data.ReturnServiceData;
import com.bl.facades.shipment.data.ShipFromData;
import com.bl.facades.shipment.data.ShipToData;
import com.bl.facades.shipment.data.ShipmentData;
import com.bl.facades.shipment.data.ShipmentPhoneData;
import com.bl.facades.shipment.data.ShipperData;
import com.bl.facades.shipment.data.UpsPaymentInformation;
import com.bl.facades.shipment.data.UpsShipmentServiceData;
import com.bl.facades.shipment.data.UpsShippingRequestData;
import com.bl.integration.constants.BlintegrationConstants;


/**
 * This class is responsible to populate shipment data
 *
 * @author Aditi Sharma
 *
 */
public class BLUpsShippingDataPopulator
{
	@Value("${blintegration.ups.shipment.shipper.number.ca}")
	private String shipperNumberCA;

	@Value("${blintegration.ups.shipment.shipper.number.ma}")
	private String shipperNumberMA;

	@Value("${blintegration.ups.shipment.shipper.phone.number}")
	private String shipperPhoneNumber;

	@Value("${blintegration.ups.shipment.shipper.name}")
	private String shipperName;

	@Value("${blintegration.ups.shipment.shipper.attension.name}")
	private String shipperAttensionName;

	@Value("${blintegration.ups.shipment.shipper.company.name}")
	private String shipperCompanyName;

	@Value("${blintegration.ups.shipment.payment.information.type}")
	private String paymentInfoType;

	@Value("${blintegration.ups.shipment.packaging.weight.code}")
	private String packageWeightCode;

	@Value("${blintegration.ups.shipment.packaging.weight.description}")
	private String packageWeightDescription;

	@Value("${blintegration.ups.shipment.dimension.type.code}")
	private String dimensionTypeCode;

	@Value("${blintegration.ups.shipment.dimension.type.description}")
	private String dimensionTypeDescription;

	@Value("${blintegration.ups.shipment.return.service.code}")
	private String returnServiceCode;

	@Value("${blintegration.ups.shipment.return.service.description}")
	private String returnServiceDescription;

	@Resource(name = "addressConverter")
	private Converter<AddressModel, AddressData> addressConverter;

	/**
	 * method will be used to create shipment request for UPS
	 *
	 * @param packagingInfo
	 * @return
	 */
	public UpsShippingRequestData populateUPSShipmentRequest(final PackagingInfoModel packagingInfo, final OptimizedShippingMethodModel om, final boolean isSignatureRequired)
	{
		final UpsShippingRequestData upsRequestData = new UpsShippingRequestData();
		final ShipmentData shipmentData = new ShipmentData();

		final ShipmentData upsShipmentData = populateUpsShipmentRequestData(packagingInfo, shipmentData, null, false, om, isSignatureRequired);
		upsRequestData.setShipment(upsShipmentData);
		return upsRequestData;

	}

	/**
	 * method will be used to create return shipment request for UPS
	 *
	 * @param packagingInfo
	 * @return
	 */
	public UpsShippingRequestData populateUPSReturnShipmentRequest(final PackagingInfoModel packagingInfo,
			final WarehouseModel warehouseModel)
	{
		final UpsShippingRequestData upsReturnRequestData = new UpsShippingRequestData();
		final ShipmentData shipmentData = new ShipmentData();

		final ShipmentData upsReturnShipmentData = populateUpsShipmentRequestData(packagingInfo, shipmentData, warehouseModel, true, null, false);

		/** Creating return service Data **/

		final ReturnServiceData returnServiceData = new ReturnServiceData();
		returnServiceData.setCode(returnServiceCode);
		returnServiceData.setDescription(returnServiceDescription);
		upsReturnShipmentData.setReturnService(returnServiceData);
		upsReturnRequestData.setShipment(shipmentData);

		return upsReturnRequestData;

	}

	/**
	 * method will be used to create shipment request for UPS
	 *
	 * @param packagingInfo
	 * @param upsRequestData
	 * @param shipmentData
	 */
	private ShipmentData populateUpsShipmentRequestData(final PackagingInfoModel packagingInfo, final ShipmentData shipmentData,
			final WarehouseModel stateWarehouse, final boolean isRSLabel, final OptimizedShippingMethodModel om,final boolean isSignatureRequired)
	{
		/** Creating UPS Payment Data **/
		final UpsPaymentInformation upsPaymentInformation = new UpsPaymentInformation();

		final WarehouseModel warehouse = packagingInfo.getConsignment().getWarehouse();
		final ShipperData shipperData = new ShipperData();
		AddressModel shipperAddress = new AddressModel();

		/** Creating Shipper Data **/

		if (Objects.isNull(stateWarehouse) && Objects.nonNull(warehouse) && Objects.nonNull(warehouse.getPointsOfService()))
		{
			shipperAddress = getWarehouseAddress(warehouse);
			shipperData.setShipperNumber(warehouse.getAccountNumber());
			populatePaymentServiceData(warehouse, upsPaymentInformation);
		}
		else if (Objects.nonNull(stateWarehouse) && Objects.nonNull(stateWarehouse.getPointsOfService()))
		{
			shipperAddress = getWarehouseAddress(stateWarehouse);
			shipperData.setShipperNumber(stateWarehouse.getAccountNumber());
			populatePaymentServiceData(stateWarehouse, upsPaymentInformation);
		}
		final AddressModel packageAddress = packagingInfo.getConsignment().getOrder().getDeliveryAddress();
		final AddressData addressData = addressConverter.convert(isRSLabel ? packageAddress : shipperAddress);



		final ShipmentPhoneData shipperPhone = new ShipmentPhoneData();
		shipperPhone.setNumber(trimmedPhoneNumber(addressData.getPhone()));

		final AddressData shipperAddressData = new AddressData();
		populateShipperAddressData(addressData, shipperAddressData, isRSLabel);
		if (isRSLabel)
		{
			shipperData.setName(trimNameOnRequest(
					new AtomicReference<>(StringUtils.isNotBlank(addressData.getCompanyName()) ? addressData.getCompanyName()
							: getEmptyIfNullOrBlank(addressData.getFirstName()).concat(StringUtils.SPACE)
									.concat(getEmptyIfNullOrBlank(addressData.getLastName())))));
			shipperData.setAttentionName(trimNameOnRequest(new AtomicReference<>(getEmptyIfNullOrBlank(addressData.getFirstName())
					.concat(StringUtils.SPACE).concat(getEmptyIfNullOrBlank(addressData.getLastName())))));
		}
		else
		{
			shipperData.setName(shipperName);
			shipperData.setAttentionName(shipperAttensionName);
		}
		shipperData.setPhone(shipperPhone);
		shipperData.setAddress(shipperAddressData);

		/** Creating ShipFrom Data **/
		final ShipFromData shipFromData = new ShipFromData();
		populateShipFromData(shipperPhone, shipperAddressData, shipFromData, isRSLabel);

		/** Creating ShipTo Data **/
		final AddressData shipToAddress = addressConverter.convert(isRSLabel ? shipperAddress : packageAddress);

		final ShipToData shipToData = populateShipToData(shipToAddress, isRSLabel);

		/** Creating UPS Shipment Service Data **/
		final UpsShipmentServiceData upsShipmentServiceData = new UpsShipmentServiceData();

		populateUpsShipmentServiceData(packagingInfo, upsShipmentServiceData, om, isRSLabel, isSignatureRequired);

		/** Creating UPS Package Data List **/
		final List<PackageTypeData> packageDataList = new ArrayList<>();
		populatePackage(packageDataList, packagingInfo);

		shipmentData.setShipper(shipperData);
		shipmentData.setShipFrom(shipFromData);
		shipmentData.setShipTo(shipToData);
		shipmentData.setService(upsShipmentServiceData);
		shipmentData.setPaymentInformation(upsPaymentInformation);
		shipmentData.setShipmentPackage(packageDataList);

		return shipmentData;
	}

	/**
	 * method will be used to populate shipper address data
	 *
	 * @param warehouse
	 * @param addressData
	 * @param shipperData
	 * @param shipperAddressData
	 */
	private void populateShipperAddressData(final AddressData addressData, final AddressData shipperAddressData, final boolean isRSLabel)
	{
		if(isRSLabel)
		{
			shipperAddressData.setFirstName(addressData.getFirstName());
			shipperAddressData.setLastName(addressData.getLastName());
			shipperAddressData.setCompanyName(addressData.getCompanyName());
		}
		shipperAddressData.setLine1(addressData.getLine1());
		shipperAddressData.setLine2(addressData.getLine2());
		shipperAddressData.setTown(addressData.getTown());
		shipperAddressData.setPhone(trimmedPhoneNumber(addressData.getPhone()));

		if (Objects.nonNull(addressData.getCountry()) && Objects.nonNull(addressData.getCountry().getIsocode()))
		{
			final CountryData countryData = new CountryData();
			countryData.setIsocode(addressData.getCountry().getIsocode());
			shipperAddressData.setCountry(countryData);
		}

		shipperAddressData.setPostalCode(addressData.getPostalCode());

		final RegionData regionData = new RegionData();
		regionData.setIsocodeShort(addressData.getRegion().getIsocodeShort());
		shipperAddressData.setRegion(regionData);
	}

	/**
	 * method will be used to populate payment service data
	 *
	 * @param warehouse
	 * @param upsPaymentInformation
	 */
	private void populatePaymentServiceData(final WarehouseModel warehouse, final UpsPaymentInformation upsPaymentInformation)
	{
		upsPaymentInformation.setType(paymentInfoType);

		if (Objects.nonNull(warehouse) && Objects.nonNull(warehouse.getAccountNumber()))
		{
			upsPaymentInformation.setAccountNumber(warehouse.getAccountNumber());
		}
	}

	/**
	 * method will be used to populate ups shipment service data
	 *
	 * @param packagingInfo
	 * @param upsShipmentServiceData
	 */
	private void populateUpsShipmentServiceData(final PackagingInfoModel packagingInfo,
			final UpsShipmentServiceData upsShipmentServiceData, final OptimizedShippingMethodModel om, final boolean isRSLabel, final boolean isSignatureRequired)
	{
		if(isRSLabel){
			upsShipmentServiceData.setCode(BlintegrationConstants.RETURN_LABEL_CODE);
			upsShipmentServiceData.setDescription(BlintegrationConstants.RETURN_LABEL_DESC);
		}
		else if(Objects.nonNull(om) && StringUtils.isNotBlank(om.getServiceTypeCode()) && StringUtils.isNotBlank(om.getServiceTypeDesc()))
		{
			if(isSignatureRequired && om.getSignatureServiceTypeCode()!=null) {
				upsShipmentServiceData.setCode(om.getSignatureServiceTypeCode());
			}
			else {
				upsShipmentServiceData.setCode(om.getServiceTypeCode());
			}
			upsShipmentServiceData.setDescription(om.getServiceTypeDesc());
		}
		else if (Objects.nonNull(packagingInfo.getConsignment().getOptimizedShippingType())
				&& StringUtils.isNotBlank(packagingInfo.getConsignment().getOptimizedShippingType().getCode()))
		{
			upsShipmentServiceData.setCode(packagingInfo.getConsignment().getOptimizedShippingType().getServiceTypeCode());
			upsShipmentServiceData.setDescription(packagingInfo.getConsignment().getOptimizedShippingType().getServiceTypeDesc());
		}
	}

	/**
	 * method will be used to populate ship from data
	 *
	 * @param shipperPhone
	 * @param shipperAddressData
	 * @param shipFromData
	 */
	private void populateShipFromData(final ShipmentPhoneData shipperPhone, final AddressData shipperAddressData,
			final ShipFromData shipFromData, final boolean isRSLabel)
	{
		if (isRSLabel)
		{
			shipFromData.setName(trimNameOnRequest(new AtomicReference<>(
					StringUtils.isNotBlank(shipperAddressData.getCompanyName()) ? shipperAddressData.getCompanyName()
							: getEmptyIfNullOrBlank(shipperAddressData.getFirstName()).concat(StringUtils.SPACE)
									.concat(getEmptyIfNullOrBlank(shipperAddressData.getLastName())))));
			shipFromData
					.setAttentionName(trimNameOnRequest(new AtomicReference<>(getEmptyIfNullOrBlank(shipperAddressData.getFirstName())
							.concat(StringUtils.SPACE).concat(getEmptyIfNullOrBlank(shipperAddressData.getLastName())))));
		}
		else
		{
			shipFromData.setName(shipperName);
			shipFromData.setAttentionName(shipperAttensionName);
		}
		shipFromData.setPhone(shipperPhone);
		shipFromData.setAddress(shipperAddressData);
	}

	/**
	 * method will be used to populate ship to data
	 *
	 * @param shipToAddress
	 * @return
	 */
	private ShipToData populateShipToData(final AddressData shipToAddress, final boolean isRSLabel)
	{
		final AddressData shipToAddressData = new AddressData();
		if (StringUtils.isNotEmpty(shipToAddress.getFirstName()))
		{
			shipToAddressData.setFirstName(shipToAddress.getFirstName());
		}
		shipToAddressData.setLastName(shipToAddress.getLastName());
		shipToAddressData.setCompanyName(shipToAddress.getCompanyName());
		shipToAddressData.setLine1(shipToAddress.getLine1());
		shipToAddressData.setLine2(shipToAddress.getLine2());
		shipToAddressData.setTown(shipToAddress.getTown());
		shipToAddressData.setPhone(trimmedPhoneNumber(shipToAddress.getPhone()));

		if (Objects.nonNull(shipToAddress.getRegion()) && Objects.nonNull(shipToAddress.getRegion().getIsocodeShort()))
		{
			final RegionData shipToRegionData = new RegionData();
			shipToRegionData.setIsocodeShort(shipToAddress.getRegion().getIsocodeShort());
			shipToAddressData.setRegion(shipToRegionData);
		}

		if (Objects.nonNull(shipToAddress.getCountry()) && Objects.nonNull(shipToAddress.getCountry().getIsocode()))
		{
			final CountryData shipToCountryData = new CountryData();
			shipToCountryData.setIsocode(shipToAddress.getCountry().getIsocode());
			shipToAddressData.setCountry(shipToCountryData);
		}
		shipToAddressData.setPostalCode(shipToAddress.getPostalCode());

		final ShipmentPhoneData shipToPhone = new ShipmentPhoneData();
		shipToPhone.setNumber(trimmedPhoneNumber(shipToAddress.getPhone()));

		final ShipToData shipToData = new ShipToData();
		if (StringUtils.isNotBlank(shipToAddressData.getCompanyName()))
		{
			shipToData.setName(trimNameOnRequest(new AtomicReference<>(shipToAddressData.getCompanyName())));
		}
		else
		{
			shipToData.setName(trimNameOnRequest(new AtomicReference<>(getEmptyIfNullOrBlank(shipToAddressData.getFirstName()).concat(StringUtils.SPACE).concat(getEmptyIfNullOrBlank(shipToAddressData.getLastName())))));
		}
		shipToData
				.setAttentionName(isRSLabel ? shipperAttensionName : trimNameOnRequest(new AtomicReference<>(getEmptyIfNullOrBlank(shipToAddressData.getFirstName()).concat(StringUtils.SPACE).concat(getEmptyIfNullOrBlank(shipToAddressData.getLastName())))));
		shipToData.setPhone(shipToPhone);
		shipToData.setAddress(shipToAddressData);
		return shipToData;
	}

	private String getEmptyIfNullOrBlank(final String value)
	{
		return StringUtils.defaultIfBlank(value, StringUtils.EMPTY);
	}


	/**
	 * This method created to trim the name if its greater than 35
	 * @param  name<String> name
	 */
	private String trimNameOnRequest(final AtomicReference<String> name) {
		final AtomicInteger maxCharacter = new AtomicInteger(0);
		if(Objects.nonNull(Config.getInt(BlintegrationConstants.NAME_MAX_CHARACTER, 35))) {
			maxCharacter.set(Config.getInt((BlintegrationConstants.NAME_MAX_CHARACTER), 35));
		}
		if(StringUtils.isNotBlank(name.get()) && name.get().length() > maxCharacter.get()){
			name.set(name.get().substring(0 , maxCharacter.get()));
		}
		return name.get();
	}

	/**
	 * method will be used to populate package data
	 *
	 * @param packageDataList
	 * @param consignmentEntries
	 */
	private void populatePackage(final List<PackageTypeData> packageDataList, final PackagingInfoModel packagingInfo)
	{
		final UpsShipmentServiceData serviceDataForPackagingType = new UpsShipmentServiceData();

		serviceDataForPackagingType.setCode(BlintegrationConstants.PACAKAGING_TYPE_CODE);
		serviceDataForPackagingType.setDescription(BlintegrationConstants.PACAKAGING_TYPE_DESCIPTION);

		final UpsShipmentServiceData serviceDataForUnit = new UpsShipmentServiceData();
		serviceDataForUnit.setCode(packageWeightCode);
		serviceDataForUnit.setDescription(packageWeightDescription);

		final PackageTypeData packageData = new PackageTypeData();
		final DimensionsTypeData dimensionsTypeData = new DimensionsTypeData();
		final PackageWeightTypeData packageWeight = new PackageWeightTypeData();
		packageWeight.setUnitOfMeasurement(serviceDataForUnit);
		packageWeight.setWeight(packagingInfo.getGrossWeight());
		packageData.setPackagingType(serviceDataForPackagingType);
		packageData.setPackageWeight(packageWeight);

		final UpsShipmentServiceData unitOfMeas = new UpsShipmentServiceData();
		unitOfMeas.setCode(dimensionTypeCode);
		unitOfMeas.setDescription(dimensionTypeDescription);

		dimensionsTypeData.setHeight(packagingInfo.getHeight());
		dimensionsTypeData.setLength(packagingInfo.getLength());
		dimensionsTypeData.setWidth(packagingInfo.getWidth());
		dimensionsTypeData.setUnitOfMeasurement(unitOfMeas);
		packageData.setDimensions(dimensionsTypeData);
		packageDataList.add(packageData);
	}

	/**
	 * this method is used to get warehouse address
	 *
	 * @param warehouse
	 * @return AddressModel
	 */
	private AddressModel getWarehouseAddress(final WarehouseModel warehouse)
	{
		return warehouse.getPointsOfService().iterator().next().getAddress();
	}

	private String trimmedPhoneNumber(final String phoneNumber)
	{
		if(StringUtils.isNotBlank(phoneNumber) && phoneNumber.length() > 15)
		{
			return phoneNumber.replace(BlintegrationConstants.WHITE_SPACE, StringUtils.EMPTY);
		}
		return phoneNumber;
	}
}
